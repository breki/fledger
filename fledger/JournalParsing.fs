module fledger.JournalParsing

open Journal

open System
open System.Globalization

open FParsec

let trimStringOptional (s: string option) =
    match s with
    | Some s -> s.Trim() |> Some
    | None -> None

let whitespace: Parser<string, unit> =
    manyChars (pchar ' ' <|> pchar '\t')
    <?> "whitespace"

let whitespace1: Parser<string, unit> =
    many1Chars (pchar ' ' <|> pchar '\t')
    <?> "whitespace 1"

let endOfLineWhitespace: Parser<unit, unit> =
    whitespace >>. newline >>% ()

let pDatePartSeparator: Parser<char, unit> =
    pchar '/' <|> pchar '-'

let pYear =
    pint32 .>> pDatePartSeparator <?> "year"

let pMonth = pint32 .>> pDatePartSeparator
let pDay = pint32

let pDate =
    pipe3 pYear pMonth pDay (fun year month day -> DateTime(year, month, day))

// status character = "!" | "*"
let pTxStatus: Parser<char, unit> =
    pchar '!' <|> pchar '*' <?> "transaction status"

// tx description = text - ";"
let pTxDescription: Parser<string, unit> =
    manyChars (noneOf ";")
    <?> "transaction description"

// tx comment = ";", [comment], end of line
let pTxComment =
    pstring ";" >>. restOfLine true
    <?> "transaction comment"

// tx first line = date, [whitespace1], [status character], [whitespace],
//                 [tx description], [whitespace], [tx comment]
let pTxFirstLine =
    pDate .>> whitespace1
    .>>. (opt pTxStatus
          |>> fun optChar ->
                  match optChar with
                  | Some '*' -> TransactionStatus.Cleared
                  | Some '!' -> TransactionStatus.Pending
                  | None -> TransactionStatus.Unmarked
                  | _ -> failwith "invalid transaction state")
    .>> whitespace
    .>>. (opt pTxDescription |>> trimStringOptional)
    .>> whitespace
    .>>. (opt pTxComment |>> trimStringOptional)
    |>> fun (((date, status), description), comment) ->
            { Date = date
              Status = status
              Description = description
              Comment = comment }

let pAccountChar =
    choice [ letter
             digit
             pchar ':'
             pchar '-'
             pchar '_'
             pchar ' ' ]
    <?> "account name character "

let pAmountSeparator =
    (pstring " " .>> (pstring " ") |> attempt
     <?> "amount separator")

let pAccount: Parser<string, unit> =
    many1CharsTill pAccountChar pAmountSeparator
    <?> "account name"

let pAccountRef =
    whitespace1 >>. pAccount .>> whitespace
    <?> "account reference"

let pAmountValue =
    numberLiteral NumberLiteralOptions.DefaultFloat "amount"
    |>> (fun num -> Decimal.Parse(num.String, CultureInfo.InvariantCulture))
    <?> "amount value"

let pCurrencyChar = letter

let pCurrency: Parser<string, unit> =
    many1Chars pCurrencyChar <?> "currency"

let pAmountCurrency =
    (whitespace1 >>. pCurrency) |> attempt
    <??> "amount currency"

let pAmount: Parser<Amount, unit> =
    pipe2 pAmountValue (opt pAmountCurrency) (fun amount currency ->
        match currency with
        | Some currency -> { Value = amount; Currency = currency }
        | None -> { Value = amount; Currency = "EUR" })
    <??> "amount"

let pTotalPriceIndicator =
    pstring "@@" >>% ()
    <?> "total price indicator (@@)"

let pTotalPrice =
    (whitespace1 >>? pTotalPriceIndicator
     .>>? whitespace1
     >>. pAmount)
    |> attempt
    <??> "total price"

let pExpectedBalance =
    (pchar ' ' >>. whitespace1 >>? (pstring "=")
     .>> whitespace1
     >>. pAmount)
    |> attempt
    <??> "expected balance"

let pPostingLine =
    pipe5
        pAccountRef
        pAmount
        (opt pTotalPrice)
        (opt pExpectedBalance)
        endOfLineWhitespace
        (fun account amount totalPrice expectedBalance _ ->
            { Account = account
              Amount = amount
              TotalPrice = totalPrice
              ExpectedBalance = expectedBalance })
    <??> "posting line"

let pPostingLines: Parser<PostingLine list, unit> =
    many pPostingLine

let pTx =
    pTxFirstLine .>>. pPostingLines
    |>> (fun (info, postings) -> { Info = info; Postings = postings })
