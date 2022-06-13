module fledger.JournalParsing

open Journal

open System
open System.Globalization

open FParsec

let spacesTabs: Parser<string, unit> =
    manyChars (pchar ' ' <|> pchar '\t')
    <?> "space or tab"

let spacesTabs1: Parser<string, unit> =
    many1Chars (pchar ' ' <|> pchar '\t')
    <?> "space or tab"

let endOfLineWhitespace: Parser<unit, unit> = spacesTabs >>. newline >>% ()

let pDatePartSeparator: Parser<char, unit> = pchar '/' <|> pchar '-'
let pYear = pint32 .>> pDatePartSeparator <?> "year"
let pMonth = pint32 .>> pDatePartSeparator
let pDay = pint32

let pDate =
    pipe3 pYear pMonth pDay (fun year month day -> DateTime(year, month, day))

let pPendingTxDesc: Parser<TransactionState * string, unit> =
    pipe3
        (pstring "!")
        spacesTabs
        (restOfLine true)
        (fun _ _ desc -> (TransactionState.Pending, desc))

let pClearedTxDesc =
    pipe3
        (pstring "*")
        spacesTabs
        (restOfLine true)
        (fun _ _ desc -> (TransactionState.Cleared, desc))

let pUnmarkedTxDesc =
    (restOfLine true)
    |>> (fun desc -> (TransactionState.Unmarked, desc))

let pTxStateAndDescription =
    pPendingTxDesc
    <|> pClearedTxDesc
    <|> pUnmarkedTxDesc

let pTxFirstLine =
    pipe3
        pDate
        spacesTabs1
        pTxStateAndDescription
        (fun date _ (state, description) ->
            { Date = date
              State = state
              Description = description.Trim() })

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
    spacesTabs1 >>. pAccount .>> spacesTabs
    <?> "account reference"

let pAmountValue =
    numberLiteral NumberLiteralOptions.DefaultFloat "amount"
    |>> (fun num -> Decimal.Parse(num.String, CultureInfo.InvariantCulture))
    <?> "amount value"

let pCurrencyChar = letter

let pCurrency: Parser<string, unit> = many1Chars pCurrencyChar <?> "currency"

let pAmountCurrency =
    (spacesTabs1 >>. pCurrency) |> attempt
    <??> "amount currency"

let pAmount: Parser<Amount, unit> =
    pipe2
        pAmountValue
        (opt pAmountCurrency)
        (fun amount currency ->
            match currency with
            | Some currency -> { Value = amount; Currency = currency }
            | None -> { Value = amount; Currency = "EUR" })
    <??> "amount"

let pTotalPriceIndicator =
    pstring "@@" >>% ()
    <?> "total price indicator (@@)"

let pTotalPrice =
    (spacesTabs1 >>? pTotalPriceIndicator
     .>>? spacesTabs1
     >>. pAmount)
    |> attempt
    <??> "total price"

let pExpectedBalance =
    (pchar ' ' >>. spacesTabs1 >>? (pstring "=")
     .>> spacesTabs1
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

let pPostingLines: Parser<PostingLine list, unit> = many pPostingLine

let pTx =
    pTxFirstLine .>>. pPostingLines
    |>> (fun (info, postings) -> { Info = info; Postings = postings })
