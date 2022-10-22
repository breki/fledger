﻿module fledger.JournalParsing

open Journal

open System
open System.Globalization

open FParsec

let filterOutNone items =
    items
    |> (List.filter Option.isSome)
    |> List.map Option.get

let trimStringOptional (s: string option) =
    match s with
    | Some s ->
        s.Trim()
        |> function
            | "" -> None
            | trimmed -> Some trimmed
    | None -> None

let whitespace: Parser<string, unit> =
    manyChars (pchar ' ' <|> pchar '\t')
    <?> "whitespace"

let whitespace1: Parser<string, unit> =
    many1Chars (pchar ' ' <|> pchar '\t')
    <?> "whitespace 1"

let endOfLineWhitespace: Parser<unit, unit> =
    whitespace >>. newline >>% ()
    <?> "end of line whitespace"

let pEmptyLine =
    endOfLineWhitespace <?> "empty line"

let pDatePartSeparator: Parser<char, unit> =
    pchar '/' <|> pchar '-' <?> "date part separator"

let pYear =
    pint32 .>> pDatePartSeparator <?> "year"

let pMonth =
    pint32 .>> pDatePartSeparator <?> "month"

let pDay = pint32 <?> "day"

let pDate =
    pipe3 pYear pMonth pDay (fun year month day -> DateTime(year, month, day))
    <?> "date"

// status character = "!" | "*"
let pTxStatus: Parser<char, unit> =
    pchar '!' <|> pchar '*' <?> "tx status"

// tx description and comment = [tx description], [";" [tx comment]], end of line
let pTxDescriptionAndComment: Parser<string option * string option, unit> =
    opt (manyChars (noneOf ";\n"))
    .>>. ((pstring ";" >>. opt (restOfLine true))
          <|> opt (restOfLine true))
    <?> "tx description and comment"
    |>> fun (description, comment) ->
            (trimStringOptional description, trimStringOptional comment)

// tx first line = date, [whitespace1], [status character], [whitespace],
//                 [tx description], [whitespace], [tx comment]
let pTxFirstLine =
    pDate .>> whitespace
    .>>. (opt pTxStatus
          |>> fun optChar ->
                  match optChar with
                  | Some '*' -> TransactionStatus.Cleared
                  | Some '!' -> TransactionStatus.Pending
                  | None -> TransactionStatus.Unmarked
                  | _ -> failwith "invalid transaction state")
    .>> whitespace
    .>>. pTxDescriptionAndComment
    |>> fun ((date, status), (description, comment)) ->
            { Date = date
              Status = status
              Description = description
              Comment = comment }
    <?> "tx first line"

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
    pAccount .>> whitespace <?> "account reference"

let pAmountValue =
    numberLiteral NumberLiteralOptions.DefaultFloat "amount"
    |>> (fun num -> Decimal.Parse(num.String, CultureInfo.InvariantCulture))
    <?> "amount value"

let pCurrencyChar = letter

let pCurrency: Parser<string, unit> =
    many1Chars pCurrencyChar <?> "currency"

let pAmountCurrency =
    (whitespace1 >>. pCurrency) |> attempt
    <?> "amount currency"

let pAmount: Parser<Amount, unit> =
    pipe2 pAmountValue (opt pAmountCurrency) (fun amount currency ->
        match currency with
        | Some currency -> { Value = amount; Currency = currency }
        | None -> { Value = amount; Currency = "EUR" })
    <?> "amount"

let pTotalPriceIndicator =
    pstring "@@" >>% ()
    <?> "total price indicator (@@)"

let pTotalPrice =
    (whitespace1 >>? pTotalPriceIndicator
     .>>? whitespace1
     >>. pAmount)
    |> attempt
    <?> "total price"

let pExpectedBalance =
    (pchar ' ' >>. whitespace1 >>? (pstring "=")
     .>> whitespace1
     >>. pAmount)
    |> attempt
    <?> "expected balance"

let pPostingLineActual =
    whitespace1 >>. pAccountRef
    .>>. pAmount
    .>>. (opt pTotalPrice)
    .>>. (opt pExpectedBalance)
    .>> endOfLineWhitespace
    |>> fun (((account, amount), totalPrice), expectedBalance) ->
            { Account = account
              Amount = amount
              TotalPrice = totalPrice
              ExpectedBalance = expectedBalance }
            |> Some
    <?> "posting line"

let pPostingLine =
    attempt pPostingLineActual
    <|> (pEmptyLine >>% None)
    <?> "posting line"

let pPostingLines: Parser<PostingLine list, unit> =
    many pPostingLine |>> filterOutNone
    <?> "posting lines"

let pTx =
    pTxFirstLine .>>. pPostingLines
    |>> (fun (info, postings) -> { Info = info; Postings = postings } |> Some)
    <?> "tx"

let pJournal: Parser<Journal, unit> =
    many pTx
    |>> fun txsMaybe ->
            filterOutNone txsMaybe
            |> (fun txs -> { Transactions = txs })
    <?> "journal"
