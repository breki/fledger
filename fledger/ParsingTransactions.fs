module fledger.ParsingTransactions


open FParsec

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingAmounts


// status character = "!" | "*"
let pTxStatus: Parser<char, UserState> =
    pchar '!' <|> pchar '*' <??> "tx status"

// tx description and comment = [tx description], [";" [tx comment]], end of line
let pTxDescriptionAndComment: Parser<string option * string option, UserState> =
    opt (manyChars (noneOf ";\n"))
    .>>. ((pstring ";" >>. opt (restOfLine true))
          <|> opt (restOfLine true))
    <??> "tx description and comment"
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
    <??> "tx first line"

let pAccountChar =
    choice [ letter
             digit
             pchar ':'
             pchar '-'
             pchar '_'
             pchar ' ' ]
    <??> "account name character "

let pAmountSeparator =
    (pstring " " .>> (pstring " ") |> attempt
     <??> "amount separator")

let pAccount: Parser<string, UserState> =
    many1CharsTill pAccountChar pAmountSeparator
    <??> "account name"

let pAccountRef =
    pAccount .>> whitespace <??> "account reference"

let pTotalPriceIndicator =
    pstring "@@" >>% ()
    <??> "total price indicator (@@)"

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
    <??> "posting line"

let pPostingLine =
    attempt pPostingLineActual
    <|> (pEmptyLine >>% None)
    <??> "posting line"

let pPostingLines: Parser<PostingLine list, UserState> =
    many pPostingLine |>> filterOutNone
    <??> "posting lines"

let pTx: Parser<Transaction, UserState> =
    pTxFirstLine .>>. pPostingLines
    |>> (fun (info, postings) -> { Info = info; Postings = postings })
    <??> "tx"
