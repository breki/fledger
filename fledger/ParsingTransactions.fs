module fledger.ParsingTransactions


open FParsec

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingAmounts

// status character = "!" | "*"
let pTxStatus<'T> : Parser<char, 'T> =
    pchar '!' <|> pchar '*' <??> "tx status"

// tx description and comment = [tx description], [";" [tx comment]], end of line
let pTxDescriptionAndComment<'T> : Parser<string option * string option, 'T> =
    opt (manyChars (noneOf ";\n"))
    .>>. ((pstring ";" >>. opt (restOfLine true))
          <|> opt (restOfLine true))
    <??> "tx description and comment"
    |>> fun (description, comment) ->
            (trimStringOptional description, trimStringOptional comment)

// tx first line = date, [whitespace1], [status character], [whitespace],
//                 [tx description], [whitespace], [tx comment]
let pTxFirstLine<'T> : Parser<TransactionInfo, 'T> =
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

let pAccountChar<'T> : Parser<char, 'T> =
    choice [ letter
             digit
             pchar ':'
             pchar '-'
             pchar '_'
             pchar ' ' ]
    <??> "account name character "

let pAmountSeparator<'T> : Parser<string, 'T> =
    (pstring " " .>> (pstring " ") |> attempt
     <??> "amount separator")

let pAccount<'T> : Parser<string, 'T> =
    many1CharsTill pAccountChar pAmountSeparator
    <??> "account name"

let pAccountRef<'T> : Parser<string, 'T> =
    pAccount .>> whitespace <??> "account reference"

let pTotalPriceIndicator<'T> : Parser<unit, 'T> =
    pstring "@@" >>% ()
    <??> "total price indicator (@@)"

let pTotalPrice<'T> : Parser<Amount, 'T> =
    (whitespace1 >>? pTotalPriceIndicator
     .>>? whitespace1
     >>. pAmount)
    |> attempt
    <??> "total price"

let pExpectedBalance<'T> : Parser<Amount, 'T> =
    (pchar ' ' >>. whitespace1 >>? (pstring "=")
     .>> whitespace1
     >>. pAmount)
    |> attempt
    <??> "expected balance"

let pPostingLineActual<'T> : Parser<PostingLine option, 'T> =
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

let pPostingLine<'T> : Parser<PostingLine option, 'T> =
    attempt pPostingLineActual
    <|> (pEmptyLine >>% None)
    <??> "posting line"

let pPostingLines<'T> : Parser<PostingLine list, 'T> =
    many pPostingLine |>> filterOutNone
    <??> "posting lines"

// todo 20: support for payee and note (pipe characters)
let pTx<'T> : Parser<Transaction, 'T> =
    pTxFirstLine .>>. pPostingLines
    |>> (fun (info, postings) -> { Info = info; Postings = postings })
    <??> "tx"
