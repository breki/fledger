module fledger.Parsing.ParsingTransactions


open FParsec

open fledger.BasicTypes
open fledger.Journal
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingAmounts
open fledger.Parsing.ParsingUtils

// status character = "!" | "*"
let pTxStatus<'T> : Parser<char, 'T> = pchar '!' <|> pchar '*' <??> "tx status"

// tx description and comment
//    = [tx description], ["|", [payee], ["|", [note]]],
//      [";", [tx comment]], end of line
let pTxDescriptionAndComment<'T> : Parser<string option * string option * string option * string option, 'T> =
    opt (manyChars (noneOf "|;\n"))
    .>>. opt (pstring "|" >>. manyChars (noneOf "|;\n"))
    .>>. opt (pstring "|" >>. manyChars (noneOf ";\n"))
    .>>. ((pstring ";" >>. opt (restOfLine true)) <|> opt (restOfLine true))
    <??> "tx description and comment"
    |>> fun (((description, payee), note), comment) ->
            (trimStringOptional description,
             trimStringOptional payee,
             trimStringOptional note,
             trimStringOptional comment)

/// Parses the first line of the transaction. It has to start with a date.
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
    |>> fun ((date, status), (description, payee, note, comment)) ->
            { Date = date
              Status = status
              Description = description
              Payee = payee
              Note = note
              Comment = comment }
    <??> "tx first line"

type PostingValues = JournalPostingAmount option * JournalAmount option

/// Parses the transaction posting expected balance.
let pExpectedBalance<'T> : Parser<PostingValues, 'T> =
    whitespace >>. pAmount .>> endOfLineWhitespace
    |>> fun expectedBalance -> None, Some expectedBalance
    <??> "posting line values (expected balance)"

/// Parses the posting values when the only the expected balance is specified.
let pPostingBalance<'T> : Parser<PostingValues, 'T> =
    whitespace >>. (pstring "=") >>. pExpectedBalance

/// Parses the posting values when the transaction amount, total price  and
/// expected balance are specified.
let pPostingAmountPriceBalance<'T> : Parser<PostingValues, 'T> =
    whitespace >>. pAmount .>> whitespace .>> (pstring "@@") .>> whitespace
    .>>. pAmount
    .>> whitespace
    .>> (pstring "=")
    .>> whitespace
    .>>. pAmount
    .>> endOfLineWhitespace
    |>> (fun ((amount, totalPrice), expectedBalance) ->
        Some
            { Amount = amount
              TotalPrice = Some totalPrice },
        Some expectedBalance)
    <??> "posting line values (amount, total price, expected balance)"

/// Parses the posting values when the transaction amount and total price
/// are specified.
let pPostingAmountPrice<'T> : Parser<PostingValues, 'T> =
    whitespace >>. pAmount .>> whitespace .>> (pstring "@@") .>> whitespace
    .>>. pAmount
    .>> endOfLineWhitespace
    |>> (fun (amount, totalPrice) ->
        Some
            { Amount = amount
              TotalPrice = Some totalPrice },
        None)
    <??> "posting line values (amount, total price)"

/// Parses the posting values when the transaction amount and expected balance
/// are specified.
let pPostingAmountBalance<'T> : Parser<PostingValues, 'T> =
    whitespace >>. pAmount .>> whitespace .>> (pstring "=") .>> whitespace
    .>>. pAmount
    .>> endOfLineWhitespace
    |>> (fun (amount, expectedBalance) ->
        Some { Amount = amount; TotalPrice = None }, Some expectedBalance)
    <??> "posting line values (amount, expected balance)"

let pPostingAmount<'T> : Parser<PostingValues, 'T> =
    whitespace >>. pAmount .>> endOfLineWhitespace
    |>> (fun amount -> Some { Amount = amount; TotalPrice = None }, None)
    <??> "posting line values (amount only)"

/// Parses the values part of the posting (except the case when the posting does
/// not have any values or just the expected balance is specified).
let pAmountPriceBalance<'T> : Parser<PostingValues, 'T> =
    attempt pPostingBalance
    <|> attempt pPostingAmountPriceBalance
    <|> attempt pPostingAmountPrice
    <|> attempt pPostingAmountBalance
    <|> attempt pPostingAmount
    <??> "posting line values"

/// Parses the values part of the posting (the remainder of the
/// line after the account name).
let pPostingValues<'T> : Parser<PostingValues, 'T> =
    attempt (endOfLineWhitespace >>% (None, None))
    <|> attempt ((pstring " =") >>. pExpectedBalance)
    <|> attempt (pstring "  " >>. pAmountPriceBalance)
    <??> "posting line (after account name)"

/// Parses the transaction posting.
let pPosting<'T> : Parser<PostingLine, 'T> =
    whitespace1
    >>. many1CharsTillApply
        pAccountChar
        pPostingValues
        (fun accountNameRaw (amount, expectedBalance) ->
            let accountName = accountNameRaw.Trim()

            let account =
                if accountName = "" then
                    failwith "empty account name"
                else
                    AccountRef.Create(accountName)

            { Account = account
              Amount = amount
              ExpectedBalance = expectedBalance })
    <??> "posting line"


/// Succeeds if the end of the transaction block is found. The transaction block
/// ends either by an empty line, a line starting with non-whitespace or if the
/// end of file is reached.
/// Does not change the parser state.
let pEndOfTransaction<'T> : Parser<unit, 'T> =
    (followedBy pEmptyLine) <|> (followedBy nonWhitespaceChar) <|> eof
    <??> "end of transaction"

/// Parses the posting lines of a transaction.
let pPostingLines<'T> : Parser<PostingLine list, 'T> =
    manyTill pPosting pEndOfTransaction <??> "posting lines"

/// Parses a transaction.
let pTx<'T> : Parser<int64 * TransactionDirective, 'T> =
    pTxFirstLine .>>. pPostingLines |> withPos
    |>> (fun x ->
        let info, postings = x.Value
        (x.Start.Line, { Info = info; Postings = postings }))
    <??> "tx"
