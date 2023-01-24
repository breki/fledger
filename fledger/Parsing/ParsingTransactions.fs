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

/// Parses the account name in the transaction posting, when the posting does
/// not consist of the account name only.
let pAccountNameInPosting<'T> tillChars : Parser<AccountRef, 'T> =
    many1CharsTill pAccountChar (pstring tillChars)
    |>> (fun accountNameRaw ->
        let accountName = accountNameRaw.Trim()

        if accountName = "" then
            failwith "empty account name"
        else
            AccountRef.Create(accountName))
    <??> "account name"

/// Parses the account name in the transaction posting, when the posting
/// consists of the account name only.
let pAccountNameInPostingAccountOnly<'T> : Parser<AccountRef, 'T> =
    many1Chars pAccountChar
    |>> (fun accountNameRaw ->
        let accountName = accountNameRaw.Trim()

        if accountName = "" then
            failwith "empty account name"
        else
            AccountRef.Create(accountName))
    <??> "account name"

/// Parses the posting that contains the posting amount, total price and
/// expected balance.
let pPostingLineAmountPriceBalance<'T> : Parser<PostingLine, 'T> =
    whitespace >>. pAccountNameInPosting "  " .>> whitespace .>>. pAmount
    .>> whitespace
    .>> (pstring "@@")
    .>> whitespace
    .>>. pAmount
    .>> whitespace
    .>> (pstring "=")
    .>> whitespace
    .>>. pAmount
    .>> endOfLineWhitespace
    |>> fun (((account, amount), totalPrice), expectedBalance) ->
            { Account = account
              Amount =
                Some
                    { Amount = amount
                      TotalPrice = Some totalPrice }
              ExpectedBalance = Some expectedBalance }
    <??> "posting line (amount, total price, expected balance)"

/// Parses the posting that contains the posting amount and total price.
let pPostingLineAmountPrice<'T> : Parser<PostingLine, 'T> =
    whitespace >>. pAccountNameInPosting "  " .>> whitespace .>>. pAmount
    .>> whitespace
    .>> (pstring "@@")
    .>> whitespace
    .>>. pAmount
    .>> endOfLineWhitespace
    |>> fun ((account, amount), totalPrice) ->
            { Account = account
              Amount =
                Some
                    { Amount = amount
                      TotalPrice = Some totalPrice }
              ExpectedBalance = None }
    <??> "posting line (amount, total price)"

/// Parses the posting that contains the posting amount and
/// expected balance.
let pPostingLineAmountBalance<'T> : Parser<PostingLine, 'T> =
    whitespace >>. pAccountNameInPosting "  " .>> whitespace .>>. pAmount
    .>> whitespace
    .>> (pstring "=")
    .>> whitespace
    .>>. pAmount
    .>> endOfLineWhitespace
    |>> fun ((account, amount), expectedBalance) ->
            { Account = account
              Amount = Some { Amount = amount; TotalPrice = None }
              ExpectedBalance = Some expectedBalance }
    <??> "posting line (amount, expected balance)"

/// Parses the posting that contains only the posting amount.
let pPostingLineAmount<'T> : Parser<PostingLine, 'T> =
    whitespace >>. pAccountNameInPosting "  " .>> whitespace .>>. pAmount
    .>> endOfLineWhitespace
    |>> fun (account, amount) ->
            { Account = account
              Amount = Some { Amount = amount; TotalPrice = None }
              ExpectedBalance = None }
    <??> "posting line (amount only)"

/// Parses the posting that contains only the expected balance.
let pPostingLineBalance<'T> : Parser<PostingLine, 'T> =
    whitespace >>. pAccountNameInPosting "  "
    .>> whitespace
    .>> (pstring "=")
    .>> whitespace
    .>>. pAmount
    .>> endOfLineWhitespace
    |>> fun (account, expectedBalance) ->
            { Account = account
              Amount = None
              ExpectedBalance = Some expectedBalance }
    <??> "posting line (expected balance only)"

/// Parses the posting that contains only the account name.
let pPostingLineAccountOnly<'T> : Parser<PostingLine, 'T> =
    whitespace >>. pAccountNameInPostingAccountOnly
    |>> fun account ->
            { Account = account
              Amount = None
              ExpectedBalance = None }
    <??> "posting line (account only)"

/// Parses the posting line, by trying out different variants of postings lines.
/// Succeeds only if one of the posting variant parsers succeeds.
let pPostingLine<'T> : Parser<PostingLine, 'T> =
    attempt pPostingLineBalance
    <|> attempt pPostingLineAmountPriceBalance
    <|> attempt pPostingLineAmountPrice
    <|> attempt pPostingLineAmountBalance
    <|> attempt pPostingLineAmount
    <|> attempt pPostingLineAccountOnly
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
    manyTill pPostingLine pEndOfTransaction <??> "posting lines"

/// Parses a transaction.
let pTx<'T> : Parser<int64 * TransactionDirective, 'T> =
    pTxFirstLine .>>. pPostingLines |> withPos
    |>> (fun x ->
        let info, postings = x.Value
        (x.Start.Line, { Info = info; Postings = postings }))
    <??> "tx"
