module fledger.Parsing.ParsingAccountDirective

open FParsec


open fledger.BasicTypes
open fledger.Journal
open fledger.Parsing.ParsingBasics

let pAccountNameInDirective<'T> : Parser<AccountRef, 'T> =
    many1CharsTill pAccountChar newlineOrEof
    |>> AccountRef.Create
    <??> "account name"

let pAccountSubdirective<'T> : Parser<AccountName option, 'T> =
    whitespace1 >>. restOfLine true |>> Some
    <??> "account subdirective"

let pAccountSubdirectiveMaybe<'T> : Parser<AccountName option, 'T> =
    attempt pAccountSubdirective
    <|> (pEmptyLine >>% None)
    |>> trimStringOptional
    <??> "account subdirective maybe"

let pAccountSubdirectives<'T> : Parser<AccountName list, 'T> =
    many pAccountSubdirectiveMaybe |>> filterOutNone
    <??> "account subdirectives"

let pAccountDirective<'T> : Parser<JournalItem, 'T> =
    pstring "account" .>> whitespace1
    >>. pAccountNameInDirective
    .>>. pAccountSubdirectives
    |>> (fun (accountName, subdirectives) ->
        { Account = accountName
          Subdirectives = subdirectives }
        |> Account)
