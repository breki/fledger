module fledger.ParsingAccountDirective

open FParsec


open fledger.Journal
open fledger.ParsingBasics

let pAccountNameInDirective<'T> : Parser<string, 'T> =
    many1CharsTill pAccountChar newlineOrEof
    <??> "account name"

let pAccountSubdirective<'T> : Parser<string option, 'T> =
    whitespace1 >>. restOfLine true |>> Some
    <??> "account subdirective"

let pAccountSubdirectiveMaybe<'T> : Parser<string option, 'T> =
    attempt pAccountSubdirective
    <|> (pEmptyLine >>% None)
    |>> trimStringOptional
    <??> "account subdirective maybe"

let pAccountSubdirectives<'T> : Parser<string list, 'T> =
    many pAccountSubdirectiveMaybe |>> filterOutNone
    <??> "account subdirectives"

let pAccountDirective<'T> : Parser<JournalItem, 'T> =
    pstring "account" .>> whitespace1
    >>. pAccountNameInDirective
    .>>. pAccountSubdirectives
    |>> (fun (accountName, subdirectives) ->
        { AccountName = accountName
          Subdirectives = subdirectives }
        |> Account)
