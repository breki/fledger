module fledger.Tests.fparsec_exploration

open System
open Xunit

open FParsec
open Swensen.Unquote
open Xunit.Abstractions

let filterOutNone items =
    items
    |> (List.filter Option.isSome)
    |> List.map Option.get

let pTxPosting: Parser<char, unit> =
    pchar 'x' <??> "posting"

let pTx =
    pchar 'T' .>> (many pTxPosting) <??> "tx"

let pDc: Parser<char, unit> =
    pchar 'D' <??> "default commodity"

let pEmptyLine: Parser<char, unit> =
    pchar ' ' <??> "empty line"

let pJournalItem =
    (pTx |>> Some)
    <|> (pDc |>> Some)
    <|> (pEmptyLine |>> (fun _ -> None))
    <??> "journal item"

let pJournal =
    (many pJournalItem |>> filterOutNone) .>> eof
    <??> "journal"

type FParsecExplorationTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.parsingSimpleJournal() =
        let result = run pJournal "Tx x D TTxD DD"

        match result with
        | Success (journal, _, _) ->
            let printableJournal =
                journal |> List.toArray |> String

            output.WriteLine $"PARSING SUCCESS: {printableJournal}"
            test <@ true @>
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>
