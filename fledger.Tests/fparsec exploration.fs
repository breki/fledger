﻿module fledger.Tests.fparsec_exploration

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
    pchar 'x' .>> (many digit) <??> "posting"

let pTxPostingOrEmptyLine =
    (pTxPosting |>> Some)
    <|> (pchar ' ' |>> (fun _ -> None))
    <??> "posting or empty line"

let pTx: Parser<string, unit> =
    pchar 'T' .>>. (many pTxPostingOrEmptyLine)
    |>> (fun (c, postings) ->
        c :: filterOutNone postings
        |> List.toArray
        |> String)
    <??> "tx"

let pDc: Parser<string, unit> =
    pchar 'D' |>> (fun _ -> "D")
    <??> "default commodity"

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
        let result =
            run pJournal "Tx10 x20 D TTx333D D D"

        match result with
        | Success (journal, _, _) ->
            let printableJournal =
                journal |> List.toArray |> String.Concat

            output.WriteLine $"PARSING SUCCESS: {printableJournal}"
            test <@ true @>
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>
