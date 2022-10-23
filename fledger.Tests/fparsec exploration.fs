module fledger.Tests.fparsec_exploration

open System
open Xunit

open FParsec
open Swensen.Unquote
open Xunit.Abstractions

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingAmounts
open fledger.ParsingTransactions

let filterOutNone items =
    items
    |> (List.filter Option.isSome)
    |> List.map Option.get

let whitespace: Parser<string, UserState> =
    manyChars (pchar ' ' <|> pchar '\t')
    <??> "whitespace"

let whitespace1: Parser<string, UserState> =
    many1Chars (pchar ' ' <|> pchar '\t')
    <??> "whitespace1"

let newlineOrEof: Parser<unit, UserState> =
    (newline |>> fun _ -> ()) <|> eof
    <??> "newline or eof"

let emptyLine =
    whitespace .>> newlineOrEof <??> "empty line"

// status character = "!" | "*"
let pTxStatus: Parser<char, UserState> =
    pchar '!' <|> pchar '*' <??> "tx status"

// let pTxPosting: Parser<char, UserState> =
//     pchar 'x' .>> (many digit) .>> newlineOrEof
//     <??> "posting"
let pTxPosting: Parser<PostingLine, UserState> =
    pAccountRef .>>. pAmount .>> restOfLine true
    // .>>. (opt pTotalPrice)
    // .>>. (opt pExpectedBalance)
    |>> fun _ ->
            { Account = "some account"
              Amount = { Value = 0.0m; Currency = None }
              TotalPrice = None
              ExpectedBalance = None }
    <??> "posting"

let pTxPostingOrEmptyLine =
    (whitespace1 >>. pTxPosting |>> Some)
    <|> (newlineOrEof |>> (fun _ -> None))
    <??> "posting or empty line"

let pTx: Parser<JournalItem, UserState> =
    pTxFirstLine .>>. (many pTxPostingOrEmptyLine)
    |>> (fun (txInfo, postings) ->
        let postings =
            filterOutNone postings
            |> List.map (fun _ ->
                { Account = ""
                  Amount = { Value = 0m; Currency = None }
                  TotalPrice = None
                  ExpectedBalance = None })

        Transaction({ Info = txInfo; Postings = postings }))
    <??> "tx"

let pDc: Parser<JournalItem, UserState> =
    pchar 'D' .>> newlineOrEof
    |>> (fun _ -> DefaultCommodity({ Value = 0m; Currency = None }))
    <??> "default commodity"

let pEmptyLine: Parser<char, UserState> =
    whitespace >>. newline <??> "empty line"

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
            runParserOnString
                pJournal
                { Output = output }
                "test stream"
                @"
2022/01/06!
  expenses:Business:Service charges    0.39 EUR

 expenses:Business:Service charges    0.39 EUR

D

2022/01/06 description; comment
2022/01/06 *description
   expenses:Business:Service charges    0.39 EUR
D

D

D"

        match result with
        | Success (journal, _, _) ->
            let printableJournal =
                journal
                |> List.map (fun item ->
                    match item with
                    | Transaction tx ->
                        let postingsStr =
                            tx.Postings
                            |> List.map (fun _ -> "x")
                            |> String.concat ""

                        "T" + postingsStr
                    | DefaultCommodity _ -> "D")
                |> List.toArray
                |> String.Concat

            output.WriteLine $"PARSING SUCCESS: {printableJournal}"
            test <@ printableJournal = "TxxDTTxDDD" @>
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>
