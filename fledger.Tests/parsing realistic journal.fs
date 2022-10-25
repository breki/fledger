module fledger.``parsing realistic journal``


open Xunit
open System.IO
open FsCheck
open FParsec

open Xunit.Abstractions

open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingJournal
open fledger.Ledger

open Swensen.Unquote

type RealisticJournalParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    // [<Fact(Skip = "Only to be run to debug problems "
    //               + "with parsing realistic journals")>]
    member this.``parsing realistic journal``() =
        let text =
            File.ReadAllText(@"D:\ledger\igor.ledger")

        let result =
            runParserOnString pJournal { Something = 0 } "test stream" text

        match result with
        | Success (journal, _, _) ->
            // output.WriteLine $"PARSING SUCCESS: {journal}"

            let ledger = fillLedger journal

            test <@ ledger.Accounts.Count > 20 @>
            test <@ ledger.Transactions.Length > 1000 @>
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>
