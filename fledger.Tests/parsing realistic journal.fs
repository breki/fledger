module fledger.``parsing realistic journal``


open System.IO
open Xunit
open FsCheck
open FParsec

open Xunit.Abstractions

open fledger.ParsingBasics
open fledger.ParsingJournal

open Swensen.Unquote

type RealisticJournalParsingTests(output: ITestOutputHelper) =
    [<Fact(Skip = "Only to be run to debug problems "
                  + "with parsing realistic journals")>]
    member this.``parsing realistic journal``() =
        let text =
            File.ReadAllText(@"D:\ledger\igor.ledger")

        let result =
            runParserOnString pJournal { Something = 0 } "test stream" text

        match result with
        | Success (value, _, _) ->
            output.WriteLine $"PARSING SUCCESS: {value}"
            test <@ true @>
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>
