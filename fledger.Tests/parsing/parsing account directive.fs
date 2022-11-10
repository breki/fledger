module fledger.``parsing account directive``


open System
open Xunit
open FsCheck
open FParsec
open Text

open Xunit.Abstractions

open fledger.BasicTypes
open fledger.Journal
open fledger.Parsing.ParsingAccountDirective

let chooseArbitraryAccountDirective () =
    gen {
        let mutable textBuilder =
            buildString ()
            |> appendLine "account assets:current assets:Revolut/test"
            |> appendLine ""
            |> appendLine "  note  (type: BANK)  "
            |> appendLine " note  (type: BANK)"
            |> appendLine ""

        let text = textBuilder |> toString

        let expectedValue =
            { Account =
                "assets:current assets:Revolut/test"
                |> AccountRef.Create
              Subdirectives =
                [ "note  (type: BANK)"
                  "note  (type: BANK)" ] }
            |> Account

        let result =
            runParserOnString pAccountDirective () "test stream" text

        return textBuilder, expectedValue, result
    }

type AccountDirectiveParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing account directive``() =
        let arbAccountDirective =
            chooseArbitraryAccountDirective () |> Arb.fromGen

        let isParsedCorrectly (_, expectedValue, parserResult) =
            match parserResult with
            | Success (parsedValue, _, _) ->
                // output.WriteLine "PARSING SUCCESS"
                parsedValue = expectedValue
            | Failure (errorMsg, _, _) ->
                output.WriteLine $"PARSING ERROR: {errorMsg}"
                false

        isParsedCorrectly
        |> Prop.forAll arbAccountDirective
        |> Check.QuickThrowOnFailure
