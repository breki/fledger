module fledger.``parsing account directive``


open System
open Xunit
open FsCheck
open FParsec
open Text

open Xunit.Abstractions

open fledger.Journal
open fledger.ParsingBasics

let pAccountNameInDirective<'T> : Parser<string, 'T> =
    many1CharsTill pAccountChar newlineOrEof
    <??> "account name"

let pAccountDirective =
    pstring "account" .>> whitespace1
    >>. pAccountNameInDirective
    |>> (fun name -> { AccountName = name } |> Account)

let chooseArbitraryAccountDirective () =
    gen {
        let mutable textBuilder =
            buildString ()
            |> append "account assets:current assets:Revolut"

        let text = textBuilder |> toString

        let expectedValue =
            { AccountName = "assets:current assets:Revolut" }
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
