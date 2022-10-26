module fledger.``parsing tx posting``


open System
open Xunit
open FsCheck
open FParsec
open Text

open Xunit.Abstractions

open fledger.BasicTypes
open fledger.Journal
open fledger.Parsing.ParsingTransactions

//     assets:current assets:PayPal       110.47 CAD =1.270,99 CAD


let chooseArbitraryPosting () =
    gen {
        let mutable textBuilder =
            buildString ()
            |> appendLine
                "    assets:current assets:NLB          -5.37 EUR  =14698.51"

        let text = textBuilder |> toString

        let expectedValue =
            { Account = "assets:current assets:NLB" |> AccountRef.Create
              Amount =
                { Value = -5.37m
                  Commodity = Some "EUR" }
              TotalPrice = None
              ExpectedBalance = Some { Value = 14698.51m; Commodity = None } }
            |> Some

        let result =
            runParserOnString pPostingLineActual () "test stream" text

        return textBuilder, expectedValue, result
    }

type TxPostingParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing transaction posting``() =
        let arbPosting =
            chooseArbitraryPosting () |> Arb.fromGen

        let transactionIsParsedCorrectly (_, expectedValue, parserResult) =
            match parserResult with
            | Success (parsedValue, _, _) ->
                // output.WriteLine "PARSING SUCCESS"
                parsedValue = expectedValue
            | Failure (errorMsg, _, _) ->
                output.WriteLine $"PARSING ERROR: {errorMsg}"
                false

        transactionIsParsedCorrectly
        |> Prop.forAll arbPosting
        |> Check.QuickThrowOnFailure
