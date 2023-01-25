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

type AmountCase =
    | NoAmount
    | AmountOnly
    | AmountCommodity

// todo 9: implement a truly arbitrary posting, including the optional amount
//   and various combinations of whitespace

let renderAmountBasedOnCase prefix amount commodity case =
    match case with
    | NoAmount -> ""
    | AmountOnly -> $"{prefix}{amount} {commodity}"
    | AmountCommodity -> $"{amount} {commodity}"

let constructAmountBasedOnCase amount commodity case =
    match case with
    | NoAmount -> None
    | AmountOnly -> Some { Value = amount; Commodity = None }
    | AmountCommodity ->
        Some
            { Value = amount
              Commodity = Some commodity }

let chooseArbitraryPosting () =
    gen {
        let! amountCase = Arb.from<AmountCase>.Generator
        let! totalPriceCase = Arb.from<AmountCase>.Generator
        let! expectedBalanceCase = Arb.from<AmountCase>.Generator

        // construct the text of the posting line that needs to be parsed
        let text =
            buildString ()
            |> append "    assets:current assets:NLB"
            |> (append (renderAmountBasedOnCase "  " -5.37m "EUR" amountCase))

        let text =
            if amountCase <> NoAmount then
                text
                |> (append (
                    renderAmountBasedOnCase " @@ " 7.35m "USD" totalPriceCase
                ))
            else
                text

        let text =
            text
            |> (append (
                renderAmountBasedOnCase " =" 14698.51m "EUR" expectedBalanceCase
            ))

        let text = text |> toString

        // construct the expected posting line object
        let expectedValue =
            { Account = "assets:current assets:NLB" |> AccountRef.Create
              Amount = None
              ExpectedBalance = None }

        let expectedValue =
            if amountCase <> NoAmount then
                { expectedValue with
                    Amount =
                        Some
                            { Amount =
                                (constructAmountBasedOnCase
                                    -5.37m
                                    "EUR"
                                    amountCase)
                                |> Option.get
                              TotalPrice =
                                (constructAmountBasedOnCase
                                    7.35m
                                    "USD"
                                    totalPriceCase) } }
            else
                expectedValue

        let expectedValue =
            { expectedValue with
                ExpectedBalance =
                    constructAmountBasedOnCase
                        14698.51m
                        "EUR"
                        expectedBalanceCase }

        let result, ex =
            try
                let result =
                    runParserOnString pPostingLine () "test stream" text

                Some result, None
            with ex ->
                None, Some ex

        return text, expectedValue, result, ex
    }

type TxPostingParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing transaction posting``() =
        let arbPosting = chooseArbitraryPosting () |> Arb.fromGen

        let transactionIsParsedCorrectly
            (
                originalText,
                expectedValue,
                parserResult,
                ex
            ) =
            match parserResult, ex with
            | Some (Success (parsedValue, _, _)), _ ->
                match parsedValue = expectedValue with
                | true ->
                    output.WriteLine $"PARSING SUCCESS: '{originalText}'"
                    true
                | false ->
                    output.WriteLine $"PARSING WRONG:"
                    output.WriteLine $"text: '{originalText}'"
                    output.WriteLine $"expected: '{expectedValue}'"
                    output.WriteLine $"parsed: '{parsedValue}'"
                    false
            | Some (Failure (errorMsg, _, _)), _ ->
                output.WriteLine $"PARSING ERROR: {errorMsg}"
                false
            | _, Some ex ->
                output.WriteLine $"PARSING EXCEPTION: {ex}"
                false
            | _, _ -> invalidOp "unexpected parser result"

        transactionIsParsedCorrectly
        |> Prop.forAll arbPosting
        |> Check.QuickThrowOnFailure
