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


type AmountCommodityCase =
    | NoAmount
    | AmountOnly
    | AmountCommodity

type AmountValueCase =
    | Positive
    | Negative
    | Zero
    | WithoutDecimals
    | WithDecimalPoint


let amountStrBasedOnCase case =
    match case with
    | Positive -> "5.37"
    | Negative -> "-5.37"
    | Zero -> "0"
    | WithoutDecimals -> "5"
    | WithDecimalPoint -> "5."


let renderAmountBasedOnCase prefix amount commodity case =
    match case with
    | NoAmount -> ""
    | AmountOnly -> $"{prefix}{amount}"
    | AmountCommodity -> $"{prefix}{amount} {commodity}"

let constructAmountBasedOnCase valueCase commodity case =
    let amountValueBasedOnCase case =
        match case with
        | Positive -> 5.37m
        | Negative -> -5.37m
        | Zero -> 0m
        | WithoutDecimals -> 5m
        | WithDecimalPoint -> 5.0m

    let amount = amountValueBasedOnCase valueCase

    match case with
    | NoAmount -> None
    | AmountOnly -> Some { Value = amount; Commodity = None }
    | AmountCommodity ->
        Some
            { Value = amount
              Commodity = Some commodity }

let chooseArbitraryPosting () =
    gen {
        let! amountCommodityCase = Arb.from<AmountCommodityCase>.Generator
        let! amountValueCase = Arb.from<AmountValueCase>.Generator
        let! extraWhitespaceBeforeAmount = Arb.from<bool>.Generator
        let! extraWhitespaceBetweenAmountAndCommodity = Arb.from<bool>.Generator
        let! totalPriceCase = Arb.from<AmountCommodityCase>.Generator
        let! totalPriceValueCase = Arb.from<AmountValueCase>.Generator
        let! expectedBalanceCase = Arb.from<AmountCommodityCase>.Generator
        let! expectedBalanceValueCase = Arb.from<AmountValueCase>.Generator
        let! extraWhitespaceBeforeEquals = Arb.from<bool>.Generator
        let! extraWhitespaceAfterEquals = Arb.from<bool>.Generator
        let! extraWhitespaceAtPostingEnd = Arb.from<bool>.Generator

        // construct the text of the posting line that needs to be parsed
        let text =
            buildString ()
            |> append "    assets:current assets:NLB"
            |> (append (
                renderAmountBasedOnCase
                    (if extraWhitespaceBeforeAmount then "     " else "  ")
                    (amountStrBasedOnCase amountValueCase)
                    (if extraWhitespaceBetweenAmountAndCommodity then
                         "  EUR"
                     else
                         "EUR")
                    amountCommodityCase
            ))

        let text =
            if amountCommodityCase <> NoAmount then
                text
                |> (append (
                    renderAmountBasedOnCase
                        " @@ "
                        (amountStrBasedOnCase totalPriceValueCase)
                        "USD"
                        totalPriceCase
                ))
            else
                text

        let text =
            text
            |> (append (
                renderAmountBasedOnCase
                    ((if extraWhitespaceBeforeEquals then "   " else "")
                     + " ="
                     + (if extraWhitespaceAfterEquals then "   " else ""))
                    (amountStrBasedOnCase expectedBalanceValueCase)
                    "EUR"
                    expectedBalanceCase
            ))
            |> ifDo extraWhitespaceAtPostingEnd (append "   ")

        let text = text |> toString

        // construct the expected posting line object
        let expectedValue =
            { Account = "assets:current assets:NLB" |> AccountRef.Create
              Amount = None
              ExpectedBalance = None }

        let expectedValue =
            if amountCommodityCase <> NoAmount then
                { expectedValue with
                    Amount =
                        Some
                            { Amount =
                                (constructAmountBasedOnCase
                                    amountValueCase
                                    "EUR"
                                    amountCommodityCase)
                                |> Option.get
                              TotalPrice =
                                (constructAmountBasedOnCase
                                    totalPriceValueCase
                                    "USD"
                                    totalPriceCase) } }
            else
                expectedValue

        let expectedValue =
            { expectedValue with
                ExpectedBalance =
                    constructAmountBasedOnCase
                        expectedBalanceValueCase
                        "EUR"
                        expectedBalanceCase }

        let result, ex =
            try
                let result = runParserOnString pPosting () "test stream" text

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
