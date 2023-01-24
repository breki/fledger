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
open fledger.Tests.JournalBuilders

type PostingAmount =
    | NoAmount
    | Amount
    | AmountAndTotalPrice


// todo 9: implement a truly arbitrary posting, including the optional amount
//   and various combinations of whitespace
let chooseArbitraryPosting () =
    gen {
        let! amountCase = Arb.from<PostingAmount>.Generator
        let! hasExpectedBalance = Arb.from<bool>.Generator

        let mutable textBuilder =
            buildString ()
            |> append "    assets:current assets:NLB"
            |> ifDo (amountCase <> NoAmount) (fun x ->
                x |> append "          -5.37 EUR")
            |> ifDo (amountCase = AmountAndTotalPrice) (fun x ->
                x |> append " @@ 7.35 USD")
            |> ifDo (amountCase = NoAmount) (fun x -> x |> append " ")
            |> ifDo hasExpectedBalance (fun x -> x |> append " =14698.51 EUR")

        let text = textBuilder |> toString

        let expectedValue =
            { Account = "assets:current assets:NLB" |> AccountRef.Create
              Amount = None
              ExpectedBalance = None }
            |> ifDo (amountCase <> NoAmount) (fun p ->
                p |> withAmount -5.37m (Some "EUR"))
            |> ifDo (amountCase = AmountAndTotalPrice) (fun p ->
                p |> withTotalPrice 7.35m "USD")
            |> ifDo hasExpectedBalance (fun p ->
                p |> withExpectedBalance 14698.51m "EUR")
            |> Some

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
                output.WriteLine $"PARSING SUCCESS: '{originalText}'"
                parsedValue = expectedValue
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
