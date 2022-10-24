module fledger.``parsing tx first line``


open System
open Xunit
open FsCheck
open FParsec
open Text

open Xunit.Abstractions

open fledger.Journal
open fledger.ParsingTransactions
open fledger.Tests.ParsingUtils


let chooseArbitraryTxFirstLine () =
    gen {
        let! dateFormat = Arb.from<bool>.Generator
        let! hasWhitespaceAfterDate = Arb.from<bool>.Generator
        let! statusChar = Gen.elements [ "!"; "*"; "" ]
        let! hasDescription = Arb.from<bool>.Generator
        let! hasComment = Arb.from<bool>.Generator

        let txString =
            buildString ()
            |> ifDo dateFormat (fun x -> x |> append "2022/01/06")
            |> ifDont dateFormat (fun x -> x |> append "2022-01-06")
            |> ifDo hasWhitespaceAfterDate (fun x -> x |> append " ")
            |> append statusChar
            |> ifDo hasDescription (fun x -> x |> append "s.p. prispevki ")
            |> ifDo hasComment (fun x -> x |> append "; this is a comment ")
            |> toString

        let expectedTxInfo =
            { Date = DateTime(2022, 1, 6)
              Status =
                match statusChar with
                | "!" -> TransactionStatus.Pending
                | "*" -> TransactionStatus.Cleared
                | _ -> TransactionStatus.Unmarked
              Description =
                if hasDescription then
                    Some "s.p. prispevki"
                else
                    None
              Payee = None
              Note = None
              Comment =
                if hasComment then
                    Some "this is a comment"
                else
                    None }

        let result =
            runParserOnString pTxFirstLine () "test stream" txString

        return txString, expectedTxInfo, result
    }

// todo 30: randomize parsing of transaction first line instead of
//   the whole journal
type TxFirstLineParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing transaction's first line``() =
        let arbTxInfo =
            chooseArbitraryTxFirstLine () |> Arb.fromGen

        let transactionIsParsedCorrectly (_, expectedValue, parserResult) =
            match parserResult with
            | Success (parsedValue, _, _) ->
                // output.WriteLine "PARSING SUCCESS"
                parsedValue = expectedValue
            | Failure (errorMsg, _, _) ->
                output.WriteLine $"PARSING ERROR: {errorMsg}"
                false

        transactionIsParsedCorrectly
        |> Prop.forAll arbTxInfo
        |> Check.QuickThrowOnFailure

    // todo 45: remove these tests once we have the property test
    [<Fact>]
    member this.``parsing transaction info with description and comment``() =
        let text =
            "2022/01/06 *description ; comment"

        let expectedValue =
            { Date = DateTime(2022, 1, 6)
              Status = TransactionStatus.Cleared
              Description = Some "description"
              Payee = None
              Note = None
              Comment = Some "comment" }

        testParser pTxFirstLine output text expectedValue

    [<Fact>]
    member this.``parsing transaction info with payee``() =
        let text =
            "2022/01/06 *description |Third Wind ; comment"

        let expectedValue =
            { Date = DateTime(2022, 1, 6)
              Status = TransactionStatus.Cleared
              Description = Some "description"
              Payee = Some "Third Wind"
              Note = None
              Comment = Some "comment" }

        testParser pTxFirstLine output text expectedValue

    [<Fact>]
    member this.``parsing transaction info with payee and note``() =
        let text =
            "2022/01/06 *description |Third Wind | this is a note ; comment"

        let expectedValue =
            { Date = DateTime(2022, 1, 6)
              Status = TransactionStatus.Cleared
              Description = Some "description"
              Payee = Some "Third Wind"
              Note = Some "this is a note"
              Comment = Some "comment" }

        testParser pTxFirstLine output text expectedValue
