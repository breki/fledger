module fledger.``parsing tx first line``


open System
open Xunit
open FsCheck
open FParsec
open Text

open Xunit.Abstractions

open fledger.BasicTypes
open fledger.Journal
open fledger.Parsing.ParsingTransactions


type PayeeNoteSituation =
    | NoPayee
    | EmptyPayee
    | Payee
    | PayeeEmptyNote
    | EmptyPayeeNote
    | EmptyPayeeEmptyNote
    | Both

type CommentSituation =
    | NoComment
    | EmptyComment
    | Comment

let chooseArbitraryTxFirstLine () =
    gen {
        let! dateFormat = Arb.from<bool>.Generator
        let! hasWhitespaceAfterDate = Arb.from<bool>.Generator
        let! statusChar = Gen.elements [ "!"; "*"; "" ]
        let! hasDescription = Arb.from<bool>.Generator
        let! payeeNote = Arb.from<PayeeNoteSituation>.Generator
        let! comment = Arb.from<CommentSituation>.Generator

        let mutable txBuilder =
            buildString ()
            |> ifDo dateFormat (fun x -> x |> append "2022/01/06")
            |> ifDont dateFormat (fun x -> x |> append "2022-01-06")
            |> ifDo hasWhitespaceAfterDate (fun x -> x |> append " ")
            |> append statusChar
            |> ifDo hasDescription (fun x -> x |> append "s.p. prispevki ")

        txBuilder <-
            match payeeNote with
            | NoPayee -> txBuilder
            | EmptyPayee -> txBuilder |> append "|"
            | Payee -> txBuilder |> append "| Payee"
            | PayeeEmptyNote -> txBuilder |> append "| Payee |"
            | EmptyPayeeNote -> txBuilder |> append "| | Note"
            | EmptyPayeeEmptyNote -> txBuilder |> append "| |"
            | Both -> txBuilder |> append "| Payee | Note"

        txBuilder <-
            match comment with
            | NoComment -> txBuilder
            | EmptyComment -> txBuilder |> append " ;"
            | Comment -> txBuilder |> append " ; this is a comment"

        let txString = txBuilder |> toString

        let expectedTxInfo =
            { Date = DateTime(2022, 1, 6)
              Status =
                match statusChar with
                | "!" -> TransactionStatus.Pending
                | "*" -> TransactionStatus.Cleared
                | _ -> TransactionStatus.Unmarked
              Description =
                if hasDescription then Some "s.p. prispevki" else None
              Payee =
                match payeeNote with
                | Payee -> Some "Payee"
                | PayeeEmptyNote -> Some "Payee"
                | Both -> Some "Payee"
                | _ -> None
              Note =
                match payeeNote with
                | EmptyPayeeNote -> Some "Note"
                | Both -> Some "Note"
                | _ -> None
              Comment =
                match comment with
                | Comment -> Some "this is a comment"
                | _ -> None }

        let result = runParserOnString pTxFirstLine () "test stream" txString

        return txBuilder, expectedTxInfo, result
    }

type TxFirstLineParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing transaction's first line``() =
        let arbTxInfo = chooseArbitraryTxFirstLine () |> Arb.fromGen

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
