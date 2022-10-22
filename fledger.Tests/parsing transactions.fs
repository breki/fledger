module fledger.``parsing transactions``

open fledger.Journal
open Text

open System
open Xunit
open FsCheck

open FParsec
open Xunit.Abstractions


let chooseFromRandomTransaction () =
    gen {
        let! dateFormat = Arb.from<bool>.Generator
        let! hasComment = Arb.from<bool>.Generator

        let txString =
            buildString ()
            |> ifDo dateFormat (fun x -> x |> append "2022/01/06")
            |> ifDont dateFormat (fun x -> x |> append "2022-01-06")
            |> append @" *s.p. prispevki"
            |> ifDo hasComment (fun x -> x |> append ";this is a comment")
            |> append
                @"
  expenses:Business:Service charges    0.39 EUR
  expenses:Business:Employment Costs    4.25  @@ 12.20 USD
  assets:current assets:Sparkasse    -4.64 EUR  = 132.55 EUR
"
            |> toString

        let expectedTransaction =
            { Info =
                { Date = DateTime(2022, 1, 6)
                  Status = TransactionStatus.Cleared
                  Description = Some "s.p. prispevki"
                  Comment =
                    if hasComment then
                        Some "this is a comment"
                    else
                        None }
              Postings =
                [ { Account = "expenses:Business:Service charges"
                    Amount = { Value = 0.39m; Currency = "EUR" }
                    TotalPrice = None
                    ExpectedBalance = None }
                  { Account = "expenses:Business:Employment Costs"
                    Amount = { Value = 4.25m; Currency = "EUR" }
                    TotalPrice = Some { Value = 12.2m; Currency = "USD" }
                    ExpectedBalance = None }
                  { Account = "assets:current assets:Sparkasse"
                    Amount = { Value = -4.64m; Currency = "EUR" }
                    TotalPrice = None
                    ExpectedBalance = Some { Value = 132.55m; Currency = "EUR" } } ] }

        let result: ParserResult<Transaction, unit> =
            run JournalParsing.pTx txString

        return txString, expectedTransaction, result
    }

type LedgerParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing transactions``() =
        let arbTransaction =
            chooseFromRandomTransaction () |> Arb.fromGen

        let transactionIsParsedCorrectly
            (
                _,
                expectedTransaction,
                parserResult
            ) =
            // todo 5: how to output the reason why the transaction
            // is not as expected? - do we include the parsed transaction
            // in the arbitrary itself?
            match parserResult with
            | Success (parsedTransaction, _, _) ->
                output.WriteLine "PARSING SUCCESS"
                parsedTransaction = expectedTransaction
            | Failure (errorMsg, _, _) ->
                output.WriteLine $"PARSING ERROR: {errorMsg}"
                false

        transactionIsParsedCorrectly
        |> Prop.forAll arbTransaction
        |> Check.QuickThrowOnFailure
