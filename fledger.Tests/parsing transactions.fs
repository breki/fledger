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

        return
            buildString ()
            |> ifDo dateFormat (fun x -> x |> append "2022/01/06")
            |> ifDont dateFormat (fun x -> x |> append "2022-01-06")
            |> append
                @" *s.p. prispevki;this is a comment
  expenses:Business:Service charges    0.39 EUR
  expenses:Business:Employment Costs    4.25  @@ 12.20 USD
  assets:current assets:Sparkasse    -4.64 EUR  = 132.55 EUR
"
            |> toString
    }

type LedgerParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing sample transaction``() =
        let expectedTransaction =
            { Info =
                { Date = DateTime(2022, 1, 6)
                  Status = TransactionStatus.Cleared
                  Description = Some "s.p. prispevki"
                  Comment = Some "this is a comment" }
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

        let randomTransaction =
            chooseFromRandomTransaction () |> Arb.fromGen

        let transactionIsParsedCorrectly transactionString =
            let result: ParserResult<Transaction, unit> =
                run JournalParsing.pTx transactionString

            match result with
            | Success (tx, _, _) -> tx = expectedTransaction
            // todo 20: how to record the error so it is visible in test
            //   results?
            | Failure (errorMsg, error, _) ->
                output.WriteLine errorMsg
                false

        transactionIsParsedCorrectly
        |> Prop.forAll randomTransaction
        |> Check.QuickThrowOnFailure

//            match result with
//            | Success (tx, _, _) -> test <@ tx = expectedTransaction @>
//            | Failure (err, _, _) ->
//                output.WriteLine $"%s{err}\n"
//                test <@ false @>
