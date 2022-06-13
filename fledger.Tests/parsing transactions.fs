module fledger.``parsing transactions``

open fledger.Journal

open System
open Xunit
open Swensen.Unquote
open FsCheck

open FParsec
open Xunit.Abstractions

let sample =
    @"2022/01/06 *s.p. prispevki
  expenses:Business:Service charges    0.39 EUR
  expenses:Business:Employment Costs    4.25  @@ 12.20 USD
  assets:current assets:Sparkasse    -4.64 EUR  = 132.55 EUR
"

type LedgerParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing sample transaction``() =
        let expectedTransaction =
            { Info =
                  { Date = DateTime(2022, 1, 6)
                    State = TransactionState.Cleared
                    Description = "s.p. prispevki" }
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
                      ExpectedBalance =
                          Some { Value = 132.55m; Currency = "EUR" } } ] }

        let result = run JournalParsing.pTx sample

        match result with
        | Success (tx, _, _) -> test <@ tx = expectedTransaction @>
        | Failure (err, _, _) ->
            output.WriteLine $"%s{err}\n"
            test <@ false @>
