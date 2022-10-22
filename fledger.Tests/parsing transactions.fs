module fledger.``parsing transactions``

open System.Linq
open fledger.Journal
open Text

open System
open Xunit
open FsCheck

open FParsec
open Xunit.Abstractions

// todo 10: add some random whitespace (empty or whitespace-only lines)
// between transactions
// todo 20: support for payee and note (pipe characters)

let chooseFromRandomJournal () =
    gen {
        let! dateFormat = Arb.from<bool>.Generator
        let! hasStatus = Arb.from<bool>.Generator
        let! hasDescription = Arb.from<bool>.Generator
        let! hasComment = Arb.from<bool>.Generator
        let! txCount = Gen.choose (0, 3)

        let txString =
            buildString ()
            |> ifDo dateFormat (fun x -> x |> append "2022/01/06")
            |> ifDont dateFormat (fun x -> x |> append "2022-01-06")
            |> ifDo hasStatus (fun x -> x |> append " *")
            |> ifDo hasDescription (fun x -> x |> append "s.p. prispevki ")
            |> ifDo hasComment (fun x -> x |> append "; this is a comment ")
            |> append
                @"
  expenses:Business:Service charges    0.39 EUR
 expenses:Business:Employment Costs    4.25  @@ 12.20 USD
 
  assets:current assets:Sparkasse    -4.64 EUR  = 132.55 EUR
"
            |> toString

        // construct journalString by appending txString txCount times
        let journalString =
            String.Concat(Enumerable.Repeat(txString, txCount))

        let expectedTransaction =
            { Info =
                { Date = DateTime(2022, 1, 6)
                  Status =
                    if hasStatus then
                        TransactionStatus.Cleared
                    else
                        TransactionStatus.Unmarked
                  Description =
                    if hasDescription then
                        Some "s.p. prispevki"
                    else
                        None
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

        let expectedJournal =
            { Transactions =
                Enumerable.Repeat(expectedTransaction, txCount)
                |> Seq.toList }

        let result =
            run JournalParsing.pJournal journalString

        return journalString, expectedJournal, result
    }

type LedgerParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing transactions``() =
        let arbJournal =
            chooseFromRandomJournal () |> Arb.fromGen

        let transactionIsParsedCorrectly
            (
                _,
                expectedTransaction,
                parserResult
            ) =
            match parserResult with
            | Success (parsedTransaction, _, _) ->
                output.WriteLine "PARSING SUCCESS"
                parsedTransaction = expectedTransaction
            | Failure (errorMsg, _, _) ->
                output.WriteLine $"PARSING ERROR: {errorMsg}"
                false

        transactionIsParsedCorrectly
        |> Prop.forAll arbJournal
        |> Check.QuickThrowOnFailure
