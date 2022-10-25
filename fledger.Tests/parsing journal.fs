module fledger.``parsing journal``

open System.Linq
open fledger.ParsingBasics
open fledger.Journal
open Text

open System
open Xunit
open FsCheck

open FParsec
open Xunit.Abstractions

let chooseFromRandomJournal () =
    gen {
        let! hasEmptyLinesBetweenTxs = Arb.from<bool>.Generator
        let! txCount = Gen.choose (0, 3)

        let startingJournalStr =
            @"D 1,000.00 EUR
        
commodity EUR
commodity USD
                
"

        let txString =
            buildString ()
            |> append "2022/01/06 *s.p. prispevki ; this is a comment "
            |> append
                @"
  expenses:Business:Service charges    0.39 EUR
 expenses:Business:Employment Costs    4.25  @@ 12.20 USD
 
  assets:current assets:Sparkasse    -4.64 EUR  = 132.55 EUR
"
            |> ifDo hasEmptyLinesBetweenTxs (fun x ->
                x
                |> append
                    $"{Environment.NewLine}     {Environment.NewLine}{Environment.NewLine}")
            |> toString

        // construct journalString by appending txString txCount times
        let journalString =
            startingJournalStr
            + String.Concat(Enumerable.Repeat(txString, txCount))

        let expectedStartingItems =
            [ { Value = 1000.00m
                Currency = Some "EUR" }
              |> DefaultCommodity
              Commodity("EUR")
              Commodity("USD") ]

        let expectedTransaction =
            { Info =
                { Date = DateTime(2022, 1, 6)
                  Status = TransactionStatus.Cleared
                  Description = Some "s.p. prispevki"
                  Payee = None
                  Note = None
                  Comment = Some "this is a comment" }
              Postings =
                [ { Account = "expenses:Business:Service charges"
                    Amount = { Value = 0.39m; Currency = Some "EUR" }
                    TotalPrice = None
                    ExpectedBalance = None }
                  { Account = "expenses:Business:Employment Costs"
                    Amount = { Value = 4.25m; Currency = None }
                    TotalPrice = Some { Value = 12.2m; Currency = Some "USD" }
                    ExpectedBalance = None }
                  { Account = "assets:current assets:Sparkasse"
                    Amount =
                      { Value = -4.64m
                        Currency = Some "EUR" }
                    TotalPrice = None
                    ExpectedBalance =
                      Some
                          { Value = 132.55m
                            Currency = Some "EUR" } } ] }

        let expectedTransactions =
            (Enumerable.Repeat(Transaction expectedTransaction, txCount)
             |> Seq.toList)

        let expectedJournal =
            { Items = expectedStartingItems @ expectedTransactions }

        let userState = { Something = 0 }

        let result =
            runParserOnString
                ParsingJournal.pJournal
                userState
                "test stream"
                journalString

        return journalString, expectedJournal, result
    }

type JournalParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing journal``() =
        let arbJournal =
            chooseFromRandomJournal () |> Arb.fromGen

        let journalIsParsedCorrectly (_, expectedTransaction, parserResult) =
            match parserResult with
            | Success (parsedTransaction, _, _) ->
                output.WriteLine "PARSING SUCCESS"
                parsedTransaction = expectedTransaction
            | Failure (errorMsg, _, _) ->
                output.WriteLine $"PARSING ERROR: {errorMsg}"
                false

        journalIsParsedCorrectly
        |> Prop.forAll arbJournal
        |> Check.QuickThrowOnFailure
