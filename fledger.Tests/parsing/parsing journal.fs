module fledger.``parsing journal``

open System.Linq
open fledger.BasicTypes
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingJournal
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

P 2018-11-01 USD 0.877 EUR
                
# this is a comment
account assets:current assets:NLB
  note  (type: BANK)

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
            [ (1L,
               { Value = 1000.00m
                 Commodity = Some "EUR" }
               |> DefaultCommodity)
              (3L, Commodity("EUR"))
              (4L, Commodity("USD"))
              (6L,
               MarketPrice(
                   { Date = DateTime(2018, 11, 1)
                     Commodity = "USD"
                     Price =
                       { Value = 0.877m
                         Commodity = Some "EUR" } }
               ))
              (8L, Comment(" this is a comment"))
              (9L,
               Account(
                   { Account = "assets:current assets:NLB" |> AccountRef.Create
                     Subdirectives = [ "note  (type: BANK)" ] }
               )) ]

        let expectedTransaction =
            { Info =
                { Date = DateTime(2022, 1, 6)
                  Status = TransactionStatus.Cleared
                  Description = Some "s.p. prispevki"
                  Payee = None
                  Note = None
                  Comment = Some "this is a comment" }
              Postings =
                [ { Account =
                      "expenses:Business:Service charges" |> AccountRef.Create
                    Amount =
                      { Amount =
                          { Value = 0.39m
                            Commodity = Some "EUR" }
                        TotalPrice = None }
                      |> Some
                    ExpectedBalance = None }
                  { Account =
                      "expenses:Business:Employment Costs" |> AccountRef.Create
                    Amount =
                      { Amount = { Value = 4.25m; Commodity = None }
                        TotalPrice =
                          Some
                              { Value = 12.2m
                                Commodity = Some "USD" } }
                      |> Some
                    ExpectedBalance = None }
                  { Account =
                      "assets:current assets:Sparkasse" |> AccountRef.Create
                    Amount =
                      { Amount =
                          { Value = -4.64m
                            Commodity = Some "EUR" }
                        TotalPrice = None }
                      |> Some
                    ExpectedBalance =
                      Some
                          { Value = 132.55m
                            Commodity = Some "EUR" } } ] }

        let expectedTransactions =
            List.init txCount (fun i ->
                (12L
                 + (i |> int64)
                   * (4L + (if hasEmptyLinesBetweenTxs then 3L else 0L)),
                 Transaction expectedTransaction))

        let expectedJournal =
            { Items = expectedStartingItems @ expectedTransactions }

        let userState = { Something = 0 }

        let result, ex =
            try
                let result =
                    runParserOnString
                        pJournal
                        userState
                        "test stream"
                        journalString

                Some result, None
            with ex ->
                None, Some ex

        return journalString, expectedJournal, result, ex
    }

type JournalParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing journal``() =
        let arbJournal = chooseFromRandomJournal () |> Arb.fromGen

        let journalIsParsedCorrectly
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

        journalIsParsedCorrectly
        |> Prop.forAll arbJournal
        |> Check.QuickThrowOnFailure
