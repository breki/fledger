module fledger.``parsing realistic journal``


open System
open Xunit
open System.IO
open FsCheck
open FParsec

open Xunit.Abstractions

open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingJournal
open fledger.LedgerTypes
open fledger.LedgerFilling
open fledger.BalanceTypes
open fledger.AccountingFuncs

open Swensen.Unquote

type RealisticJournalParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    // [<Fact(Skip = "Only to be run to debug problems "
    //               + "with parsing realistic journals")>]
    member this.``parsing realistic journal``() =
        let text = File.ReadAllText(@"D:\ledger\igor.ledger")

        let result =
            runParserOnString pJournal { Something = 0 } "test stream" text

        match result with
        | Success (journal, _, _) ->
            // output.WriteLine $"PARSING SUCCESS: {journal}"

            match fillLedger journal with
            | Result.Error error ->
                output.WriteLine $"LEDGER ERROR: {error}"
                test <@ false @>
            | Result.Ok ledger ->
                test <@ ledger.Accounts.Count > 20 @>
                test <@ ledger.Transactions.Length > 1000 @>

                let _accountBalances = accountsBalances ledger

                let totalBalanceHistory =
                    totalBalanceChangeHistory ledger
                    |> absoluteTotalBalanceHistory

                let finalTotalBalance = totalBalanceHistory |> List.last

                let finalDate = fst finalTotalBalance

                let finalTotalBalanceAmounts = snd finalTotalBalance

                let finalTotalBalanceInEur =
                    finalTotalBalanceAmounts
                    |> convertToSingleCommodity
                        ledger.MarketPrices
                        "EUR"
                        finalDate

                output.WriteLine
                    $"Final total balance: {finalTotalBalanceInEur}"

                let wagaTransactions =
                    ledger
                    |> listAccountTransactions
                        "assets:Freelancing outstanding:WagaLabs"
                    |> List.sortBy (fun t -> t.Date)

                wagaTransactions
                |> List.iter (fun t ->
                    output.WriteLine $"{t}{Environment.NewLine}")
        | Failure (errorMsg, _, _) ->
            output.WriteLine $"PARSING ERROR: {errorMsg}"
            test <@ false @>
