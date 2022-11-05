open System.Globalization
open System.IO
open FParsec
open Thoth.Json.Net
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingJournal
open fledger.AccountingFuncs
open fledger.Ledger

let totalBalanceJson ledger =
    let eur = "EUR"

    let balanceHistory =
        totalBalanceChangeHistory ledger
        |> absoluteTotalBalanceHistory
        |> toSingleCommodityBalanceHistory ledger.MarketPrices eur

    let encodeDayBalance ((date, amount): CommodityBalanceOnDate) =
        Encode.object
            [ "date",
              Encode.string (
                  date.ToString("yyyy-MM-dd", DateTimeFormatInfo.InvariantInfo)
              )
              "amount",
              amount.Value
              |> System.Math.Round
              |> int
              |> Encode.int ]

    let json =
        Encode.object
            [ "balanceHistory",
              balanceHistory
              |> List.map encodeDayBalance
              |> Encode.list ]

    json.ToString()

[<EntryPoint>]
let main args =
    let text =
        File.ReadAllText(@"D:\ledger\igor.ledger")

    let result =
        runParserOnString pJournal { Something = 0 } "test stream" text

    match result with
    | Success (journal, _, _) ->
        let ledger = fillLedger journal
        let balanceHistory = totalBalanceJson ledger
        printfn $"%s{balanceHistory}"
        0
    | Failure (errorMsg, _, _) ->
        printfn $"PARSING ERROR: {errorMsg}"
        1
