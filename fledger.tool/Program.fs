open System.Globalization
open System.IO
open FParsec
open Newtonsoft.Json
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
            [ "d",
              Encode.string (
                  date.ToString("yyyy-MM-dd", DateTimeFormatInfo.InvariantInfo)
              )
              "v",
              amount.Value
              |> System.Math.Round
              |> int
              |> Encode.int ]

    let json =
        balanceHistory
        |> List.map encodeDayBalance
        |> Encode.list

    json.ToString(formatting = Formatting.None)

[<EntryPoint>]
let main args =
    let text =
        File.ReadAllText(@"D:\ledger\igor.ledger")

    let result =
        runParserOnString pJournal { Something = 0 } "test stream" text

    match result with
    | Success (journal, _, _) ->
        let ledger = fillLedger journal
        let json = totalBalanceJson ledger

        let strExeFilePath =
            System.Reflection.Assembly.GetExecutingAssembly().Location

        let strWorkPath =
            Path.GetDirectoryName(strExeFilePath)

        let htmlTemplateFile =
            Path.Combine(strWorkPath, "charts", "total-balance.html")

        let htmlTemplateBody =
            File.ReadAllText(htmlTemplateFile)

        let htmlBody =
            htmlTemplateBody.Replace("[placeholder_data]", json)

        printfn $"%s{htmlBody}"
        0
    | Failure (errorMsg, _, _) ->
        printfn $"PARSING ERROR: {errorMsg}"
        1
