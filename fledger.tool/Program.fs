open System
open System.CommandLine

open System.CommandLine.Invocation
open System.Globalization
open System.IO

open System.Reflection
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
        // skip the situation before 2019 since it was not fully accounted for
        |> List.filter (fun (date, _) -> date >= DateTime(2019, 1, 1))

    let encodeDayBalance ((date, amount): CommodityBalanceOnDate) =
        Encode.object
            [ "d",
              Encode.string (
                  date.ToString("yyyy-MM-dd", DateTimeFormatInfo.InvariantInfo)
              )
              "v", amount.Value |> Math.Round |> int |> Encode.int ]

    let json =
        balanceHistory
        |> List.map encodeDayBalance
        |> Encode.list

    json.ToString(formatting = Formatting.None)

type GenerateChartsCommandHandler() =
    interface ICommandHandler with
        member this.Invoke _ = raise (NotSupportedException "Invoke")

        member this.InvokeAsync _ =
            task {
                let text =
                    File.ReadAllText(@"D:\ledger\igor.ledger")

                let result =
                    runParserOnString
                        pJournal
                        { Something = 0 }
                        "test stream"
                        text

                match result with
                | Success (journal, _, _) ->
                    let ledger = fillLedger journal
                    let json = totalBalanceJson ledger

                    let strExeFilePath =
                        System
                            .Reflection
                            .Assembly
                            .GetExecutingAssembly()
                            .Location

                    let strWorkPath =
                        Path.GetDirectoryName(strExeFilePath)

                    let htmlTemplateFile =
                        Path.Combine(
                            strWorkPath,
                            "charts",
                            "total-balance.html"
                        )

                    let htmlTemplateBody =
                        File.ReadAllText(htmlTemplateFile)

                    let htmlBody =
                        htmlTemplateBody.Replace("[placeholder_data]", json)

                    printfn $"%s{htmlBody}"
                    return 0
                | Failure (errorMsg, _, _) ->
                    printfn $"PARSING ERROR: {errorMsg}"
                    return 1
            }

let generateChartsCommand () : Command =
    let cmd =
        Command("generate-charts", "Generate charts from the ledger")

    cmd.Handler <- GenerateChartsCommandHandler()
    cmd

[<EntryPoint>]
let main args =
    let root = RootCommand()

    root.Description <-
        $"fledger.tool v{Assembly.GetExecutingAssembly().GetName().Version}"

    async {
        let! exitCode = root.InvokeAsync(args) |> Async.AwaitTask
        return exitCode
    }
    |> Async.RunSynchronously
