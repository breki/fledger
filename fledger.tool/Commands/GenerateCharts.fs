module fledger.tool.Commands.GenerateCharts

open System
open System.CommandLine

open System.CommandLine.Invocation
open System.Globalization
open System.IO

open FParsec
open Newtonsoft.Json
open Thoth.Json.Net
open fledger.BalanceTypes
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingJournal
open fledger.AccountingFuncs
open fledger.Ledger


let journalFileArgument =
    Argument<string>(
        name = "journal file",
        description = "Path to the journal file",
        getDefaultValue = fun _ -> "D:\ledger\igor.ledger"
    )
        .LegalFilePathsOnly()

let outputDirOption =
    Option<string>(
        aliases = [| "--output-dir"; "-o" |],
        description = "Path to the output directory",
        getDefaultValue = fun _ -> "output"
    )
        .LegalFilePathsOnly()

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


let incomeAndExpensesJson ledger =
    let eur = "EUR"

    let income =
        dailyIncome ledger
        |> fullDatesBalanceHistory
        |> toSingleCommodityBalanceHistory ledger.MarketPrices eur
        |> List.map (fun (date, amount) -> date, amount * -30)
        |> commodityBalanceHistoryMovingAverage 90
        |> List.filter (fun (date, _) -> date >= DateTime(2019, 1, 1))


    let encodeDayBalance ((date, amount): CommodityBalanceOnDate) =
        Encode.object
            [ "d",
              Encode.string (
                  date.ToString("yyyy-MM-dd", DateTimeFormatInfo.InvariantInfo)
              )
              "v", amount.Value |> Math.Round |> int |> Encode.int ]

    let json =
        income |> List.map encodeDayBalance |> Encode.list

    json.ToString(formatting = Formatting.None)

let generateHtml templateDir htmlTemplateFileName json outputDir =
    let htmlTemplateFile =
        Path.Combine(templateDir, htmlTemplateFileName)

    let htmlTemplateBody =
        File.ReadAllText(htmlTemplateFile)

    let htmlBody =
        htmlTemplateBody.Replace("[placeholder_data]", json)

    if not (Directory.Exists(outputDir)) then
        Directory.CreateDirectory(outputDir) |> ignore

    let fullHtmlFileName =
        Path.Combine(outputDir, htmlTemplateFileName)

    File.WriteAllText(fullHtmlFileName, htmlBody)

    printfn $"Chart saved to '%s{fullHtmlFileName}'."


type GenerateChartsCommandHandler
    (
        journalFileArgument,
        outputDirOption: Option<string>
    ) =
    member this.journalFileArgument =
        journalFileArgument

    member this.outputDirOption = outputDirOption

    interface ICommandHandler with
        member this.Invoke _ = raise (NotSupportedException "Invoke")

        member this.InvokeAsync context =
            task {
                let journalFile =
                    context.ParseResult.GetValueForArgument<string>(
                        this.journalFileArgument
                    )

                let outputDir =
                    context.ParseResult.GetValueForOption<string>(
                        this.outputDirOption
                    )

                let text = File.ReadAllText(journalFile)

                let result =
                    runParserOnString
                        pJournal
                        { Something = 0 }
                        "test stream"
                        text

                match result with
                | Success (journal, _, _) ->
                    let ledger = fillLedger journal

                    let strExeFilePath =
                        System
                            .Reflection
                            .Assembly
                            .GetExecutingAssembly()
                            .Location

                    let strWorkPath =
                        Path.GetDirectoryName(strExeFilePath)

                    let templateDir =
                        Path.Combine(strWorkPath, "charts")

                    generateHtml
                        templateDir
                        "total-balance.html"
                        (totalBalanceJson ledger)
                        outputDir

                    generateHtml
                        templateDir
                        "income-expenses.html"
                        (incomeAndExpensesJson ledger)
                        outputDir

                    return 0
                | Failure (errorMsg, _, _) ->
                    printfn $"PARSING ERROR: {errorMsg}"
                    return 1
            }

let generateChartsCommand () : Command =
    let cmd =
        Command("generate-charts", "Generate charts from the ledger")

    cmd.AddArgument(journalFileArgument)
    cmd.AddOption(outputDirOption)

    cmd.Handler <-
        GenerateChartsCommandHandler(journalFileArgument, outputDirOption)

    cmd
