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
open fledger.BasicTypes
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingJournal
open fledger.AccountingFuncs
open fledger.LedgerTypes
open fledger.LedgerFilling

let eur = "EUR"

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


let encodeDayBalance ((date, amount): CommodityBalanceOnDate) =
    Encode.object
        [ "d",
          Encode.string (
              date.ToString("yyyy-MM-dd", DateTimeFormatInfo.InvariantInfo)
          )
          "v", amount.Value |> Math.Round |> int |> Encode.int ]

let encodeCommodityBalanceHistory (history: CommodityBalanceHistory) =
    history |> List.map encodeDayBalance |> Encode.list

let totalBalanceJson ledger =
    let balanceHistory =
        totalBalanceChangeHistory ledger
        |> absoluteTotalBalanceHistory
        |> toSingleCommodityBalanceHistory ledger.MarketPrices eur
        // skip the situation before 2019 since it was not fully accounted for
        |> List.filter (fun (date, _) -> date >= DateTime(2019, 1, 1))

    let json = balanceHistory |> encodeCommodityBalanceHistory

    json.ToString(formatting = Formatting.None)


let incomeAndExpensesJson ledger =
    let income =
        dailyIncome ledger
        |> fullDatesBalanceHistory
        |> toSingleCommodityBalanceHistory ledger.MarketPrices eur
        |> List.map (fun (date, amount) -> date, amount * -30)
        |> commodityBalanceHistoryMovingAverage 90
        |> List.filter (fun (date, _) -> date >= DateTime(2019, 1, 1))

    let expenses =
        dailyExpenses ledger
        |> fullDatesBalanceHistory
        |> toSingleCommodityBalanceHistory ledger.MarketPrices eur
        |> List.map (fun (date, amount) -> date, amount * 30)
        |> commodityBalanceHistoryMovingAverage 90
        |> List.filter (fun (date, _) -> date >= DateTime(2019, 1, 1))

    let allSeriesData = [ income; expenses ]

    let json =
        allSeriesData |> List.map encodeCommodityBalanceHistory |> Encode.list

    json.ToString(formatting = Formatting.None)

// todo XX 15: render stacked area charts for income
// todo XX 16: add series names when generating
// todo XX 17: use series names when rendering
// todo XX 18: automatically adjust the chart to whatever number of series
//   there are - use the pre-defined color palette
let incomeAnalysisJson ledger =
    let incomeRouter (posting: Posting) =
        if posting.Account.FullName.StartsWith("income:Freelancing") then
            Some 0
        elif
            posting.Account.FullName.StartsWith("income:Business:ScalableMaps")
        then
            Some 1
        elif posting.Account.FullName.StartsWith("income:Salary") then
            Some 2
        elif posting.Account.FullName.StartsWith("income:Inheritance") then
            None
        elif posting.Account.FullName.StartsWith("income") then
            Some 3
        else
            None

    let histories =
        ledger
        |> balancesChangeHistories incomeRouter 4
        |> Array.map (fun history ->
            history
            |> fullDatesBalanceHistory
            |> toSingleCommodityBalanceHistory ledger.MarketPrices eur
            |> List.map (fun (date, amount) -> date, amount * -30)
            |> commodityBalanceHistoryMovingAverage 90)

    let json =
        histories |> Array.map encodeCommodityBalanceHistory |> Encode.array

    json.ToString(formatting = Formatting.None)

let generateHtml templateDir htmlTemplateFileName json outputDir =
    let htmlTemplateFile = Path.Combine(templateDir, htmlTemplateFileName)

    let htmlTemplateBody = File.ReadAllText(htmlTemplateFile)

    let htmlBody = htmlTemplateBody.Replace("[placeholder_data]", json)

    if not (Directory.Exists(outputDir)) then
        Directory.CreateDirectory(outputDir) |> ignore

    let fullHtmlFileName = Path.Combine(outputDir, htmlTemplateFileName)

    File.WriteAllText(fullHtmlFileName, htmlBody)

    printfn $"Chart saved to '%s{fullHtmlFileName}'."


type GenerateChartsCommandHandler
    (
        journalFileArgument,
        outputDirOption: Option<string>
    ) =
    member this.journalFileArgument = journalFileArgument

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
                    match fillLedger journal with
                    | Result.Error error ->
                        printfn $"PARSING ERROR: {error}"
                        return 1
                    | Result.Ok ledger ->
                        let strExeFilePath =
                            System
                                .Reflection
                                .Assembly
                                .GetExecutingAssembly()
                                .Location

                        let strWorkPath = Path.GetDirectoryName(strExeFilePath)

                        let templateDir = Path.Combine(strWorkPath, "charts")

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

                        generateHtml
                            templateDir
                            "income-analysis.html"
                            (incomeAnalysisJson ledger)
                            outputDir

                        return 0
                | Failure (errorMsg, _, _) ->
                    printfn $"PARSING ERROR: {errorMsg}"
                    return 1
            }

let generateChartsCommand () : Command =
    let cmd = Command("generate-charts", "Generate charts from the ledger")

    cmd.AddArgument(journalFileArgument)
    cmd.AddOption(outputDirOption)

    cmd.Handler <-
        GenerateChartsCommandHandler(journalFileArgument, outputDirOption)

    cmd
