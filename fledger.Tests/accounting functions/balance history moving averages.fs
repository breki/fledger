﻿module fledger.Tests.accounting_functions.balance_history_moving_averages

open System
open Xunit
open FsCheck
open Swensen.Unquote
open Xunit.Abstractions
open fledger.AccountingFuncs
open fledger.BalanceTypes
open fledger.Ledger

let baseDate = DateTime(2022, 11, 12)

let balanceHistoryWith amounts : BalanceHistory =
    amounts
    |> List.mapi (fun i amount ->
        let date = baseDate.AddDays i

        (date,
         [ { Value = amount |> decimal
             Commodity = "EUR" } ]
         |> MultiCommodityBalance.FromAmounts))

let moveByDays (days: int) (balanceHistory: BalanceHistory) =
    balanceHistory
    |> List.map (fun (date, amounts) -> (date.AddDays days, amounts))

[<Fact>]
let ``empty balance history`` () =
    let balanceHistory = []

    let movingAverages =
        balanceHistory |> balanceHistoryMovingAverage 5

    test <@ movingAverages = [] @>

[<Fact>]
let ``balance history shorter than averaged days`` () =
    let balanceHistory =
        balanceHistoryWith [ 50; 100 ]

    let movingAverages =
        balanceHistory |> balanceHistoryMovingAverage 5

    test <@ movingAverages = [] @>

[<Fact>]
let ``balance history with single resulting balance`` () =
    let balanceHistory =
        balanceHistoryWith [ 50; 100; 0; -100; -50 ]

    let movingAverageDays =
        balanceHistory |> List.length

    let expectedMovingAverages =
        balanceHistoryWith [ 0 ]
        |> moveByDays (movingAverageDays / 2)

    let movingAverages =
        balanceHistory
        |> balanceHistoryMovingAverage movingAverageDays

    test <@ movingAverages = expectedMovingAverages @>

[<Fact>]
let ``balance history with two resulting balances`` () =
    let balanceHistory =
        balanceHistoryWith [ 50; 100; 0; -100 ]

    let movingAverageDays = 3

    let expectedMovingAverages =
        balanceHistoryWith [ 50; 0 ]
        |> moveByDays (movingAverageDays / 2)

    let movingAverages =
        balanceHistory
        |> balanceHistoryMovingAverage movingAverageDays

    test <@ movingAverages = expectedMovingAverages @>

[<Fact>]
let ``balance history with three resulting balances`` () =
    let balanceHistory =
        balanceHistoryWith [ 50; 100; 0; -100; 40 ]

    let movingAverageDays = 3

    let expectedMovingAverages =
        balanceHistoryWith [ 50; 0; -20 ]
        |> moveByDays (movingAverageDays / 2)

    let movingAverages =
        balanceHistory
        |> balanceHistoryMovingAverage movingAverageDays

    test <@ movingAverages = expectedMovingAverages @>

[<Fact>]
let ``balance history with even number of moving average days`` () =
    let balanceHistory =
        balanceHistoryWith [ 100; 100; 0; -100; 40 ]

    let movingAverageDays = 4

    let expectedMovingAverages =
        balanceHistoryWith [ 25; 10 ] |> moveByDays 2

    let movingAverages =
        balanceHistory
        |> balanceHistoryMovingAverage movingAverageDays

    test <@ movingAverages = expectedMovingAverages @>

type MovingAveragesCalculation =
    { BalanceHistory: BalanceHistory
      MovingAverageDays: int
      ExpectedMovingAverages: Result<BalanceHistory, string> }

// todo 10: implement property tests for moving averages
let randomBalanceHistoryMovingAveragesCalculation () =
    gen {
        let maxDaysSpan = 100

        let! permutatedAllDaysIndex = Array.init maxDaysSpan id |> Gen.shuffle

        let! permutatedSelectedDaysIndex =
            permutatedAllDaysIndex
            |> Gen.subListOf
            |> Gen.map List.sort

        let! randomAmounts =
            Gen.choose (-1000, 1000)
            |> Gen.listOfLength (permutatedSelectedDaysIndex |> Seq.length)

        let! movingAverageDays = Gen.choose (-5, maxDaysSpan)

        let balanceHistorySparse =
            List.zip permutatedSelectedDaysIndex randomAmounts
            |> List.map (fun (date, amount) ->
                (baseDate.AddDays date,
                 [ amount |> decimal |> Amount.Of "EUR" ]
                 |> MultiCommodityBalance.FromAmounts))

        let balanceHistory =
            fullDatesBalanceHistory balanceHistorySparse

        return
            try
                let movingAverages =
                    balanceHistory
                    |> balanceHistoryMovingAverage movingAverageDays

                { BalanceHistory = balanceHistory
                  MovingAverageDays = movingAverageDays
                  ExpectedMovingAverages = Ok movingAverages }
            with ex ->
                { BalanceHistory = balanceHistory
                  MovingAverageDays = movingAverageDays
                  ExpectedMovingAverages = Error ex.Message }
    }

type CalculatingBalanceHistoryMovingAveragesTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``generating moving averages for the balance history``() =
        let arbCalculation =
            randomBalanceHistoryMovingAveragesCalculation ()
            |> Arb.fromGen

        let datesAreSorted calculation = true

        datesAreSorted
        |> Prop.forAll arbCalculation
        |> Check.QuickThrowOnFailure
