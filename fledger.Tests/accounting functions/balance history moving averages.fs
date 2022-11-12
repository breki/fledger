module fledger.Tests.accounting_functions.balance_history_moving_averages

open System
open Xunit
open Swensen.Unquote
open fledger.AccountingFuncs
open fledger.Ledger

let testBalanceHistory days : BalanceHistory =
    let baseDate = DateTime(2022, 11, 12)

    [| for i in 1..days -> i - 1 |]
    |> Array.map (fun i ->
        let date = baseDate.AddDays(float i)

        let amount =
            match i % 3 with
            | 0 -> 100
            | 1 -> 0
            | _ -> -100

        (date, amount))
    |> Array.map (fun (date, amount) ->
        (date,
         [ ("EUR",
            { Value = amount |> decimal
              Commodity = "EUR" }) ]
         |> Map.ofList))
    |> Array.toList

[<Fact>]
let ``empty balance history`` () =
    let balanceHistory = []

    let movingAverages =
        balanceHistory |> balanceHistoryMovingAverage 5

    test <@ movingAverages = [] @>

[<Fact>]
let ``balance history shorter than averaged days`` () =
    let balanceHistory = testBalanceHistory 2

    let movingAverages =
        balanceHistory |> balanceHistoryMovingAverage 5

    test <@ movingAverages = [] @>
