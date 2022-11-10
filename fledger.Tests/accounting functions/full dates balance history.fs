module fledger.Tests.accounting_functions.full_dates_balance_history



open System
open Xunit
open Swensen.Unquote
open fledger.AccountingFuncs
open fledger.Ledger

let someDate = DateTime(2018, 1, 1)

let someBalance1 =
    [ ("EUR", { Value = 10m; Commodity = "EUR" }) ]
    |> Map.ofList

let someBalance2 =
    [ ("EUR", { Value = 30m; Commodity = "EUR" }) ]
    |> Map.ofList

[<Fact>]
let ``empty balance history`` () =
    let sparseBalance = []

    let fullBalance =
        fullDatesBalanceHistory sparseBalance

    test <@ fullBalance = List.Empty @>

[<Fact>]
let ``single day balance history`` () =
    let sparseBalance =
        [ (someDate, someBalance1) ]

    let fullBalance =
        fullDatesBalanceHistory sparseBalance

    test <@ fullBalance = sparseBalance @>

[<Fact>]
let ``no days between`` () =
    let sparseBalance =
        [ (someDate, someBalance1)
          (someDate.AddDays 1, someBalance2) ]

    let fullBalance =
        fullDatesBalanceHistory sparseBalance

    test <@ fullBalance = sparseBalance @>
