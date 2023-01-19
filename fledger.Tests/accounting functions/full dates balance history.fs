module fledger.Tests.accounting_functions.full_dates_balance_history



open System
open Xunit
open Swensen.Unquote
open fledger.AccountingFuncs
open fledger.BalanceTypes
open fledger.LedgerTypes

let someDate = DateTime(2018, 1, 1)

let someBalance1 =
    [ { Value = 10m; Commodity = "EUR" } ] |> MultiCommodityBalance.FromAmounts

let someBalance2 =
    [ { Value = 30m; Commodity = "EUR" } ] |> MultiCommodityBalance.FromAmounts

let someBalance3 =
    [ { Value = 20m; Commodity = "EUR" } ] |> MultiCommodityBalance.FromAmounts

[<Fact>]
let ``empty balance history`` () =
    let sparseBalance = []

    let fullBalance = fullDatesBalanceHistory sparseBalance

    test <@ fullBalance = List.Empty @>

[<Fact>]
let ``single day balance history`` () =
    let sparseBalance = [ (someDate, someBalance1) ]

    let fullBalance = fullDatesBalanceHistory sparseBalance

    test <@ fullBalance = sparseBalance @>

[<Fact>]
let ``no days between`` () =
    let sparseBalance =
        [ (someDate, someBalance1); (someDate.AddDays 1, someBalance2) ]

    let fullBalance = fullDatesBalanceHistory sparseBalance

    test <@ fullBalance = sparseBalance @>

[<Fact>]
let ``several days between`` () =
    let sparseBalance =
        [ (someDate, someBalance1); (someDate.AddDays 4, someBalance2) ]

    let fullBalance = fullDatesBalanceHistory sparseBalance

    let expectedFullBalance =
        [ (someDate, someBalance1)
          (someDate.AddDays 1, someBalance1)
          (someDate.AddDays 2, someBalance1)
          (someDate.AddDays 3, someBalance1)
          (someDate.AddDays 4, someBalance2) ]

    test <@ fullBalance = expectedFullBalance @>

[<Fact>]
let ``three balance dates`` () =
    let sparseBalance =
        [ (someDate, someBalance1)
          (someDate.AddDays 4, someBalance2)
          (someDate.AddDays 6, someBalance3) ]

    let fullBalance = fullDatesBalanceHistory sparseBalance

    let expectedFullBalance =
        [ (someDate, someBalance1)
          (someDate.AddDays 1, someBalance1)
          (someDate.AddDays 2, someBalance1)
          (someDate.AddDays 3, someBalance1)
          (someDate.AddDays 4, someBalance2)
          (someDate.AddDays 5, someBalance2)
          (someDate.AddDays 6, someBalance3) ]

    test <@ fullBalance = expectedFullBalance @>
