module fledger.Tests.accounting_functions.unbalanced_transaction

open fledger.BalanceTypes
open fledger.Tests.LedgerBuilders

open Xunit
open Swensen.Unquote

[<Fact>]
let ``balancing a transaction takes into account total price`` () =
    let posting1 = toAccount "acc1" |> amountCommodityOf 10m "EUR"

    let posting2 =
        toAccount "acc1"
        |> amountCommodityOf -8m "USD"
        |> withTotalPrice 10m "EUR"

    let tx = withTransaction () |> addPosting posting1 |> addPosting posting2

    let unbalancedCommodities = tx |> unbalancedTxCommodities

    test <@ unbalancedCommodities.Commodities |> Map.count = 0 @>
