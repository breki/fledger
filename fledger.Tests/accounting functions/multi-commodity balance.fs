module fledger.Tests.accounting_functions.multi_commodity_balance

open Xunit
open Swensen.Unquote
open fledger.BalanceTypes
open fledger.Ledger

[<Fact>]
let ``can be constructed from a sequence of amounts with repeating commodities``
    ()
    =
    let amounts =
        seq {
            { Value = 10m; Commodity = "EUR" }
            { Value = 5.5m; Commodity = "USD" }
            { Value = 5m; Commodity = "EUR" }
        }

    let balance = MultiCommodityBalance.FromAmounts amounts

    test <@ balance.Commodities["USD"] = { Value = 5.5m; Commodity = "USD" } @>
    test <@ balance.Commodities["EUR"] = { Value = 15m; Commodity = "EUR" } @>
