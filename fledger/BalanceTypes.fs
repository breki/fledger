module fledger.BalanceTypes

open fledger.BasicTypes
open fledger.Ledger

// todo 7: expose a proper MultiCommodityBalance with operators
//  (just like Amount)
type MultiCommodityBalance = Map<Commodity, Amount>

let addMultiCommodityBalances
    (a: MultiCommodityBalance)
    (b: MultiCommodityBalance)
    : MultiCommodityBalance =
    Map.union (fun amount1 amount2 -> amount1 + amount2) a b

let divideMultiCommodityBalance
    (divisor: int)
    (balance: MultiCommodityBalance)
    : MultiCommodityBalance =
    balance
    |> Map.map (fun _ amount -> amount / divisor)

/// Convert all commodities from the balance to the single commodity using
/// provided market prices for the given date.
let convertToSingleCommodity
    (marketPrices: MarketPrices)
    (toCommodity: Commodity)
    (date: Date)
    (balance: MultiCommodityBalance)
    : Amount =
    let totalSumInTargetCommodity =
        balance
        |> Map.toArray
        |> Array.map (fun (_, amount) ->
            marketPrices.Convert amount toCommodity date)
        |> Array.sumBy (fun amount -> amount.Value)

    { Value = totalSumInTargetCommodity
      Commodity = toCommodity }


type AccountBalance =
    { Account: AccountRef
      Balance: MultiCommodityBalance }

type AccountsBalances =
    { Balances: Map<AccountRef, AccountBalance> }

/// Represents a map of multi-commodity balances, indexed by the date.
type BalanceByDate = Map<Date, MultiCommodityBalance>

type BalanceOnDate = Date * MultiCommodityBalance

/// Represents a list of multi-commodity balances, one for each date.
type BalanceHistory = List<BalanceOnDate>

/// A tuple of a date and an amount.
type CommodityBalanceOnDate = Date * Amount
/// A list of single-commodity balances, one for each date.
type CommodityBalanceHistory = List<CommodityBalanceOnDate>
