﻿module fledger.BalanceTypes

open fledger.BasicTypes
open fledger.LedgerTypes

/// Represents a dictionary of balances for one or more commodities.
type MultiCommodityBalance =
    { Commodities: Map<Commodity, Amount> }

    static member FromCommodities(commodities: Map<Commodity, Amount>) =
        { Commodities = commodities }

    /// Constructs the balances of commodities from a sequence of amounts.
    /// The same commodity can appear multiple times in the sequence - the
    /// method will sum the amounts of the same commodity.
    static member FromAmounts(amounts: Amount seq) : MultiCommodityBalance =
        // calculate balances for each commodity from the sequence of amounts
        let balances =
            amounts
            |> Seq.fold
                (fun (balances: Map<Commodity, Amount>) amount ->
                    let commodity = amount.Commodity

                    let newBalances =
                        match balances.TryGetValue commodity with
                        | true, balance ->
                            balances.Add(commodity, balance + amount)
                        | false, _ -> balances.Add(commodity, amount)

                    newBalances)
                Map.empty

        { Commodities = balances }

    static member Empty = MultiCommodityBalance.FromCommodities Map.empty

    static member AddAmount (amount: Amount) balance =
        let commodity = amount.Commodity

        let newBalance =
            match balance.Commodities.TryGetValue commodity with
            | true, balance -> balance + amount
            | false, _ -> amount

        { balance with
            Commodities = balance.Commodities.Add(commodity, newBalance) }

    member this.AddCommodity commodity balance =
        let newCommodities = this.Commodities.Add(commodity, balance)

        MultiCommodityBalance.FromCommodities newCommodities

    /// Returns the balance of only the commodities that match the predicate.
    member this.Filter predicate =
        let newCommodities = this.Commodities |> Map.filter predicate

        MultiCommodityBalance.FromCommodities newCommodities

    static member (+)
        (
            a: MultiCommodityBalance,
            b: MultiCommodityBalance
        ) : MultiCommodityBalance =
        Map.union
            (fun amount1 amount2 -> amount1 + amount2)
            a.Commodities
            b.Commodities
        |> MultiCommodityBalance.FromCommodities


let divideMultiCommodityBalance
    (divisor: int)
    (balance: MultiCommodityBalance)
    : MultiCommodityBalance =
    balance.Commodities
    |> Map.map (fun _ amount -> amount / divisor)
    |> MultiCommodityBalance.FromCommodities

/// Convert all commodities from the balance to the single commodity using
/// provided market prices for the given date.
let convertToSingleCommodity
    (marketPrices: MarketPrices)
    (toCommodity: Commodity)
    (date: Date)
    (balance: MultiCommodityBalance)
    : Amount =
    let totalSumInTargetCommodity =
        balance.Commodities
        |> Map.toArray
        |> Array.map (fun (_, amount) ->
            marketPrices.Convert amount toCommodity date)
        |> Array.sumBy (fun amount -> amount.Value)

    { Value = totalSumInTargetCommodity
      Commodity = toCommodity }


/// Represents a balance for a specific account.
type AccountBalance =
    { Account: AccountRef
      Balance: MultiCommodityBalance }

/// Represents a dictionary of accounts and their balances.
type AccountsBalances =
    { Balances: Map<AccountRef, AccountBalance> }

    static member Empty = { Balances = Map.empty }

/// Represents a map of multi-commodity balances, indexed by the date.
type BalanceByDate = Map<Date, MultiCommodityBalance>

type BalanceOnDate = Date * MultiCommodityBalance

/// Represents a list of multi-commodity balances, one for each date.
type BalanceHistory = List<BalanceOnDate>

/// A tuple of a date and an amount.
type CommodityBalanceOnDate = Date * Amount
/// A list of single-commodity balances, one for each date.
type CommodityBalanceHistory = List<CommodityBalanceOnDate>
