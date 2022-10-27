﻿module fledger.AccountingFuncs

open fledger.BasicTypes
open fledger.Ledger

type MultiCommodityBalance = Map<string, Amount>

type AccountBalance =
    { Account: AccountRef
      Balance: MultiCommodityBalance }

type AccountsBalances =
    { Balances: Map<AccountRef, AccountBalance> }

/// Returns the balances of all the accounts.
let accountsBalances (ledger: Ledger) =
    let processPosting
        (balances: AccountsBalances)
        (posting: Posting)
        : AccountsBalances =
        let account = posting.Account
        let amount = posting.Amount
        let commodity = amount.Commodity

        let accountBalance =
            balances.Balances
            |> Map.tryFind account
            |> Option.defaultValue
                { Account = account
                  Balance = Map.empty }

        let newCommodityBalance =
            accountBalance.Balance
            |> Map.tryFind commodity
            |> Option.defaultValue (commodity |> Amount.Zero)
            |> (+) amount

        let newAccountBalance =
            { accountBalance with
                Balance =
                    accountBalance.Balance
                    |> Map.add commodity newCommodityBalance }

        { balances with
            Balances =
                balances.Balances
                |> Map.add account newAccountBalance }

    let processTx balances (transaction: Transaction) =
        transaction.Postings
        |> List.fold processPosting balances

    let initialState = { Balances = Map.empty }

    ledger.Transactions
    |> List.fold processTx initialState

/// Represents a map of multi-commodity balances, indexed by the date.
type BalanceByDate = Map<Date, MultiCommodityBalance>

/// Adds an amount to the balance-by-date structure.
let addAmountToBalance
    date
    (amount: Amount)
    (balances: BalanceByDate)
    : BalanceByDate =
    let commodity = amount.Commodity

    let balance =
        balances
        |> Map.tryFind date
        |> Option.defaultValue Map.empty

    let newBalance =
        balance
        |> Map.tryFind commodity
        |> Option.defaultValue (commodity |> Amount.Zero)
        |> (+) amount

    balances
    |> Map.add date (balance |> Map.add commodity newBalance)


/// Represents a list of multi-commodity balances, one for each date.
type BalanceHistory = List<Date * MultiCommodityBalance>


/// Returns the total balance change for each day.
let totalBalanceChangeHistory (ledger: Ledger) : BalanceHistory =
    let processPosting
        (balances: BalanceByDate)
        (transaction: Transaction)
        (posting: Posting)
        : BalanceByDate =
        let date = transaction.Date

        match posting.Account.NameParts[0] with
        | "assets" -> addAmountToBalance date posting.Amount balances
        | "liabilities" -> addAmountToBalance date posting.Amount balances
        | _ -> balances

    let processTx balances (transaction: Transaction) =
        transaction.Postings
        |> List.fold
            (fun balances -> processPosting balances transaction)
            balances

    let initialState = Map.empty

    let balancesByDates =
        ledger.Transactions
        |> List.fold processTx initialState

    // sort balances by date
    balancesByDates
    |> Map.toSeq
    |> Seq.sortBy fst
    |> Seq.toList


// todo 15: implement total balance history function
