module fledger.AccountingFuncs

open fledger.BasicTypes
open fledger.Ledger

type AccountBalance =
    { Account: AccountName
      Balance: Map<string, decimal> }

type AccountsBalances =
    { Balances: Map<AccountName, AccountBalance> }

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
            |> Option.defaultValue 0m
            |> (+) amount.Value

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
