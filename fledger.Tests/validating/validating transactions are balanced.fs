module fledger.Tests.validating.validating_transactions_are_balanced

open Xunit
open fledger.BalanceTypes
open fledger.Ledger
open fledger.ValidationFuncs
open fledger.Tests.LedgerBuilders
open Swensen.Unquote

// todo 13: validator: total amount of a transaction should be 0
let transactionsAreBalanced ledger =
    // return unbalanced commodities for a transaction
    let unbalancedTxCommodities tx =
        // calculate multi-commodity balances from the tx postings
        let balances =
            tx.Postings
            |> List.map (fun p -> p.Amount)
            |> MultiCommodityBalance.FromAmounts

        // return only the unbalanced commodities
        balances.Filter(fun _ amount -> amount.Value <> 0m)

    let unbalancedTransactions =
        ledger.Transactions
        |> List.map (fun tx -> tx, unbalancedTxCommodities tx)
        |> List.filter (fun (_, unbalancedCommodities) ->
            unbalancedCommodities.Commodities.Count > 0)

    // todo 11: each unbalanced transaction should be a separate validation error
    // - redesign the validators to support returning multiple errors
    // (and associated transactions)
    if unbalancedTransactions.Length > 0 then
        Result.Error { Message = "Transactions are not balanced" }
    else
        Result.Ok ledger

[<Fact>]
let ``transactions should be balanced`` () =
    let accountObj = Account.Create "Assets:Checking"
    let account = accountObj.Name

    let tx =
        withTransaction ()
        |> withDescription "not balanced"
        |> (addPosting (toAccount account |> amountOf 10m))
        |> (addPosting (toAccount account |> amountOf -5m))

    let ledger = withLedger [ accountObj ] [ tx ]

    let result = transactionsAreBalanced ledger

    test <@ Result.isError result @>

    // todo 14: the transaction (un)balance should be written into the error
    // message
    test
        <@ result = Result.Error({ Message = "Transactions are not balanced" }) @>
