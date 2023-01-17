module fledger.Tests.validating.validating_chronological_order

open System
open Xunit
open fledger.Ledger
open fledger.ValidationFuncs
open fledger.Tests.LedgerBuilders
open Swensen.Unquote

[<Fact>]
let ``transactions should be in chronological order`` () =
    // create ledger with single account and two transactions

    let account = Account.Create "Assets:Checking"

    let tx1 = withTransaction ()

    let tx2 =
        withTransaction ()
        |> onDate (DateTime(2023, 1, 6))
        |> withDescription "not in order"

    let ledger = withLedger [ account ] [ tx1; tx2 ]

    let result = transactionsAreChronologicallyOrdered ledger

    test <@ Result.isError result @>

    test
        <@
            result = Result.Error(
                { Message = "Transaction out of order: 2023/01/06 not in order" }
            )
        @>
