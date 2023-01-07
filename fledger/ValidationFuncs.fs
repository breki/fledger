module fledger.ValidationFuncs

open fledger.Ledger

type ValidationError = { Message: string }

/// Asserts the ledger's transactions are in chronological order.
let transactionsAreChronologicallyOrdered ledger =
    // find first ledger transaction that is not in the chronological order
    let firstOutOfOrderTransaction =
        ledger.Transactions
        |> List.pairwise
        |> Seq.tryFindIndex (fun (t1, t2) -> t1.Date.Date > t2.Date.Date)

    match firstOutOfOrderTransaction with
    | Some index ->
        let transactionBefore = ledger.Transactions.[index]
        let transaction = ledger.Transactions.[index + 1]

        Result.Error
            { Message =
                ($"Transaction out of order: {transaction.DateStr} "
                 + $"{transaction.FullDescription}") }
    | None -> Result.Ok None
