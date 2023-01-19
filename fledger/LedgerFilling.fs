module fledger.LedgerFilling

open fledger.BalanceTypes
open fledger.BasicTypes
open fledger.Journal
open fledger.LedgerTypes

let fillLedger (journal: Journal) : Result<Ledger, LedgerError list> =

    let verifyItemIsInChronologicalOrder
        state
        itemType
        (itemDate: Date)
        lineNumber
        errorsSoFar
        =
        match state.CurrentDate with
        | Some currentDate ->
            if itemDate.Date < currentDate.Date then
                { Message =
                    $"%s{itemType} on date %s{itemDate |> dateToStr} is not in chronological order."
                  Line = lineNumber }
                :: errorsSoFar
            else
                errorsSoFar
        | None -> errorsSoFar


    let verifyAccountExists
        (state: LedgerFillingState)
        account
        lineNumber
        errorsSoFar
        =
        if state.Accounts.ContainsKey account then
            account, errorsSoFar
        else
            account,
            { Message = $"Account '%s{account.FullName}' not defined."
              Line = lineNumber }
            :: errorsSoFar

    let toLedgerAmount
        state
        (amount: JournalAmount)
        lineNumber
        errorsSoFar
        : Amount * LedgerError list =
        match amount.Commodity with
        | Some commodity ->
            if state.Commodities.Contains commodity then
                { Value = amount.Value
                  Commodity = commodity },
                errorsSoFar
            else
                { Value = amount.Value
                  Commodity = commodity },
                { Message = $"Commodity '%s{commodity}' not defined."
                  Line = lineNumber }
                :: errorsSoFar
        | None ->
            match state.DefaultCommodity with
            | Some commodity ->
                { Value = amount.Value
                  Commodity = commodity },
                errorsSoFar
            | None ->
                { Value = amount.Value
                  Commodity = "not defined" },
                { Message =
                    "An amount does not specify the commodity even though "
                    + "the default commodity was not defined."
                  Line = lineNumber }
                :: errorsSoFar

    // todo 5: take into account posting TotalPrice when calculating balances
    let unbalancedTxCommodities tx =
        // calculate multi-commodity balances from the tx postings
        let balances =
            tx.Postings
            |> List.map (fun p -> p.Amount)
            |> MultiCommodityBalance.FromAmounts

        // return only the unbalanced commodities
        balances.Filter(fun _ amount -> amount.Value <> 0m)


    let processJournalItem
        (state: LedgerFillingState)
        (lineNumber, journalItem)
        =
        match journalItem with
        // todo 10: validate there are no duplicate accounts
        | Account account ->
            { state with
                Accounts =
                    state.Accounts.Add(
                        account.Account,
                        { Name = account.Account }
                    ) }
        | Comment _ -> state
        | Commodity commodity ->
            { state with Commodities = state.Commodities.Add commodity }
        | DefaultCommodity defaultCommodity ->
            match defaultCommodity.Commodity with
            | Some commodity ->
                if state.Commodities.Contains commodity then
                    { state with DefaultCommodity = defaultCommodity.Commodity }
                else
                    // automatically add the default commodity to the
                    // commodities list
                    { state with
                        Commodities = state.Commodities.Add commodity
                        DefaultCommodity = defaultCommodity.Commodity }
            | None -> invalidOp "No default commodity"
        | MarketPrice marketPrice ->
            let errors =
                if state.Commodities.Contains marketPrice.Commodity then
                    []
                else
                    [ { Message =
                          $"Commodity '%s{marketPrice.Commodity}' not defined."
                        Line = lineNumber } ]

            let price, errors =
                toLedgerAmount state marketPrice.Price lineNumber errors

            let errors =
                verifyItemIsInChronologicalOrder
                    state
                    "Market price"
                    marketPrice.Date
                    lineNumber
                    errors

            ({ state with
                MarketPrices =
                    state.MarketPrices
                    |> addMarketPrice
                        { Date = marketPrice.Date
                          Commodity = marketPrice.Commodity
                          Price = price } }
                .withDate marketPrice.Date)
                .withErrors errors

        | Transaction transaction ->
            // todo 9: check the transaction is balanced
            let processPosting
                (posting: PostingLine)
                lineNumber
                : Posting * LedgerError list =
                let errors = []

                let accountRef, errors =
                    verifyAccountExists state posting.Account lineNumber errors

                let amount, errors =
                    toLedgerAmount state posting.Amount lineNumber errors

                let totalPrice, errors =
                    match posting.TotalPrice with
                    | Some totalPrice ->
                        let totalPriceAmount, errors =
                            toLedgerAmount state totalPrice lineNumber errors

                        Some totalPriceAmount, errors
                    | None -> None, errors

                let expectedBalance, errors =
                    match posting.ExpectedBalance with
                    | Some expectedBalance ->
                        let expectedBalanceAmount, errors =
                            toLedgerAmount
                                state
                                expectedBalance
                                lineNumber
                                errors

                        Some expectedBalanceAmount, errors
                    | None -> None, errors

                { Account = accountRef
                  Amount = amount
                  TotalPrice = totalPrice
                  ExpectedBalance = expectedBalance },
                errors

            // collect postings and any errors detected while processing them
            let postings, errors =
                transaction.Postings
                |> List.fold
                    (fun (postings, errors) posting ->
                        let posting, postingErrors =
                            processPosting posting lineNumber

                        posting :: postings, errors @ postingErrors)
                    ([], [])

            // return the collected transaction and any errors detected while processing them
            let txDate = transaction.Info.Date

            let errors =
                verifyItemIsInChronologicalOrder
                    state
                    "Transaction"
                    txDate
                    lineNumber
                    errors

            let ledgerTx =
                { Date = txDate
                  Status = transaction.Info.Status
                  Description = transaction.Info.Description
                  Payee = transaction.Info.Payee
                  Note = transaction.Info.Note
                  Comment = transaction.Info.Comment
                  Postings = postings }

            // create error entry for each unbalanced commodity
            let unbalancedCommodities = unbalancedTxCommodities ledgerTx

            let errors =
                unbalancedCommodities.Commodities
                |> Map.toList
                |> List.fold
                    (fun errors (_, amount) ->
                        { Message =
                            ("Transaction is unbalanced for commodity: "
                             + $"%s{amount.ToString()}.")
                          Line = lineNumber }
                        :: errors)
                    errors

            ({ state with Transactions = ledgerTx :: state.Transactions }
                .withDate txDate)
                .withErrors errors


    let initialState: LedgerFillingState =
        { Commodities = Set.empty
          DefaultCommodity = None
          MarketPrices = { Prices = Map.empty }
          Accounts = Map.empty
          Transactions = []
          CurrentDate = None
          Errors = [] }

    let finalState = journal.Items |> List.fold processJournalItem initialState

    if finalState.Errors |> List.isEmpty then
        Ok
            { Transactions = finalState.Transactions |> List.rev
              Accounts = finalState.Accounts
              MarketPrices = finalState.MarketPrices |> sortMarketPrices }
    else
        Error finalState.Errors |> Result.mapError List.rev
