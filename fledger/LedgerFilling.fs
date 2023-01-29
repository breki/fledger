module fledger.LedgerFilling

open fledger.BalanceTypes
open fledger.BasicTypes
open fledger.Journal
open fledger.LedgerTypes
open fledger.AccountingFuncs


/// The state of the ledger while filling it from a journal.
type LedgerFillingState =
    { Commodities: Set<Commodity>
      DefaultCommodity: string option
      MarketPrices: MarketPrices
      Accounts: Map<AccountRef, Account>
      AccountsBalances: AccountsBalances
      Transactions: Transaction list
      CurrentDate: Date option
      Errors: LedgerError list }

    member this.withDate date = { this with CurrentDate = Some date }

    member this.withErrors(errors) =
        // Deduplicate errors so we don't report the same one multiple times
        // for the same transaction. Also sort them by the message so we have
        // a deterministic order.
        let uniqueErrors =
            errors
            |> Set.ofList
            |> Set.toList
            |> List.sortByDescending (fun e -> e.Message)

        { this with Errors = this.Errors |> List.append uniqueErrors }

/// Verifies that the specified account is registered and adds an error
/// if it is not.
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


/// Verifies that the specified journal item is in chronological order
/// and adds an error if it is not.
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
                $"%s{itemType} on date %s{itemDate |> dateToStr} "
                + "is not in chronological order."
              Line = lineNumber }
            :: errorsSoFar
        else
            errorsSoFar
    | None -> errorsSoFar


/// Converts the specified journal amount to a ledger amount. If the amount
/// commodity does not exist in the ledger, adds an error. Also adds an error
/// if the amount does not specify a commodity and the default commodity is not
/// specified in the journal.
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


/// Adds the specified journal transaction to the ledger. Reports any errors
/// encountered.
let processTransactionDirective
    (transaction: TransactionDirective)
    lineNumber
    state
    =
    let foldPosting
        foldPostingState
        (posting: PostingLine)
        : Posting list * MultiCommodityBalance * LedgerError list =
        let errors = []

        let accountRef, errors =
            verifyAccountExists state posting.Account lineNumber errors

        match posting.Amount with
        | Some postingAmount ->
            let amount, errors =
                toLedgerAmount state postingAmount.Amount lineNumber errors

            let totalPrice, errors =
                match postingAmount.TotalPrice with
                | Some totalPrice ->
                    let totalPriceAmount, errors =
                        toLedgerAmount state totalPrice lineNumber errors

                    Some totalPriceAmount, errors
                | None -> None, errors

            let expectedBalance, errors =
                match posting.ExpectedBalance with
                | Some expectedBalance ->
                    let expectedBalanceAmount, errors =
                        toLedgerAmount state expectedBalance lineNumber errors

                    Some expectedBalanceAmount, errors
                | None -> None, errors

            let ledgerPosting =
                { Account = accountRef
                  Amount = amount
                  TotalPrice = totalPrice
                  ExpectedBalance = expectedBalance }

            let postingBalance =
                match ledgerPosting.TotalPrice with
                // if the posting has total price, use its value
                // to balance the transaction
                | Some totalPrice ->
                    if ledgerPosting.Amount.Value >= 0.0m then
                        totalPrice
                    else
                        -totalPrice
                | None -> ledgerPosting.Amount

            let postings, balance, postingErrors = foldPostingState

            let newBalance =
                balance |> MultiCommodityBalance.AddAmount postingBalance

            ledgerPosting :: postings, newBalance, errors @ postingErrors
        | None ->
            invalidOp "The function should not be called on elided postings."

    /// Balance out the transaction by generating postings for each unbalanced
    /// commodity with the negative amount of the commodity balance.
    let generatePostingsForElided
        (elidedPosting: PostingLine)
        (commodityBalances: MultiCommodityBalance)
        =
        // for each unbalanced commodity...
        let unbalancedCommodities =
            commodityBalances.Filter(fun _ amount -> amount.Value <> 0m)

        // ... generate a posting with the opposite amount to balance it out
        unbalancedCommodities.Commodities
        |> Map.toList
        |> List.fold
            (fun (postings, commodityBalances) (_, amount) ->
                let posting =
                    { Account = elidedPosting.Account
                      Amount = -amount
                      TotalPrice = None
                      ExpectedBalance = None }

                let commodityBalances =
                    commodityBalances |> MultiCommodityBalance.AddAmount -amount

                (posting :: postings, commodityBalances))
            ([], commodityBalances)

    // Start with postings that have the amounts specified.
    // Collect postings and any errors detected while processing them.
    let postingsWithAmounts, commodityBalances, errors =
        transaction.Postings
        |> List.filter (fun posting -> posting.Amount.IsSome)
        |> List.fold foldPosting ([], MultiCommodityBalance.Empty, [])

    // reverse the postings to preserve the original order
    let postingsWithAmounts = postingsWithAmounts |> List.rev

    // check if there are any postings with elided amounts
    let generatedPostings, commodityBalances, errors =
        match
            (transaction.Postings
             |> List.filter (fun posting -> posting.Amount.IsNone))
        with
        | [] ->
            // no elided postings, nothing extra to do
            [], commodityBalances, errors
        | [ elidedPosting ] ->
            // single elided posting means we need to fill the
            // transaction with generated postings to balance the transaction
            let generatedPostings, commodityBalances =
                generatePostingsForElided elidedPosting commodityBalances

            generatedPostings, commodityBalances, errors
        | _ ->
            // if there are more than one postings with elided
            // amounts, report an error
            [],
            // we treat the transaction as balanced since we cannot
            // really determine the balance
            MultiCommodityBalance.Empty,
            { Message =
                ("Could not balance the transaction - cannot "
                 + "have more than one posting without an amount.")
              Line = lineNumber }
            :: errors

    let postings = postingsWithAmounts @ generatedPostings

    // check if the transaction is balanced
    let unbalancedCommodities =
        commodityBalances.Filter(fun _ amount -> amount.Value <> 0m)

    let errors =
        match unbalancedCommodities.Commodities |> Map.toList with
        | [] -> errors
        | [ (_, amount) ] ->
            // If only one commodity is unbalanced, we know the transaction is
            // unbalanced and we can report an error.
            { Message =
                ("Transaction is unbalanced for commodity: "
                 + $"%s{amount.ToString()}.")
              Line = lineNumber }
            :: errors
        | _ ->
            // if there is more than one unbalanced commodity, this means
            // there is no pricing data and we cannot balance the transaction
            errors

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
          Postings = postings
          Line = lineNumber }

    let accountsBalances, errors =
        updateAccountsBalancesWithTransaction
            (state.AccountsBalances, errors)
            ledgerTx

    ({ state with
        Transactions = ledgerTx :: state.Transactions
        AccountsBalances = accountsBalances }
        .withDate txDate)
        .withErrors errors


let fillLedger (journal: Journal) : Result<Ledger, LedgerError list> =
    let processJournalItem
        (state: LedgerFillingState)
        (lineNumber, journalItem)
        =
        match journalItem with
        | Account accountDirective ->
            let accountRef = accountDirective.Account

            // validate there are no duplicate accounts
            match state.Accounts.TryFind accountRef with
            | Some _ ->
                let errors =
                    [ { Message =
                          $"Duplicate '%s{accountRef.FullName}' "
                          + "account declaration."
                        Line = lineNumber } ]

                state.withErrors errors
            | None ->
                { state with
                    Accounts =
                        state.Accounts.Add(accountRef, { Name = accountRef }) }
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
            processTransactionDirective transaction lineNumber state

    let initialState: LedgerFillingState =
        { Commodities = Set.empty
          DefaultCommodity = None
          MarketPrices = { Prices = Map.empty }
          Accounts = Map.empty
          AccountsBalances = AccountsBalances.Empty
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
