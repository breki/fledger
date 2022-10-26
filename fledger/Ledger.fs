module fledger.Ledger

open System
open fledger.BasicTypes
open fledger.Journal

type Account = { Name: AccountRef }

type Amount = { Value: Decimal; Commodity: string }

type Posting =
    { Account: AccountRef
      Amount: Amount
      TotalPrice: Amount option
      ExpectedBalance: Amount option }

type Transaction =
    { Date: DateTime
      Status: TransactionStatus
      Description: string option
      Payee: string option
      Note: string option
      Comment: string option
      Postings: Posting list }

type Ledger =
    { Accounts: Map<AccountRef, Account>
      Transactions: Transaction list }

type MarketPrice2 =
    { Date: DateTime
      Commodity: string
      Price: Amount }

type LedgerFillingState =
    { Commodities: Set<string>
      DefaultCommodity: string option
      MarketPrices: List<MarketPrice2>
      Accounts: Map<AccountRef, Account>
      Transactions: Transaction list }

let fillLedger (journal: Journal) : Ledger =
    let toLedgerAmount state (amount: JournalAmount) =
        match amount.Commodity with
        | Some commodity ->
            { Value = amount.Value
              Commodity = commodity }
        | None ->
            match state.DefaultCommodity with
            | Some commodity ->
                { Value = amount.Value
                  Commodity = commodity }
            | None -> invalidOp "No default commodity"


    let processJournalItem (state: LedgerFillingState) journalItem =
        match journalItem with
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
            { state with DefaultCommodity = defaultCommodity.Commodity }
        | MarketPrice marketPrice ->
            let price =
                toLedgerAmount state marketPrice.Price

            { state with
                MarketPrices =
                    { Date = marketPrice.Date
                      Commodity = marketPrice.Commodity
                      Price = price }
                    :: state.MarketPrices }
        | Transaction transaction ->
            { state with
                Transactions =
                    { Date = transaction.Info.Date
                      Status = transaction.Info.Status
                      Description = transaction.Info.Description
                      Payee = transaction.Info.Payee
                      Note = transaction.Info.Note
                      Comment = transaction.Info.Comment
                      Postings =
                        transaction.Postings
                        |> List.map (fun posting ->
                            { Account = posting.Account
                              Amount = toLedgerAmount state posting.Amount
                              TotalPrice =
                                match posting.TotalPrice with
                                | Some totalPrice ->
                                    Some(toLedgerAmount state totalPrice)
                                | None -> None
                              ExpectedBalance =
                                match posting.ExpectedBalance with
                                | Some expectedBalance ->
                                    Some(toLedgerAmount state expectedBalance)
                                | None -> None }) }
                    :: state.Transactions }


    let initialState: LedgerFillingState =
        { Commodities = Set.empty
          DefaultCommodity = None
          MarketPrices = []
          Accounts = Map.empty
          Transactions = [] }

    let finalState =
        journal.Items
        |> List.fold processJournalItem initialState

    { Transactions = finalState.Transactions
      Accounts = finalState.Accounts }
