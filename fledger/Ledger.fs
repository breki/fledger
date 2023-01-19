module fledger.Ledger

open System
open fledger.BasicTypes
open fledger.Journal
open Text

type Account =
    { Name: AccountRef }

    static member Create(fullName: string) =
        { Name = AccountRef.Create(fullName) }


[<Struct>]
[<StructuredFormatDisplay("{DisplayText}")>]
type Amount =
    { Value: Decimal
      Commodity: Commodity }

    static member Of (commodity: Commodity) (value: Decimal) =
        { Value = value; Commodity = commodity }

    static member Zero(commodity: string) =
        { Value = 0.0M; Commodity = commodity }

    static member inline (~-)(a: Amount) = { a with Value = -a.Value }

    static member (+)(a: Amount, b: Amount) =
        if a.Commodity <> b.Commodity then
            failwith "Cannot add amounts of different commodities"

        { Value = a.Value + b.Value
          Commodity = a.Commodity }

    static member (*)(amount: Amount, multiplier: int) =
        { Value = amount.Value * (decimal multiplier)
          Commodity = amount.Commodity }

    static member (/)(amount: Amount, divisor: int) =
        { Value = amount.Value / (decimal divisor)
          Commodity = amount.Commodity }

    member this.Convert(price: Amount) : Amount =
        { Value = this.Value * price.Value
          Commodity = price.Commodity }

    override this.ToString() = $"%.2f{this.Value} %s{this.Commodity}"
    member this.DisplayText = this.ToString()

type Posting =
    { Account: AccountRef
      Amount: Amount
      TotalPrice: Amount option
      ExpectedBalance: Amount option }

    override this.ToString() =
        buildString ()
        |> append this.Account.FullName
        |> append "  "
        |> append (this.Amount.ToString())
        |> ifDo this.TotalPrice.IsSome (fun x ->
            x |> append " @@ " |> append (this.TotalPrice.Value.ToString()))
        |> ifDo this.ExpectedBalance.IsSome (fun x ->
            x |> append " = " |> append (this.ExpectedBalance.Value.ToString()))
        |> toString

type Transaction =
    { Date: Date
      Status: TransactionStatus
      Description: string option
      Payee: string option
      Note: string option
      Comment: string option
      Postings: Posting list }

    member this.DateStr = this.Date |> dateToStr

    member this.FullDescription =
        buildString ()
        |> ifDo this.Description.IsSome (fun x ->
            x |> append this.Description.Value)
        |> ifDo this.Payee.IsSome (fun x -> x |> append ("|" + this.Payee.Value))
        |> ifDo this.Note.IsSome (fun x -> x |> append ("|" + this.Note.Value))
        |> toString

    override this.ToString() =
        buildString ()
        |> append this.DateStr
        |> append " "
        |> append this.FullDescription
        |> ifDo this.Comment.IsSome (fun x ->
            x |> append ("; " + this.Comment.Value))
        |> newLine
        |> append (
            this.Postings
            |> List.map (fun p -> "  " + p.ToString())
            |> String.concat Environment.NewLine
        )
        |> toString


type MarketPrice =
    { Date: Date
      Commodity: Commodity
      Price: Amount }

/// Stores market prices for commodities as a map from the source commodity to
/// the target commodity, which then stores the prices sorted descendingly
/// by the date of the price.
type MarketPrices =
    { Prices: Map<Commodity, Map<Commodity, List<Date * Amount>>> }

    member this.Convert
        (amount: Amount)
        (commodity2: Commodity)
        (date: Date)
        : Amount =
        match amount.Commodity = commodity2 with
        | true -> amount
        | false ->
            let prices =
                this.Prices
                |> Map.tryFind amount.Commodity
                |> Option.defaultValue Map.empty

            let prices =
                prices |> Map.tryFind commodity2 |> Option.defaultValue []

            let closestPriceAfterDate =
                prices |> List.tryFind (fun (date', _) -> date' <= date)

            let priceAtDate =
                match closestPriceAfterDate with
                | Some priceAtDate -> Some priceAtDate
                | None -> prices |> List.tryFind (fun _ -> true)

            match priceAtDate with
            | Some (_, price) -> amount.Convert price
            | None ->
                let dateStr = date |> dateToStr

                failwith
                    $"No market price to convert commodity \
                    %s{amount.Commodity} to commodity %s{commodity2} \
                    on date %s{dateStr}"

/// Add a new price to the MarketPrices.
let addMarketPrice (price: MarketPrice) (prices: MarketPrices) : MarketPrices =
    let fromCommodity = price.Commodity

    let fromCommodityPrices =
        match prices.Prices.TryFind fromCommodity with
        | Some fromCommodityPrices -> fromCommodityPrices
        | None -> Map.empty

    let toCommodity = price.Price.Commodity

    let fromCommodityToCommodityPrices =
        match fromCommodityPrices.TryFind toCommodity with
        | Some fromCommodityToCommodityPrices -> fromCommodityToCommodityPrices
        | None -> []

    let prices =
        prices.Prices
        |> Map.add
            fromCommodity
            (fromCommodityPrices
             |> Map.add
                 toCommodity
                 ((price.Date, price.Price) :: fromCommodityToCommodityPrices))

    { Prices = prices }


/// Sort market prices by dates (sorted descendingly)
let sortMarketPrices (prices: MarketPrices) : MarketPrices =
    let prices =
        prices.Prices
        |> Map.map (fun _ ->
            Map.map (fun _ prices -> prices |> List.sortBy fst |> List.rev))

    { Prices = prices }

/// An error encountered while filling the ledger from a journal.
type LedgerError = { Message: string; Line: int64 }

// todo 7: all dated directives should be checked for chronological order
type LedgerFillingState =
    { Commodities: Set<Commodity>
      DefaultCommodity: string option
      MarketPrices: MarketPrices
      Accounts: Map<AccountRef, Account>
      Transactions: Transaction list
      CurrentDate: Date option
      Errors: LedgerError list }

    member this.withDate date = { this with CurrentDate = Some date }

    member this.withErrors(errors) =
        { this with Errors = this.Errors |> List.append errors }

type Ledger =
    { Accounts: Map<AccountRef, Account>
      Transactions: Transaction list
      MarketPrices: MarketPrices }

let fillLedger (journal: Journal) : Result<Ledger, LedgerError list> =

    let verifyItemIsInChronologicalOrder
        state
        itemType
        itemDate
        lineNumber
        errorsSoFar
        =
        match state.CurrentDate with
        | Some currentDate ->
            if itemDate < currentDate then
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

            ({ state with
                Transactions =
                    { Date = txDate
                      Status = transaction.Info.Status
                      Description = transaction.Info.Description
                      Payee = transaction.Info.Payee
                      Note = transaction.Info.Note
                      Comment = transaction.Info.Comment
                      Postings = postings }
                    :: state.Transactions }
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
