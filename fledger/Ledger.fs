module fledger.Ledger

open System
open System.Globalization
open fledger.BasicTypes
open fledger.Journal
open Text

type Account = { Name: AccountRef }

type Amount =
    { Value: Decimal
      Commodity: Commodity }
    override this.ToString() = $"%.2f{this.Value} %s{this.Commodity}"

    static member Zero(commodity: string) =
        { Value = 0.0M; Commodity = commodity }

    static member (+)(a: Amount, b: Amount) =
        if a.Commodity <> b.Commodity then
            failwith "Cannot add amounts of different commodities"

        { Value = a.Value + b.Value
          Commodity = a.Commodity }

    member this.Convert(price: Amount) : Amount =
        { Value = this.Value * price.Value
          Commodity = price.Commodity }

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
            x
            |> append " @@ "
            |> append (this.TotalPrice.Value.ToString()))
        |> ifDo this.ExpectedBalance.IsSome (fun x ->
            x
            |> append " = "
            |> append (this.ExpectedBalance.Value.ToString()))
        |> toString

type Transaction =
    { Date: Date
      Status: TransactionStatus
      Description: string option
      Payee: string option
      Note: string option
      Comment: string option
      Postings: Posting list }

    override this.ToString() =
        buildString ()
        |> append (
            this.Date.ToString("yyyy/MM/dd", DateTimeFormatInfo.InvariantInfo)
        )
        |> append " "
        |> ifDo (this.Status = TransactionStatus.Cleared) (append "*")
        |> ifDo (this.Status = TransactionStatus.Pending) (append "!")
        |> ifDo (this.Status = TransactionStatus.Unmarked) (append " ")
        |> ifDo this.Description.IsSome (fun x ->
            x |> append this.Description.Value)
        |> ifDo this.Payee.IsSome (fun x -> x |> append ("|" + this.Payee.Value))
        |> ifDo this.Note.IsSome (fun x -> x |> append ("|" + this.Note.Value))
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
                prices
                |> Map.tryFind commodity2
                |> Option.defaultValue []

            let closestPriceAfterDate =
                prices
                |> List.tryFind (fun (date', _) -> date' <= date)

            let priceAtDate =
                match closestPriceAfterDate with
                | Some priceAtDate -> Some priceAtDate
                | None -> prices |> List.tryFind (fun _ -> true)

            match priceAtDate with
            | Some (_, price) -> amount.Convert price
            | None ->
                let dateStr =
                    date.ToString(
                        "yyyy/MM/dd",
                        DateTimeFormatInfo.InvariantInfo
                    )

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
                 ((price.Date, price.Price)
                  :: fromCommodityToCommodityPrices))

    { Prices = prices }


/// Sort market prices by dates (sorted descendingly)
let sortMarketPrices (prices: MarketPrices) : MarketPrices =
    let prices =
        prices.Prices
        |> Map.map (fun _ ->
            Map.map (fun _ prices -> prices |> List.sortBy fst |> List.rev))

    { Prices = prices }

type LedgerFillingState =
    { Commodities: Set<Commodity>
      DefaultCommodity: string option
      MarketPrices: MarketPrices
      Accounts: Map<AccountRef, Account>
      Transactions: Transaction list }

type Ledger =
    { Accounts: Map<AccountRef, Account>
      Transactions: Transaction list
      MarketPrices: MarketPrices }

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
                    state.MarketPrices
                    |> addMarketPrice
                        { Date = marketPrice.Date
                          Commodity = marketPrice.Commodity
                          Price = price } }
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
          MarketPrices = { Prices = Map.empty }
          Accounts = Map.empty
          Transactions = [] }

    let finalState =
        journal.Items
        |> List.fold processJournalItem initialState

    { Transactions = finalState.Transactions
      Accounts = finalState.Accounts
      MarketPrices = finalState.MarketPrices |> sortMarketPrices }
