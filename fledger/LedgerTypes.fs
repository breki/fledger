module fledger.LedgerTypes

open System
open System.Globalization
open fledger.BasicTypes
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

    member this.IsZero = this.Value = 0.0M

    member this.Convert(price: Amount) : Amount =
        { Value = this.Value * price.Value
          Commodity = price.Commodity }

    override this.ToString() =
        String.Format(
            CultureInfo.InvariantCulture,
            "{0:0.00} {1}",
            this.Value,
            this.Commodity
        )

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
      Postings: Posting list
      Line: int64 }

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

// todo 8: define ledger items as a discriminated union
type LedgerItem =
    | Account of Account
    // todo 9: add support for keeping comment items in ledger
    //| Comment of Comment
    // todo 10: add support for keeping DefaultCommodity items in ledger
    //| DefaultCommodity of DefaultCommodity
    | MarketPrice of MarketPrice
    | Transaction of Transaction

/// The final, filled ledger.
type Ledger =
    { Accounts: Map<AccountRef, Account>
      Transactions: Transaction list
      MarketPrices: MarketPrices
      Items: LedgerItem list }
