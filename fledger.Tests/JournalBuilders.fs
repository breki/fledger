module fledger.Tests.JournalBuilders

open System
open fledger.BasicTypes
open fledger.Journal


let defaultCommodityDirective commodity =
    DefaultCommodity
        { Value = 1m
          Commodity = Some commodity }

let commodity commodityId = Commodity commodityId

let marketPriceDirective () =
    { Date = DateTime(2023, 01, 17)
      Price = { Value = 1m; Commodity = Some "USD" }
      Commodity = "EUR" }
    |> MarketPrice

/// Modifies the specified journal item by setting the specified date.
let onDate date journalItem =
    match journalItem with
    | Transaction tx ->
        Transaction { tx with Info = { tx.Info with Date = date } }
    | MarketPrice mp -> MarketPrice { mp with Date = date }
    | _ ->
        invalidOp "onDate: journal item must be a transaction or a market price"


let withAccountDirective account =
    { Account = AccountRef.Create account
      Subdirectives = [] }
    |> Account

let withTransaction () =
    { Info =
        { Date = DateTime(2023, 1, 7)
          Status = TransactionStatus.Unmarked
          Description = None
          Payee = None
          Note = None
          Comment = None }
      Postings = [] }

let txOnDate date tx =
    { tx with Info = { tx.Info with Date = date } }

let withPostingLine account postingLineBuilder tx =
    { tx with
        Postings =
            tx.Postings
            @ [ { Account = AccountRef.Create account
                  Amount = { Value = 0m; Commodity = Some "EUR" }
                  TotalPrice = None
                  ExpectedBalance = None }
                |> postingLineBuilder ] }

let withAmount amount commodity postingLine =
    { postingLine with
        Amount =
            { Value = amount
              Commodity = commodity } }

let withTotalPrice amount commodity postingLine =
    { postingLine with
        TotalPrice =
            Some
                { Value = amount
                  Commodity = Some commodity } }

let withExpectedBalance amount commodity postingLine =
    { postingLine with
        ExpectedBalance =
            Some
                { Value = amount
                  Commodity = Some commodity } }
