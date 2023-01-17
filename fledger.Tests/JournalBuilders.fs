module fledger.Tests.JournalBuilders

open System
open fledger.BasicTypes
open fledger.Journal


let defaultCommodityDirective () =
    DefaultCommodity { Value = 1m; Commodity = Some "EUR" }

let marketPriceDirective () =
    { Date = DateTime(2023, 01, 17)
      Price = { Value = 1m; Commodity = Some "USD" }
      Commodity = "EUR" }
    |> MarketPrice

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

let withPostingLine account postingLineBuilder tx =
    { tx with
        Postings =
            tx.Postings
            @ [ { Account = AccountRef.Create account
                  Amount = { Value = 1m; Commodity = Some "EUR" }
                  TotalPrice = None
                  ExpectedBalance = None }
                |> postingLineBuilder ] }

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
