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

// type TransactionInfo =
//     { Date: Date
//       Status: TransactionStatus
//       Description: string option
//       Payee: string option
//       Note: string option
//       Comment: string option }
//
// [<Struct>]
// type JournalAmount =
//     { Value: Decimal
//       Commodity: string option }
//
// type PostingLine =
//     { Account: AccountRef
//       Amount: JournalAmount
//       TotalPrice: JournalAmount option
//       ExpectedBalance: JournalAmount option }
//
// type TransactionDirective =
//     { Info: TransactionInfo
//       Postings: PostingLine list }

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

// todo 3: continue with journal builders - implement builders
//  for posting lines
