module fledger.Tests.validating.validating_chronological_order

open System
open Xunit
open Swensen.Unquote
open fledger.Ledger
open fledger.BasicTypes

// type Ledger =
//     { Accounts: Map<AccountRef, Account>
//       Transactions: Transaction list
//       MarketPrices: MarketPrices }

// type Account = { Name: AccountRef }

// type AccountRef =
//     { FullName: AccountName
//       NameParts: string[] }
//
//     static member Create(fullName: string) =
//         let parts = fullName.Split(':')
//
//         { FullName = fullName
//           NameParts = parts }

// type Transaction =
//     { Date: Date
//       Status: TransactionStatus
//       Description: string option
//       Payee: string option
//       Note: string option
//       Comment: string option
//       Postings: Posting list }


let transactionsAreChronologicallyOrdered ledger =
    ledger.Transactions
    |> List.pairwise
    |> List.forall (fun (t1, t2) -> t1.Date <= t2.Date)

[<Fact>]
let ``transactions should be in chronological order`` () =
    // create ledger with single account and two transactions

    let accountRef = AccountRef.Create "Assets:Checking"
    let account = { Name = accountRef }

    // todo 5: we need a tx builder
    let tx1 =
        { Date = DateTime(2023, 1, 7)
          Status = TransactionStatus.Unmarked
          Description = None
          Payee = None
          Note = None
          Comment = None
          Postings = [] }

    let tx2 =
        { Date = DateTime(2023, 1, 6)
          Status = TransactionStatus.Unmarked
          Description = None
          Payee = None
          Note = None
          Comment = None
          Postings = [] }

    let ledger =
        { Accounts = [ accountRef, account ] |> Map.ofList
          Transactions = [ tx1; tx2 ]
          MarketPrices = { Prices = Map.empty } }

    test <@ transactionsAreChronologicallyOrdered ledger = false @>
