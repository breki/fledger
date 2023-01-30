﻿module fledger.Tests.LedgerBuilders

open System
open fledger.LedgerTypes
open fledger.BasicTypes


let withTransaction () =
    { Date = DateTime(2023, 1, 7)
      Status = TransactionStatus.Unmarked
      Description = None
      Payee = None
      Note = None
      Comment = None
      Postings = []
      Line = 0 }

let onDate date (tx: Transaction) = { tx with Date = date }
let withDescription desc (tx: Transaction) = { tx with Description = Some desc }

let addPosting posting tx =
    { tx with Postings = tx.Postings @ [ posting ] }

let toAccount account =
    { Account = AccountRef.Create account
      Amount = Amount.Zero("EUR")
      TotalPrice = None
      ExpectedBalance = None }

let amountOf amount posting =
    { posting with Amount = { Value = amount; Commodity = "EUR" } }

let amountCommodityOf amount commodity posting =
    { posting with
        Amount =
            { Value = amount
              Commodity = commodity } }

let withTotalPrice amount commodity posting =
    { posting with
        TotalPrice =
            Some
                { Value = amount
                  Commodity = commodity } }


let withLedger (accounts: Account list) (transactions: Transaction list) =
    // convert account list to a map, with account names as keys
    let accountsMap = accounts |> List.map (fun a -> a.Name, a) |> Map.ofList

    { Accounts = accountsMap
      Transactions = transactions
      MarketPrices = { Prices = Map.empty }
      Items = [] }
