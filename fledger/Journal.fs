module fledger.Journal

open System

type TransactionState =
    | Unmarked
    | Pending
    | Cleared

type TransactionInfo =
    { Date: DateTime
      State: TransactionState
      Description: string }

type Amount = { Value: Decimal; Currency: string }

type PostingLine =
    { Account: string
      Amount: Amount
      TotalPrice: Amount option
      ExpectedBalance: Amount option }

type Transaction =
    { Info: TransactionInfo
      Postings: PostingLine list }
