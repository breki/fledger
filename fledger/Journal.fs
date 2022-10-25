module fledger.Journal

open System

type TransactionStatus =
    | Unmarked
    | Pending
    | Cleared

type TransactionInfo =
    { Date: DateTime
      Status: TransactionStatus
      Description: string option
      Payee: string option
      Note: string option
      Comment: string option }

type Amount =
    { Value: Decimal
      Commodity: string option }

type PostingLine =
    { Account: string
      Amount: Amount
      TotalPrice: Amount option
      ExpectedBalance: Amount option }

type Transaction =
    { Info: TransactionInfo
      Postings: PostingLine list }

type JournalItem =
    | Transaction of Transaction
    | Commodity of string
    | DefaultCommodity of Amount

type Journal = { Items: JournalItem list }
