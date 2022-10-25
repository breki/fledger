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

type MarketPrice =
    { Date: DateTime
      Commodity: string
      Price: Amount }

type JournalItem =
    | Commodity of string
    | DefaultCommodity of Amount
    | MarketPrice of MarketPrice
    | Transaction of Transaction

type Journal = { Items: JournalItem list }
