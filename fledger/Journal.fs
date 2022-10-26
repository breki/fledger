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

type TransactionDirective =
    { Info: TransactionInfo
      Postings: PostingLine list }

type MarketPriceDirective =
    { Date: DateTime
      Commodity: string
      Price: Amount }

type AccountDirective =
    { AccountName: string
      Subdirectives: string list }

type JournalItem =
    | Account of AccountDirective
    | Comment of string
    | Commodity of string
    | DefaultCommodity of Amount
    | MarketPrice of MarketPriceDirective
    | Transaction of TransactionDirective

type Journal = { Items: JournalItem list }
