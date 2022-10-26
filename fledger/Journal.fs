module fledger.Journal

open System

open fledger.BasicTypes

type TransactionInfo =
    { Date: DateTime
      Status: TransactionStatus
      Description: string option
      Payee: string option
      Note: string option
      Comment: string option }

type JournalAmount =
    { Value: Decimal
      Commodity: string option }

type PostingLine =
    { Account: AccountName
      Amount: JournalAmount
      TotalPrice: JournalAmount option
      ExpectedBalance: JournalAmount option }

type TransactionDirective =
    { Info: TransactionInfo
      Postings: PostingLine list }

type MarketPriceDirective =
    { Date: DateTime
      Commodity: string
      Price: JournalAmount }

type AccountDirective =
    { AccountName: AccountName
      Subdirectives: string list }

type JournalItem =
    | Account of AccountDirective
    | Comment of string
    | Commodity of string
    | DefaultCommodity of JournalAmount
    | MarketPrice of MarketPriceDirective
    | Transaction of TransactionDirective

type Journal = { Items: JournalItem list }
