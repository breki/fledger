module fledger.Journal

open System

open fledger.BasicTypes

type TransactionInfo =
    { Date: Date
      Status: TransactionStatus
      Description: string option
      Payee: string option
      Note: string option
      Comment: string option }

[<Struct>]
type JournalAmount =
    { Value: Decimal
      Commodity: string option }

type JournalPostingAmount =
    { Amount: JournalAmount
      TotalPrice: JournalAmount option }

type PostingLine =
    { Account: AccountRef
      Amount: JournalPostingAmount option
      ExpectedBalance: JournalAmount option }

type TransactionDirective =
    { Info: TransactionInfo
      Postings: PostingLine list }

[<Struct>]
type MarketPriceDirective =
    { Date: Date
      Commodity: string
      Price: JournalAmount }

type AccountDirective =
    { Account: AccountRef
      Subdirectives: string list }

type JournalItem =
    | Account of AccountDirective
    | Comment of string
    | Commodity of Commodity
    | DefaultCommodity of JournalAmount
    | MarketPrice of MarketPriceDirective
    | Transaction of TransactionDirective

type Journal = { Items: (int64 * JournalItem) list }
