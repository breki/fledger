module fledger.Journal

// todo 30: add "directive" to all the types that represent directives
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

type AccountDirective =
    { AccountName: string
      Subdirectives: string list }

type JournalItem =
    | Account of AccountDirective
    | Comment of string
    | Commodity of string
    | DefaultCommodity of Amount
    | MarketPrice of MarketPrice
    | Transaction of Transaction

type Journal = { Items: JournalItem list }
