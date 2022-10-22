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
      Comment: string option }

// todo 10: amount currency should be optional
type Amount = { Value: Decimal; Currency: string }

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
    | DefaultCommodity of Amount

type Journal = { Items: JournalItem list }
