module fledger.BasicTypes

type AccountName = string

type TransactionStatus =
    | Unmarked
    | Pending
    | Cleared

