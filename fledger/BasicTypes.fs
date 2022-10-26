module fledger.BasicTypes

type AccountName = string

// todo 12: implement custom equality that just uses the full account name
type AccountRef =
    { FullName: AccountName
      NameParts: string [] }

    static member Create(fullName: string) =
        let parts = fullName.Split(':')

        { FullName = fullName
          NameParts = parts }

type TransactionStatus =
    | Unmarked
    | Pending
    | Cleared
