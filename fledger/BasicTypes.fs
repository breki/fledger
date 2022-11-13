module fledger.BasicTypes

open System

type Date = DateTime

type Commodity = string
type AccountName = string


[<Struct; CustomEquality; CustomComparison>]
type AccountRef =
    { FullName: AccountName
      NameParts: string[] }

    static member Create(fullName: string) =
        let parts = fullName.Split(':')

        { FullName = fullName
          NameParts = parts }

    interface IEquatable<AccountRef> with
        member this.Equals other = other.FullName.Equals this.FullName

    override this.Equals other =
        match other with
        | :? AccountRef as p -> (this :> IEquatable<_>).Equals p
        | _ -> false

    override this.GetHashCode() = this.FullName.GetHashCode()

    interface IComparable with
        member this.CompareTo other =
            match other with
            | :? AccountRef as p -> (this :> IComparable<_>).CompareTo p
            | _ -> -1

    interface IComparable<AccountRef> with
        member this.CompareTo other = other.FullName.CompareTo this.FullName

type TransactionStatus =
    | Unmarked
    | Pending
    | Cleared
