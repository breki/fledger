module fledger.Tests.parsing.filling_ledger

open System
open fledger.Journal
open Xunit
open fledger.Ledger
open Swensen.Unquote



// type JournalItem =
//     | Account of AccountDirective
//     | Comment of string
//     | Commodity of Commodity
//     | DefaultCommodity of JournalAmount
//     | MarketPrice of MarketPriceDirective
//     | Transaction of TransactionDirective
//
// type Journal = { Items: (int64 * JournalItem) list }


[<Fact>]
let ``reports a missing commodity in DefaultCommodity directive`` () =
    let journal =
        { Items =
            [ 14L, DefaultCommodity { Value = 1m; Commodity = Some "EUR" } ] }

    match fillLedger journal with
    | Result.Error errors ->
        match errors with
        | [ error ] ->
            test
                <@
                    error = { Message = "Commodity 'EUR' not defined."
                              Line = 14L }
                @>
        | _ -> failwith "Expected one error"
    | Result.Ok _ -> failwith "should not be ok"


[<Fact>]
let ``reports missing commodities in MarketPrice directive`` () =
    let journal =
        { Items =
            [ 14L,
              MarketPrice
                  { Date = DateTime.Now
                    Price = { Value = 1m; Commodity = Some "USD" }
                    Commodity = "EUR" } ] }

    match fillLedger journal with
    | Result.Error errors ->
        <@
            errors = [ { Message = "Commodity 'EUR' not defined."
                         Line = 14L }
                       { Message = "Commodity 'USD' not defined."
                         Line = 14L } ]
        @>
    | Result.Ok _ -> failwith "should not be ok"
