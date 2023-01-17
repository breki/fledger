module fledger.Tests.parsing.filling_ledger

open fledger.Journal
open Xunit
open fledger.Ledger
open fledger.Tests.JournalBuilders
open Swensen.Unquote


[<Fact>]
let ``automatically adds a missing commodity in DefaultCommodity directive``
    ()
    =
    let journal = { Items = [ 14L, defaultCommodityDirective () ] }

    match fillLedger journal with
    | Result.Error errors ->
        failwith (String.concat "," (errors |> List.map (fun x -> x.Message)))
    | Result.Ok _ -> test <@ true @>


[<Fact>]
let ``reports missing commodities in MarketPrice directive`` () =
    let journal = { Items = [ 14L, marketPriceDirective () ] }

    match fillLedger journal with
    | Result.Error errors ->
        test
            <@
                errors = [ { Message = "Commodity 'EUR' not defined."
                             Line = 14L }
                           { Message = "Commodity 'USD' not defined."
                             Line = 14L } ]
            @>
    | Result.Ok _ -> failwith "should not be ok"

[<Fact>]
let ``reports missing account and commodity errors for Transaction directive``
    ()
    =
    let journal =
        { Items =
            [ 14L,
              withTransaction ()
              |> (withPostingLine "acc1" (fun p ->
                  p |> withTotalPrice 10m "USD" |> withExpectedBalance 20m "GBP"))
              |> Transaction ] }

    match fillLedger journal with
    | Result.Error errors ->
        test
            <@
                errors = [ { Message = "Account 'acc1' not defined."
                             Line = 14L }
                           { Message = "Commodity 'EUR' not defined."
                             Line = 14L }
                           { Message = "Commodity 'USD' not defined."
                             Line = 14L }
                           { Message = "Commodity 'GBP' not defined."
                             Line = 14L } ]
            @>
    | Result.Ok _ -> failwith "should not be ok"

// todo 9: verify unbalanced transactions are reported
