module fledger.Tests.parsing.filling_ledger

open fledger.Journal
open Xunit
open fledger.Ledger
open fledger.Tests.JournalBuilders


[<Fact>]
let ``automatically adds a missing commodity in DefaultCommodity directive``
    ()
    =
    let journal = { Items = [ 14L, defaultCommodityDirective () ] }

    match fillLedger journal with
    | Result.Error errors ->
        failwith (String.concat "," (errors |> List.map (fun x -> x.Message)))
    | Result.Ok _ -> <@ true @>


[<Fact>]
let ``reports missing commodities in MarketPrice directive`` () =
    let journal = { Items = [ 14L, marketPriceDirective () ] }

    match fillLedger journal with
    | Result.Error errors ->
        <@
            errors = [ { Message = "Commodity 'EUR' not defined."
                         Line = 14L }
                       { Message = "Commodity 'USD' not defined."
                         Line = 14L } ]
        @>
    | Result.Ok _ -> failwith "should not be ok"

[<Fact>]
let ``reports validation errors for Transaction directive`` () =
    let journal =
        { Items =
            [ 14L,
              withTransaction () |> (withPostingLine "acc1" id) |> Transaction ] }

    // todo 5: continue with the test once we have the journal builder
    <@ true @>
