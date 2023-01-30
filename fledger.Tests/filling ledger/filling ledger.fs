module fledger.Tests.filling_ledger.filling_ledger

open System
open fledger.Journal
open Xunit
open fledger.LedgerTypes
open fledger.LedgerFilling
open fledger.Tests.JournalBuilders
open Swensen.Unquote


let shouldBeOk =
    function
    | Result.Error errors ->
        failwith (String.concat "," (errors |> List.map (fun x -> x.Message)))
    | Result.Ok _ -> test <@ true @>


[<Fact>]
let ``automatically adds a missing commodity in DefaultCommodity directive``
    ()
    =
    let journal = { Items = [ 14L, defaultCommodityDirective "EUR" ] }

    fillLedger journal |> shouldBeOk


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
let ``reports duplicate account declarations`` () =
    let journal =
        { Items =
            [ 12L, withAccountDirective "acc1"
              13L, withAccountDirective "acc1" ] }

    match fillLedger journal with
    | Result.Error errors ->
        test
            <@
                errors = [ { Message = "Duplicate 'acc1' account declaration."
                             Line = 13L } ]
            @>
    | Result.Ok _ -> failwith "should not be ok"

[<Fact>]
let ``reports market price not in chronological order`` () =
    let journal =
        { Items =
            [ 12L, commodity "EUR"
              13L, commodity "USD"
              14L, marketPriceDirective () |> onDate (DateTime(2023, 01, 19))
              15L, marketPriceDirective () |> onDate (DateTime(2023, 01, 18)) ] }

    match fillLedger journal with
    | Result.Error errors ->
        test
            <@
                errors = [ { Message =
                               "Market price on date 2023/01/18 is not in chronological order."
                             Line = 15L } ]
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
                  p
                  |> withAmount 0m (Some "EUR")
                  |> withTotalPrice 0m "USD"
                  |> withExpectedBalance 0m "GBP"))
              |> (withPostingLine "acc1" (fun p ->
                  p
                  |> withAmount 0m (Some "EUR")
                  |> withTotalPrice 0m "USD"
                  |> withExpectedBalance 0m "GBP"))
              |> fledger.Journal.Transaction ] }

    match fillLedger journal with
    | Result.Error errors ->
        test
            <@
                errors = [ { Message = "Account 'acc1' not defined."
                             Line = 14L }
                           { Message = "Commodity 'EUR' not defined."
                             Line = 14L }
                           { Message = "Commodity 'GBP' not defined."
                             Line = 14L }
                           { Message = "Commodity 'USD' not defined."
                             Line = 14L } ]
            @>
    | Result.Ok _ -> failwith "should not be ok"

[<Fact>]
let ``reports transaction is not in chronological order`` () =
    let journal =
        { Items =
            [ 14L,
              withTransaction ()
              |> txOnDate (DateTime(2018, 1, 2))
              |> fledger.Journal.Transaction
              15L,
              withTransaction ()
              |> txOnDate (DateTime(2018, 1, 1))
              |> fledger.Journal.Transaction ] }

    match fillLedger journal with
    | Result.Error errors ->
        test
            <@
                errors = [ { Message =
                               "Transaction on date 2018/01/01 is not in chronological order."
                             Line = 15L } ]
            @>
    | Result.Ok _ -> failwith "should not be ok"

[<Fact>]
let ``reports unbalanced transaction commodities`` () =
    let journal =
        { Items =
            [ 11L, defaultCommodityDirective "EUR"
              13L, withAccountDirective "acc1"
              14L,
              withTransaction ()
              |> withPostingLine "acc1" (fun p -> p |> withAmount 10m None)
              |> withPostingLine "acc1" (fun p -> p |> withAmount -5m None)
              |> fledger.Journal.Transaction ] }

    match fillLedger journal with
    | Result.Error errors ->
        test
            <@
                errors = [ { Message =
                               "Transaction is unbalanced for commodity: 5.00 EUR."
                             Line = 14L } ]
            @>
    | Result.Ok _ -> failwith "should not be ok"

[<Fact>]
let ``postings' order is the same as in the journal`` () =
    let journal =
        { Items =
            [ 11L, defaultCommodityDirective "EUR"
              13L, withAccountDirective "acc1"
              14L, withAccountDirective "acc2"
              15L,
              withTransaction ()
              |> withPostingLine "acc1" (fun p -> p |> withAmount 10m None)
              |> withPostingLine "acc2" (fun p -> p |> withNoAmount)
              |> fledger.Journal.Transaction ] }

    let fillLedgerResult = fillLedger journal

    fillLedgerResult |> shouldBeOk

    match fillLedgerResult with
    | Result.Ok ledger ->
        let tx = ledger.Transactions |> Seq.head

        test
            <@
                tx.Postings |> List.map (fun p -> p.Account.FullName) = [ "acc1"
                                                                          "acc2" ]
            @>
    | Result.Error _ -> failwith "should not be error"

[<Fact>]
let ``supports elided posting amounts`` () =
    let journal =
        { Items =
            [ 11L, defaultCommodityDirective "EUR"
              13L, withAccountDirective "acc1"
              14L,
              withTransaction ()
              |> withPostingLine "acc1" (fun p -> p |> withAmount 10m None)
              |> withPostingLine "acc1" (fun p -> p |> withNoAmount)
              |> fledger.Journal.Transaction ] }

    fillLedger journal |> shouldBeOk

[<Fact>]
let ``only one posting can be elided`` () =
    let journal =
        { Items =
            [ 11L, defaultCommodityDirective "EUR"
              13L, withAccountDirective "acc1"
              14L,
              withTransaction ()
              |> withPostingLine "acc1" (fun p -> p |> withAmount 10m None)
              |> withPostingLine "acc1" (fun p -> p |> withNoAmount)
              |> withPostingLine "acc1" (fun p -> p |> withNoAmount)
              |> fledger.Journal.Transaction ] }

    match fillLedger journal with
    | Result.Error errors ->
        test
            <@
                errors = [ { Message =
                               "Could not balance the transaction - cannot "
                               + "have more than one posting without an amount."
                             Line = 14L } ]
            @>
    | Result.Ok _ -> failwith "should not be ok"

[<Fact>]
let ``balancing a transaction takes into account total price 1`` () =
    let journal =
        { Items =
            [ 11L, defaultCommodityDirective "EUR"
              12L, commodity "USD"
              13L, withAccountDirective "acc1"
              14L,
              withTransaction ()
              |> withPostingLine "acc1" (fun p ->
                  p |> withAmount 10m (Some "EUR"))
              |> withPostingLine "acc1" (fun p ->
                  p |> withAmount -8m (Some "USD") |> withTotalPrice 10m "EUR")
              |> fledger.Journal.Transaction ] }

    fillLedger journal |> shouldBeOk

[<Fact>]
let ``balancing a transaction takes into account total price 2`` () =
    let journal =
        { Items =
            [ 11L, defaultCommodityDirective "EUR"
              12L, commodity "USD"
              13L, withAccountDirective "acc1"
              14L,
              withTransaction ()
              |> withPostingLine "acc1" (fun p ->
                  p |> withAmount -10m (Some "EUR"))
              |> withPostingLine "acc1" (fun p ->
                  p |> withAmount 8m (Some "USD") |> withTotalPrice 10m "EUR")
              |> fledger.Journal.Transaction ] }

    fillLedger journal |> shouldBeOk

[<Fact>]
let ``multi-commodity transaction without pricing cannot be balanced`` () =
    let journal =
        { Items =
            [ 11L, defaultCommodityDirective "EUR"
              12L, commodity "USD"
              13L, withAccountDirective "acc1"
              14L,
              withTransaction ()
              |> withPostingLine "acc1" (fun p ->
                  p |> withAmount -10m (Some "EUR"))
              |> withPostingLine "acc1" (fun p ->
                  p |> withAmount 8m (Some "USD"))
              |> fledger.Journal.Transaction ] }

    fillLedger journal |> shouldBeOk
