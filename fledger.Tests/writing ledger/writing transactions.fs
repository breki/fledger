module fledger.Tests.writing_ledger.writing_transactions

open System
open System.Globalization
open System.IO
open Xunit
open FsCheck
open FParsec

open Xunit.Abstractions
open fledger.BasicTypes
open fledger.LedgerTypes
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingJournal
open fledger.LedgerFilling

// todo 1: the padding between the account and the amount is weird and
// inconsistent - make it a testable property

// todo 8: add more properties of the written transaction:
//   - the order of postings
//   - which posting(s) are elided?
// todo 10: document

/// Describes the current state of writing of the ledger. The functions that
/// write ledger entries should return an updated state.
type WritingState = { DefaultCommodity: Commodity option }

/// Converts the amount to a string to be written to a journal file,
/// making sure the default commodity is not explicitly written.
let amountForJournal (amount: Amount option) prefix state =
    match amount with
    | None -> ""
    | Some amount ->
        let amountValueText =
            if Math.Round(amount.Value) = amount.Value then
                String.Format(CultureInfo.InvariantCulture, "{0}", amount.Value)
            else
                String.Format(
                    CultureInfo.InvariantCulture,
                    "{0:0.00}",
                    amount.Value
                )

        match state.DefaultCommodity with
        | None ->
            String.Format(
                CultureInfo.InvariantCulture,
                "{0}{1} {2}",
                prefix,
                amountValueText,
                amount.Commodity
            )
        | Some defaultCommodity ->
            if amount.Commodity = defaultCommodity then
                String.Format(
                    CultureInfo.InvariantCulture,
                    "{0}{1}",
                    prefix,
                    amountValueText
                )
            else
                String.Format(
                    CultureInfo.InvariantCulture,
                    "{0}{1} {2}",
                    prefix,
                    amountValueText,
                    amount.Commodity
                )


/// Writes a transaction to a text stream.
let writeTransaction
    (tx: Transaction)
    (state: WritingState)
    (writer: TextWriter)
    =
    writer.Write tx.DateStr

    let description = tx.FullDescription

    let descriptionMaybe =
        match description with
        | "" -> None
        | _ -> Some description

    match tx.Status, descriptionMaybe with
    | Cleared, Some description -> writer.Write $" *{description}"
    | Cleared, None -> writer.Write " *"
    | Pending, Some description -> writer.Write $" !{description}"
    | Pending, None -> writer.Write " !"
    | Unmarked, Some description -> writer.Write $" {description}"
    | Unmarked, None -> ()

    writer.WriteLine()

    // calculate the maximum length of the account names
    let accountNameMaxLen =
        tx.Postings
        |> List.map (fun posting -> posting.Account.FullName.Length)
        |> List.max

    // generate amount texts for all postings
    let postingAmountsTexts =
        tx.Postings
        |> List.map (fun posting ->
            amountForJournal (Some posting.Amount) "" state)

    // calculate the maximum length of the amount texts
    let amountIntegerMaxLen =
        postingAmountsTexts
        |> List.map (fun amountText ->
            let decimalPointPos = amountText.IndexOf('.')

            match decimalPointPos with
            | x when x >= 0 -> x
            | _ -> amountText.Length)
        |> List.max

    // how much should the account names be right-padded?
    let accountPaddedLength = accountNameMaxLen + 2

    // write the postings
    tx.Postings
    |> List.zip postingAmountsTexts
    |> List.iter (fun (amountText, posting) ->
        writer.Write "    "
        writer.Write(posting.Account.FullName.PadRight(accountPaddedLength))

        let amountDecimalPos = amountText.IndexOf('.')

        match amountDecimalPos with
        | x when x >= 0 ->
            let leftPaddingNeeded = amountIntegerMaxLen - x
            writer.Write("".PadLeft(leftPaddingNeeded))
            writer.Write amountText
        | _ ->
            let commodityDelimitedPos = amountText.LastIndexOf(' ')

            match commodityDelimitedPos with
            | x when x >= 0 ->
                let leftPaddingNeeded = amountIntegerMaxLen - x
                writer.Write("".PadLeft(leftPaddingNeeded))
                writer.Write amountText
            | _ -> writer.Write(amountText.PadLeft(amountIntegerMaxLen))

        let totalPriceText = amountForJournal posting.TotalPrice " @@ " state
        writer.Write totalPriceText

        let expectedBalanceText =
            amountForJournal posting.ExpectedBalance " = " state

        writer.WriteLine expectedBalanceText)

    state

/// A helper test method to write a transaction to a string.
let writeTransactionToString tx ledger =
    let stream = new MemoryStream()
    let writer = new StreamWriter(stream)

    writer |> writeTransaction tx ledger |> ignore
    writer.Flush()

    stream.Seek(0L, SeekOrigin.Begin) |> ignore

    let reader = new StreamReader(stream)
    reader.ReadToEnd()

/// Generate a text with one or two words or None.
let arbitraryFewWords () =
    gen {
        let! fewWords =
            Gen.elements [ Some "something"; Some "something else"; None ]

        return fewWords
    }

let defaultCommodity = "EUR"

/// Creates an initial ledger with the commodities, the default commodity and
/// accounts. This ledger is then used as a parameter to the transaction
/// writing function.
let initialLedger journalText =
    let journalParserResult =
        runParserOnString pJournal { Something = 0 } "test stream" journalText

    match journalParserResult with
    | Success (journal, _, _) ->
        match fillLedger journal with
        | Result.Ok ledger -> ledger
        | Result.Error errors ->
            failwith (
                "the initial ledger is faulty: "
                + (String.concat "," (errors |> List.map (fun x -> x.Message)))
            )
    | Failure (errorMsg, _, _) ->
        failwith ("the initial ledger is faulty:" + errorMsg)

let chooseArbitraryTransaction () =
    gen {
        // add the commodity, default commodity and accounts lines
        // to the journal string

        let journalPrefixText =
            $@"D 1,000.00 {defaultCommodity}
commodity EUR
commodity USD
account assets:assets1 
account expenses:expenses1
"

        let! status = Arb.from<TransactionStatus>.Generator
        let! description = arbitraryFewWords ()
        let! payee = arbitraryFewWords ()
        let! note = arbitraryFewWords ()
        let! comment = arbitraryFewWords ()

        let! amountValue = Gen.elements [ 0m; 100m; 100.2m; 100.23m ]
        let! amountCommodity = Gen.elements [ "EUR"; "USD" ]

        let! hasTotalPrice = Arb.from<bool>.Generator

        let totalPrice =
            if hasTotalPrice then Some(Amount.Of "USD" 50.23m) else None

        let! hasExpectedBalance = Arb.from<bool>.Generator

        let amount = Amount.Of amountCommodity amountValue

        let posting1 =
            { Account = AccountRef.Create "assets:assets1"
              Amount = amount
              TotalPrice = totalPrice
              ExpectedBalance = if hasExpectedBalance then Some amount else None }

        let posting2 =
            { Account = AccountRef.Create "expenses:expenses1"
              Amount =
                match totalPrice with
                | Some totalPrice -> -totalPrice
                | None -> Amount.Of amountCommodity -amountValue
              TotalPrice = None
              ExpectedBalance = None }

        let postings = [ posting1; posting2 ]

        let tx =
            { Date = DateTime(2023, 1, 7)
              Status = status
              Description = description
              Payee = payee
              Note = note
              Comment = comment
              Postings = postings
              Line = 6L }

        let writingState = { DefaultCommodity = Some defaultCommodity }

        let txText = writeTransactionToString tx writingState
        let journalText = journalPrefixText + txText

        // parse the whole journal
        let journalParserResult =
            runParserOnString
                pJournal
                { Something = 0 }
                "test stream"
                journalText

        return
            match journalParserResult with
            | Success (journal, _, _) ->
                match fillLedger journal with
                | Result.Ok ledger ->
                    // extract the transaction from the ledger
                    let parsedTx = ledger.Transactions.Head

                    // do the original and parsed transactions match?
                    if parsedTx = tx then
                        tx, Some parsedTx, txText, None
                    else
                        tx,
                        Some parsedTx,
                        txText,
                        (Some
                            "Parsed transaction is not equal to the original one")

                | Result.Error errors ->
                    let ledgerFillingErrors =
                        (String.concat
                            ","
                            (errors |> List.map (fun x -> x.Message)))

                    tx, None, txText, (Some ledgerFillingErrors)
            | Failure (errorMsg, _, _) -> tx, None, txText, (Some errorMsg)
    }


let propTransactionIsWrittenCorrectly
    (
        originalTx: Transaction,
        parsedTx: Transaction option,
        txString: string,
        errors,
        output: ITestOutputHelper
    ) =
    match errors with
    | None ->
        output.WriteLine "SUCCESS:"
        output.WriteLine txString
        true
    | Some errorMsg ->
        output.WriteLine "Original transaction:"
        output.WriteLine $"{originalTx}"

        match parsedTx with
        | Some parsedTx ->
            output.WriteLine "Parsed transaction:"
            output.WriteLine $"{parsedTx}"
        | None -> ()

        output.WriteLine "Transaction string:"
        output.WriteLine txString
        output.WriteLine $"ERROR: {errorMsg}"
        false

let propNumberOfLinesIsAsExpected
    (
        originalTx: Transaction,
        _: Transaction option,
        txString: string,
        errors,
        output: ITestOutputHelper
    ) =
    match errors with
    | None ->
        let linesProduced = txString.Split(Environment.NewLine).Length
        let expectedNumberOfLines = 1 + originalTx.Postings.Length + 1

        if linesProduced = expectedNumberOfLines then
            true
        else
            output.WriteLine $"Transaction string: {txString}"

            output.WriteLine(
                $"ERROR: Expected {expectedNumberOfLines} lines, "
                + $"but got {linesProduced}"
            )

            false
    | Some _ -> true

let propDefaultCommodityIsNotWritten
    (
        _: Transaction,
        _: Transaction option,
        txString: string,
        errors,
        output: ITestOutputHelper
    ) =
    match errors with
    | None ->
        if not (txString.Contains(defaultCommodity)) then
            true
        else
            output.WriteLine $"Transaction string: {txString}"

            output.WriteLine(
                "ERROR: the default commodity is explicitly written, "
                + "but it should not be."
            )

            false
    | Some _ -> true

let propDecimalsAreSkippedIfTheyAreZero
    (
        _: Transaction,
        _: Transaction option,
        txString: string,
        errors,
        output: ITestOutputHelper
    ) =
    match errors with
    | None ->
        if
            not (
                txString.Contains(".00")
                || txString.Contains(". ")
                || txString.Contains($".{Environment.NewLine}")
            )
        then
            true
        else
            output.WriteLine $"Transaction string: {txString}"

            output.WriteLine(
                "ERROR: amounts with '.00' should not have decimals written"
            )

            false
    | Some _ -> true

let propAmountsAreAlignedToTheDecimal
    (
        originalTx: Transaction,
        _: Transaction option,
        txString: string,
        errors,
        output: ITestOutputHelper
    ) =
    match errors with
    | None ->
        let lines = txString.Split(Environment.NewLine)

        // remove total prices and expected balances from lines since they
        // do not affect the alignment
        let lines =
            lines
            |> Array.map (fun line ->
                let splits = line.Split("@@")
                let splits = splits[ 0 ].Split("=")
                splits[ 0 ].TrimEnd())

        // find distinct decimal positions for the lines
        let decimalPositions =
            lines
            // skip the first line of the transaction
            |> Array.skip 1
            // and also the last (empty) line
            |> Array.take originalTx.Postings.Length
            |> Array.map (fun line ->
                match line.IndexOf('.') with
                | pos when pos >= 0 -> pos
                | _ ->
                    // if there is no decimal point, then we need to determine
                    // whether the amount has a commodity, by checking the
                    // last character of the line
                    if Char.IsLetter(line.[line.Length - 1]) then
                        // if the last character is a letter, then the amount
                        // has a commodity, and we will use the space between
                        // the amount and the commodity as the alignment position
                        line.LastIndexOf ' '
                    else
                        // if the last character is not a letter, then
                        // we only have an amount value and the alignment
                        // position will be the last character + 1
                        line.Length)
            |> Array.distinct

        if decimalPositions.Length = 1 then
            true
        else
            output.WriteLine $"Transaction string: {txString}"

            output.WriteLine(
                "ERROR: posting amounts are not aligned to the decimal"
            )

            false
    | Some _ -> true

type TxWritingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``transaction writing properties``() =
        let arbTransaction = chooseArbitraryTransaction () |> Arb.fromGen

        let props (originalTx, parsedTx, txString, errors) =
            let propsWithOutput =
                (originalTx, parsedTx, txString, errors, output)

            (propTransactionIsWrittenCorrectly propsWithOutput
             .&. propNumberOfLinesIsAsExpected propsWithOutput
             .&. propDefaultCommodityIsNotWritten propsWithOutput
             .&. propDecimalsAreSkippedIfTheyAreZero propsWithOutput
             .&. propAmountsAreAlignedToTheDecimal propsWithOutput)

        props |> Prop.forAll arbTransaction |> Check.QuickThrowOnFailure
