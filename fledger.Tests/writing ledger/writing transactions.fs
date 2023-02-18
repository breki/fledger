module fledger.Tests.writing_ledger.writing_transactions

open System
open System.IO
open Xunit
open FsCheck
open FParsec

open Xunit.Abstractions
open fledger.LedgerTypes
open fledger.Tests.LedgerBuilders
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingJournal
open fledger.LedgerFilling

// todo 5: implement parsing of the resulting text and compare
//   the parsed transaction with the original one
// todo 8: add more properties of the written transaction:
//   - do not output the default commodity
//   - the order of postings
//   - which posting(s) are elided?
//   - right-alignment of the posting amounts
//   - do not output .00 decimals if all amounts are rounded
// todo 10: document

let writeTransaction (tx: Transaction) (writer: TextWriter) =
    writer.WriteLine(tx.ToString())

let writeTransactionToString tx =
    let stream = new MemoryStream()
    let writer = new StreamWriter(stream)

    writer |> writeTransaction tx
    writer.Flush()

    stream.Seek(0L, SeekOrigin.Begin) |> ignore

    let reader = new StreamReader(stream)
    reader.ReadToEnd()


let chooseArbitraryTransaction () =
    gen {
        let tx = withTransaction () |> onDate (DateTime(2023, 02, 17))

        let txText = writeTransactionToString tx

        // todo 1: add the commodity, default commodity and accounts lines
        // to the journal string
        let journalText = txText

        // parse the transaction as a journal
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


type TxWritingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``transaction writing properties``() =
        let arbTransaction = chooseArbitraryTransaction () |> Arb.fromGen

        let transactionIsWrittenCorrectly
            (
                originalTx: Transaction,
                parsedTx: Transaction option,
                txString: string,
                errors
            ) =
            match errors with
            | None -> true
            | Some errorMsg ->
                output.WriteLine $"Original transaction: {originalTx}"

                match parsedTx with
                | Some parsedTx ->
                    output.WriteLine $"Parsed transaction: {parsedTx}"
                | None -> ()

                output.WriteLine $"Transaction string: {txString}"
                output.WriteLine $"ERROR: {errorMsg}"
                false

        transactionIsWrittenCorrectly
        |> Prop.forAll arbTransaction
        |> Check.QuickThrowOnFailure
