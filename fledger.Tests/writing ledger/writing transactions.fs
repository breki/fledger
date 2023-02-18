module fledger.Tests.writing_ledger.writing_transactions

open System
open System.IO
open Xunit
open FsCheck

open Xunit.Abstractions
open fledger.LedgerTypes
open fledger.Tests.LedgerBuilders

// todo 5: implement parsing of the resulting text and compare
//   the parsed transaction with the original one
// todo 8: add more properties of the written transaction:
//   - do not output the default commodity
//   - the order of postings
//   - which posting(s) are elided?
//   - right-alignment of the posting amounts
//   - do not output .00 decimals if all amounts are rounded

let writeTransaction (tx: Transaction) (writer: TextWriter) =
    writer.WriteLine(tx.ToString())

let chooseArbitraryTransaction () =
    gen {
        let tx = withTransaction () |> onDate (DateTime(2023, 02, 17))

        let stream = new MemoryStream()
        let writer = new StreamWriter(stream)

        let tx = withTransaction () |> onDate (DateTime(2023, 02, 17))

        writer |> writeTransaction tx
        writer.Flush()

        stream.Seek(0L, SeekOrigin.Begin) |> ignore

        let reader = new StreamReader(stream)
        let value = reader.ReadToEnd()

        let expectedValue =
            @"2023/02/17 

"

        return tx, value, expectedValue
    }


type TxWritingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``icebreaker``() =
        let arbTransaction = chooseArbitraryTransaction () |> Arb.fromGen

        let transactionIsWrittenCorrectly (tx, value: string, expectedValue) =
            if value = expectedValue then
                true
            else
                output.WriteLine("Expected: {0}", expectedValue)
                output.WriteLine("Actual: {0}", value)
                false

        transactionIsWrittenCorrectly
        |> Prop.forAll arbTransaction
        |> Check.QuickThrowOnFailure
