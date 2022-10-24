module fledger.``parsing tx first line``


open System
open Xunit
open FsCheck

open Xunit.Abstractions

open fledger.Journal
open fledger.ParsingTransactions
open fledger.Tests.ParsingUtils


// todo 30: randomize parsing of transaction first line instead of
//   the whole journal
type TxFirstLineParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.``parsing transaction info with description and comment``() =
        let text =
            "2022/01/06 *description ; comment"

        let expectedValue =
            { Date = DateTime(2022, 1, 6)
              Status = TransactionStatus.Cleared
              Description = Some "description"
              Payee = None
              Note = None
              Comment = Some "comment" }

        testParser pTxFirstLine output text expectedValue

    [<Fact>]
    member this.``parsing transaction info with payee``() =
        let text =
            "2022/01/06 *description |Third Wind ; comment"

        let expectedValue =
            { Date = DateTime(2022, 1, 6)
              Status = TransactionStatus.Cleared
              Description = Some "description"
              Payee = Some "Third Wind"
              Note = None
              Comment = Some "comment" }

        testParser pTxFirstLine output text expectedValue

    [<Fact>]
    member this.``parsing transaction info with payee and note``() =
        let text =
            "2022/01/06 *description |Third Wind | this is a note ; comment"

        let expectedValue =
            { Date = DateTime(2022, 1, 6)
              Status = TransactionStatus.Cleared
              Description = Some "description"
              Payee = Some "Third Wind"
              Note = Some "this is a note"
              Comment = Some "comment" }

        testParser pTxFirstLine output text expectedValue
