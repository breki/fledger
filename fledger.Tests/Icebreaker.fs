module fledger.Icebreaker

open System
open Xunit
open Swensen.Unquote
open FsCheck

open FParsec
open Xunit.Abstractions

let sample =
    @"2022/01/06 *s.p. prispevki
  expenses:Business:Service charges    0.39 EUR
  expenses:Business:Employment Costs    4.25 EUR
  assets:current assets:Sparkasse    -4.64 EUR
"

type TransactionState =
    | Unmarked
    | Pending
    | Cleared

type Transaction =
    { Date: DateTime
      State: TransactionState
      Description: string }

module JournalParsing =
    let pYear = pint32 .>> pstring "/"
    let pMonth = pint32 .>> pstring "/"
    let pDay = pint32

    let pDate =
        pipe3
            pYear
            pMonth
            pDay
            (fun year month day -> DateTime(year, month, day))

    let pPendingTxDesc: Parser<(TransactionState * string), unit> =
        pipe3
            (pstring "!")
            spaces
            (restOfLine true)
            (fun _ _ desc -> (TransactionState.Pending, desc))

    let pClearedTxDesc =
        pipe3
            (pstring "*")
            spaces
            (restOfLine true)
            (fun _ _ desc -> (TransactionState.Cleared, desc))

    let pUnmarkedTxDesc =
        (restOfLine true)
        |>> (fun desc -> (TransactionState.Unmarked, desc))

    let pTxStateAndDescription =
        pPendingTxDesc
        <|> pClearedTxDesc
        <|> pUnmarkedTxDesc

    let pTxFirstLine =
        pipe3
            pDate
            spaces1
            pTxStateAndDescription
            (fun date _ (state, description) ->
                { Date = date
                  State = state
                  Description = description.Trim() })

    let pTx = pTxFirstLine

module Tests =
    type LedgerParsingTests(output: ITestOutputHelper) =
        [<Fact>]
        member this.icebreaker() =
            let result = run JournalParsing.pTx sample

            match result with
            | Success (tx, _, _) ->
                test <@ tx.Date = DateTime(2022, 1, 6) @>
                test <@ tx.State = TransactionState.Cleared @>
                test <@ tx.Description = "s.p. prispevki" @>
            | Failure (err, _, _) ->
                output.WriteLine $"%s{err}\n"
                test <@ false @>
