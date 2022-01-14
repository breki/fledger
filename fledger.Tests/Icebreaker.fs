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

type LedgerParsingTests(output: ITestOutputHelper) =
    [<Fact>]
    member this.icebreaker() =
        let pyear = pint32 .>> pstring "/"
        let pmonth = pint32 .>> pstring "/"
        let pday = pint32

        let pdate =
            pipe3
                pyear
                pmonth
                pday
                (fun year month day -> DateTime(year, month, day))

        let pTxFirstLine =
            pipe3
                pdate
                spaces1
                (restOfLine true)
                (fun date _ description ->
                    let (state, description) =
                        match description.[0] with
                        | '!' ->
                            (TransactionState.Pending, description.Substring(1))
                        | '*' ->
                            (TransactionState.Cleared, description.Substring(1))
                        | _ -> (TransactionState.Unmarked, description)

                    { Date = date
                      State = state
                      Description = description.Trim() })

        let pTx = pTxFirstLine
        let result = run pTx sample

        match result with
        | Success (tx, _, _) ->
            test <@ tx.Date = DateTime(2022, 1, 6) @>
            test <@ tx.State = TransactionState.Cleared @>
            test <@ tx.Description = "s.p. prispevki" @>
        | Failure (err, _, _) ->
            output.WriteLine $"%s{err}\n"
            test <@ false @>
