module fledger.Icebreaker

open System
open System.Globalization
open Xunit
open Swensen.Unquote
open FsCheck

open FParsec
open Xunit.Abstractions

type TransactionState =
    | Unmarked
    | Pending
    | Cleared

type TransactionInfo =
    { Date: DateTime
      State: TransactionState
      Description: string }

type PostingLine =
    { Account: string
      Amount: Decimal
      Currency: string option }

type Transaction =
    { Info: TransactionInfo
      Postings: PostingLine list }

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

    let pPendingTxDesc: Parser<TransactionState * string, unit> =
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

    let pAccountChar =
        choice [ letter
                 digit
                 pchar ':'
                 pchar '-'
                 pchar '_'
                 pchar ' ' ]

    let pAccount: Parser<string, unit> = manyChars pAccountChar

    let spaces2 = pchar ' ' .>> spaces1
    let pAccountRef = spaces1 >>. pAccount .>> spaces2

    let pAmount =
        numberLiteral NumberLiteralOptions.DefaultFloat "amount"
        |>> (fun num -> Decimal.Parse(num.String, CultureInfo.InvariantCulture))

    let pPostingLine =
        pipe3
            pAccountRef
            pAmount
            (restOfLine true)
            (fun account amount _ ->
                { Account = account
                  Amount = amount
                  Currency = None })


    let pPostingLines: Parser<PostingLine list, unit> = many pPostingLine

    let pTx =
        pTxFirstLine .>>. pPostingLines
        |>> (fun (info, postings) -> { Info = info; Postings = postings })

module Tests =
    let sample =
        @"2022/01/06 *s.p. prispevki
      expenses:Business:Service charges    0.39 EUR
      expenses:Business:Employment Costs    4.25 EUR
      assets:current assets:Sparkasse    -4.64 EUR
    "

    type LedgerParsingTests(output: ITestOutputHelper) =
        [<Fact>]
        member this.icebreaker() =
            let result = run JournalParsing.pTx sample

            match result with
            | Success (tx, _, _) ->
                test <@ tx.Info.Date = DateTime(2022, 1, 6) @>
                test <@ tx.Info.State = TransactionState.Cleared @>
                test <@ tx.Info.Description = "s.p. prispevki" @>
            | Failure (err, _, _) ->
                output.WriteLine $"%s{err}\n"
                test <@ false @>
