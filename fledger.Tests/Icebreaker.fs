﻿module fledger.Icebreaker

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
    let spacesTabs: Parser<string, unit> = manyChars (pchar ' ' <|> pchar '\t')

    let spacesTabs1: Parser<string, unit> =
        many1Chars (pchar ' ' <|> pchar '\t')

    let pYear = pint32 .>> pstring "/" <?> "year"
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
            spacesTabs
            (restOfLine true)
            (fun _ _ desc -> (TransactionState.Pending, desc))

    let pClearedTxDesc =
        pipe3
            (pstring "*")
            spacesTabs
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
            spacesTabs1
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
        <?> "account name character "

    let pAmountSeparator =
        (pstring " " .>> (pstring " ") |> attempt
         <?> "amount separator")

    let pAccount: Parser<string, unit> =
        many1CharsTill pAccountChar pAmountSeparator
        <?> "account name"

    let pAccountRef =
        spacesTabs1 >>. pAccount .>> spacesTabs
        <?> "account reference"

    let pAmountValue =
        numberLiteral NumberLiteralOptions.DefaultFloat "amount"
        |>> (fun num -> Decimal.Parse(num.String, CultureInfo.InvariantCulture))
        <?> "amount value"

    let pCurrencyChar = letter

    let pCurrency: Parser<string, unit> =
        many1Chars pCurrencyChar <?> "currency"

    let pAmount: Parser<decimal * string option, unit> =
        pAmountValue
        .>>. ((spacesTabs1 >>. pCurrency .>> (restOfLine true)
               |>> Some)
              <|> ((restOfLine true) >>% None))
        <?> "amount"


    let pPostingLine =
        pipe2
            pAccountRef
            pAmount
            (fun account (amount, currency) ->
                { Account = account
                  Amount = amount
                  Currency = currency })

    let pPostingLines: Parser<PostingLine list, unit> = many pPostingLine

    let pTx =
        pTxFirstLine .>>. pPostingLines
        |>> (fun (info, postings) -> { Info = info; Postings = postings })

module Tests =
    let sample =
        @"2022/01/06 *s.p. prispevki
      expenses:Business:Service charges    0.39 EUR
      expenses:Business:Employment Costs    4.25
      assets:current assets:Sparkasse    -4.64 EUR
"

    type LedgerParsingTests(output: ITestOutputHelper) =
        [<Fact>]
        member this.icebreaker() =
            let expectedTransaction =
                { Info =
                      { Date = DateTime(2022, 1, 6)
                        State = TransactionState.Cleared
                        Description = "s.p. prispevki" }
                  Postings =
                      [ { Account = "expenses:Business:Service charges"
                          Amount = 0.39m
                          Currency = Some "EUR" }
                        { Account = "expenses:Business:Employment Costs"
                          Amount = 4.25m
                          Currency = None }
                        { Account = "assets:current assets:Sparkasse"
                          Amount = -4.64m
                          Currency = Some "EUR" } ] }

            let result = run JournalParsing.pTx sample

            match result with
            | Success (tx, _, _) -> test <@ tx = expectedTransaction @>
            | Failure (err, _, _) ->
                output.WriteLine $"%s{err}\n"
                test <@ false @>
