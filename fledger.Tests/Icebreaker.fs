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

type TransactionDescription =
    | Description of string
    | PayeeNote of (string * string)

type TransactionInfo =
    { Date: DateTime
      State: TransactionState
      Description: TransactionDescription }

type Amount = { Value: Decimal; Currency: string }

type PostingLine =
    { Account: string
      Amount: Amount
      TotalPrice: Amount option
      ExpectedBalance: Amount option }

type Transaction =
    { Info: TransactionInfo
      Postings: PostingLine list }

module JournalParsing =
    let spacesTabs: Parser<string, unit> =
        manyChars (pchar ' ' <|> pchar '\t')
        <?> "space or tab"

    let spacesTabs1: Parser<string, unit> =
        many1Chars (pchar ' ' <|> pchar '\t')
        <?> "space or tab"

    let endOfLineWhitespace: Parser<unit, unit> = spacesTabs >>. newline >>% ()

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
                  Description = Description(description.Trim()) })

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

    let pAmountCurrency =
        (spacesTabs1 >>. pCurrency) |> attempt
        <??> "amount currency"

    let pAmount: Parser<Amount, unit> =
        pipe2
            pAmountValue
            (opt pAmountCurrency)
            (fun amount currency ->
                match currency with
                | Some currency -> { Value = amount; Currency = currency }
                | None -> { Value = amount; Currency = "EUR" })
        <??> "amount"

    let pTotalPriceIndicator =
        pstring "@@" >>% ()
        <?> "total price indicator (@@)"

    let pTotalPrice =
        (spacesTabs1 >>? pTotalPriceIndicator
         .>>? spacesTabs1
         >>. pAmount)
        |> attempt
        <??> "total price"

    let pExpectedBalance =
        (pchar ' ' >>. spacesTabs1 >>? (pstring "=")
         .>> spacesTabs1
         >>. pAmount)
        |> attempt
        <??> "expected balance"

    let pPostingLine =
        pipe5
            pAccountRef
            pAmount
            (opt pTotalPrice)
            (opt pExpectedBalance)
            endOfLineWhitespace
            (fun account amount totalPrice expectedBalance _ ->
                { Account = account
                  Amount = amount
                  TotalPrice = totalPrice
                  ExpectedBalance = expectedBalance })
        <??> "posting line"

    let pPostingLines: Parser<PostingLine list, unit> = many pPostingLine

    let pTx =
        pTxFirstLine .>>. pPostingLines
        |>> (fun (info, postings) -> { Info = info; Postings = postings })

module Tests =
    type RandomTransaction =
        { TransactionText: string
          Description: TransactionDescription }

    let randomTransaction () =
        gen {
            let transactionDescription = Description("s.p. prispevki")

            let text =
                @"2022/01/06 *s.p. prispevki
              expenses:Business:Service charges    0.39 EUR
              expenses:Business:Employment Costs    4.25  @@ 12.20 USD
              assets:current assets:Sparkasse    -4.64 EUR  = 132.55 EUR
        "


            return
                { TransactionText = text
                  Description = transactionDescription }
        }

    let transactionIsParsedCorrectly
        (output: ITestOutputHelper)
        (randomTransaction: RandomTransaction)
        =
        let result =
            run JournalParsing.pTx randomTransaction.TransactionText

        match result with
        | Success (tx, _, _) ->
            tx.Info.Description = randomTransaction.Description
        | Failure (err, _, _) ->
            output.WriteLine $"%s{err}\n"
            false

    let sample =
        @"2022/01/06 *s.p. prispevki
      expenses:Business:Service charges    0.39 EUR
      expenses:Business:Employment Costs    4.25  @@ 12.20 USD
      assets:current assets:Sparkasse    -4.64 EUR  = 132.55 EUR
"

    type LedgerParsingTests(output: ITestOutputHelper) =
        [<Fact>]
        member this.``transaction parsing``() =
            let arbTransaction = randomTransaction () |> Arb.fromGen

            (transactionIsParsedCorrectly output)
            |> Prop.forAll arbTransaction
            |> Check.QuickThrowOnFailure

            let expectedTransaction =
                { Info =
                      { Date = DateTime(2022, 1, 6)
                        State = TransactionState.Cleared
                        Description = Description("s.p. prispevki") }
                  Postings =
                      [ { Account = "expenses:Business:Service charges"
                          Amount = { Value = 0.39m; Currency = "EUR" }
                          TotalPrice = None
                          ExpectedBalance = None }
                        { Account = "expenses:Business:Employment Costs"
                          Amount = { Value = 4.25m; Currency = "EUR" }
                          TotalPrice = Some { Value = 12.2m; Currency = "USD" }
                          ExpectedBalance = None }
                        { Account = "assets:current assets:Sparkasse"
                          Amount = { Value = -4.64m; Currency = "EUR" }
                          TotalPrice = None
                          ExpectedBalance =
                              Some { Value = 132.55m; Currency = "EUR" } } ] }

            let result = run JournalParsing.pTx sample

            match result with
            | Success (tx, _, _) -> test <@ tx = expectedTransaction @>
            | Failure (err, _, _) ->
                output.WriteLine $"%s{err}\n"
                test <@ false @>
