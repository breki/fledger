module fledger.ParsingAmounts

open System

open System.Globalization
open FParsec

open fledger.Journal
open fledger.ParsingBasics

let pAmountValue =
    numberLiteral NumberLiteralOptions.DefaultFloat "amount"
    |>> (fun num -> Decimal.Parse(num.String, CultureInfo.InvariantCulture))
    <?> "amount value"

let pCurrencyChar = letter

let pCurrency: Parser<string, unit> =
    many1Chars pCurrencyChar <?> "currency"

let pAmountCurrency =
    (whitespace1 >>. pCurrency) |> attempt
    <?> "amount currency"

let pAmount: Parser<Amount, unit> =
    pipe2 pAmountValue (opt pAmountCurrency) (fun amount currency ->
        match currency with
        | Some currency ->
            { Value = amount
              Currency = Some currency }
        | None -> { Value = amount; Currency = None })
    <?> "amount"
