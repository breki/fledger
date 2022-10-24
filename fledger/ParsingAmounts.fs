﻿module fledger.ParsingAmounts

open System

open System.Globalization
open FParsec

open fledger.Journal
open fledger.ParsingBasics

// number sign = ["+" | "-"]
let pSign<'T> : Parser<string, 'T> =
    (pchar '+' |>> fun _ -> "+")
    <|> (pchar '-' |>> fun _ -> "-")
    <|>% ""
    <??> "sign"

// thousands digits = ",", digit, digit, digit
let pThousandsDigits<'T> : Parser<string, 'T> =
    pchar ',' >>. parray 3 digit
    |>> fun digits -> digits |> String
    <??> "thousands digits"

// digits many = {digit}
let pDigitsMany<'T> : Parser<string, 'T> =
    many digit
    |>> fun digits -> digits |> List.toArray |> String
    <??> "digits 0 or more"

// digits many 1 = digit, {digit}
let pDigitsMany1<'T> : Parser<string, 'T> =
    many1 digit
    |>> fun digits -> digits |> List.toArray |> String
    <??> "digits 1 or more"

// thousands part = [digits many 1], {thousands digits}
let pThousandsPart<'T> : Parser<string, 'T> =
    pDigitsMany1 .>>. many1 pThousandsDigits
    |>> fun (digits, thousandsParts) ->
            let thousandsStr =
                thousandsParts |> String.concat ""

            digits + thousandsStr
    <??> "thousands part"

let pIntegerPart<'T> : Parser<string, 'T> =
    (attempt pThousandsPart) <|> pDigitsMany
    <??> "integer part"

// decimal part = ".", {digit}
let pDecimalPart<'T> : Parser<string, 'T> =
    pchar '.' >>. pDigitsMany <??> "decimal part"
    |>> (fun digits -> "." + digits)

// number = [number sign], thousands part | digits many, [decimal part]
let numberLiteral2<'T> : Parser<string, 'T> =
    pipe3
        pSign
        pIntegerPart
        (opt pDecimalPart)
        (fun sign integerPart decimalPart ->
            match decimalPart with
            | Some decimalPart -> sign + integerPart + decimalPart
            | None -> sign + integerPart)
    <??> "number"

let pAmountValue<'T> : Parser<decimal, 'T> =
    numberLiteral2
    |>> (fun num -> Decimal.Parse(num, CultureInfo.InvariantCulture))
    <??> "amount value"

let pCurrencyChar = letter

let pCurrency<'T> : Parser<string, 'T> =
    many1Chars pCurrencyChar <??> "currency"

let pAmountCurrency<'T> : Parser<string, 'T> =
    (whitespace1 >>. pCurrency) |> attempt
    <??> "amount currency"

let pAmount<'T> : Parser<Amount, 'T> =
    pipe2 pAmountValue (opt pAmountCurrency) (fun amount currency ->
        match currency with
        | Some currency ->
            { Value = amount
              Currency = Some currency }
        | None -> { Value = amount; Currency = None })
    <??> "amount"
