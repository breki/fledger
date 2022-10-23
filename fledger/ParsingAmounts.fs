module fledger.ParsingAmounts

open System

open System.Globalization
open FParsec

open fledger.Journal
open fledger.ParsingBasics

// number sign = ["+" | "-"]
let pSign =
    (pchar '+' |>> fun _ -> "+")
    <|> (pchar '-' |>> fun _ -> "-")
    <|>% ""
    <?> "sign"

// thousands digits = ",", digit, digit, digit
let pThousandsDigits =
    pchar ',' >>. parray 3 digit
    |>> fun digits -> digits |> String
    <?> "thousands digits"

// digits many = {digit}
let pDigitsMany: Parser<string, UserState> =
    many digit
    |>> fun digits -> digits |> List.toArray |> String
    <?> "digits 0 or more"

// digits many 1 = digit, {digit}
let pDigitsMany1 =
    many1 digit
    |>> fun digits -> digits |> List.toArray |> String
    <?> "digits 1 or more"

// thousands part = [digits many 1], {thousands digits}
let pThousandsPart =
    pDigitsMany1 .>>. parray 1 pThousandsDigits
    |>> fun (digits, thousandsParts) ->
            let thousandsStr =
                thousandsParts |> String.concat ""

            digits + thousandsStr
    <?> "thousands part"


// decimal part = ".", {digit}
let pDecimalPart =
    pchar '.' >>. pDigitsMany <?> "decimal part"
    |>> (fun digits -> "." + digits)

// number = [number sign], thousands part | digits many, [decimal part]
let numberLiteral2 =
    pipe3
        pSign
        (pThousandsPart <|> pDigitsMany)
        pDecimalPart
        (fun sign integerPart decimalPart -> sign + integerPart + decimalPart)
    <?> "number"

let pAmountValue =
    numberLiteral2
    |>> (fun num -> Decimal.Parse(num, CultureInfo.InvariantCulture))
    <?> "amount value"

let pCurrencyChar = letter

let pCurrency: Parser<string, UserState> =
    many1Chars pCurrencyChar <?> "currency"

let pAmountCurrency =
    (whitespace1 >>. pCurrency) |> attempt
    <?> "amount currency"

let pAmount: Parser<Amount, UserState> =
    pipe2 pAmountValue (opt pAmountCurrency) (fun amount currency ->
        match currency with
        | Some currency ->
            { Value = amount
              Currency = Some currency }
        | None -> { Value = amount; Currency = None })
    <?> "amount"
