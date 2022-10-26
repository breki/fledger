module fledger.Parsing.ParsingBasics

open System

open FParsec

// This is just a placeholder for the user state, if we ever need it.
type UserState = { Something: int }

let filterOutNone items =
    items
    |> (List.filter Option.isSome)
    |> List.map Option.get

let trimStringOptional (s: string option) =
    match s with
    | Some s ->
        s.Trim()
        |> function
            | "" -> None
            | trimmed -> Some trimmed
    | None -> None

let BP (p: Parser<_, _>) stream = p stream // set a breakpoint here

let whitespace<'T> : Parser<string, 'T> =
    manyChars (pchar ' ' <|> pchar '\t')
    <??> "whitespace"

let whitespace1<'T> : Parser<string, 'T> =
    many1Chars (pchar ' ' <|> pchar '\t')
    <??> "whitespace 1"

let newlineOrEof<'T> : Parser<unit, 'T> =
    (newline |>> fun _ -> ()) <|> eof
    <??> "newline or eof"

let endOfLineWhitespace<'T> : Parser<unit, 'T> =
    whitespace >>. newlineOrEof >>% ()
    <??> "end of line whitespace"

let pEmptyLine<'T> : Parser<char, 'T> =
    whitespace >>. newline <??> "empty line"

let pDatePartSeparator<'T> : Parser<char, 'T> =
    pchar '/' <|> pchar '-' <??> "date part separator"

let pYear<'T> : Parser<int, 'T> =
    pint32 .>> pDatePartSeparator <??> "year"

let pMonth<'T> : Parser<int, 'T> =
    pint32 .>> pDatePartSeparator <??> "month"

let pDay<'T> : Parser<int, 'T> =
    pint32 <??> "day"

let pDate<'T> : Parser<DateTime, 'T> =
    pipe3 pYear pMonth pDay (fun year month day -> DateTime(year, month, day))
    <??> "date"

// todo 10: create AccountRef type that has the full name and decomposed parts
let pAccountChar<'T> : Parser<char, 'T> =
    choice [ letter
             digit
             pchar ':'
             pchar '-'
             pchar '_'
             pchar '/'
             pchar ' ' ]
    <??> "account name character "
