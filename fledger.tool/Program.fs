open System.IO
open FParsec
open fledger.Parsing.ParsingBasics
open fledger.Parsing.ParsingJournal

let text =
    File.ReadAllText(@"D:\ledger\igor.ledger")

let result =
    runParserOnString pJournal { Something = 0 } "test stream" text

match result with
| Success (journal, _, _) -> printfn "Success"
| Failure (errorMsg, _, _) -> printfn $"PARSING ERROR: {errorMsg}"
