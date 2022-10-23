module fledger.ParsingJournal

open FParsec

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingDefaultCommodity
open fledger.ParsingTransactions


let pJournalItem: Parser<JournalItem option, UserState> =
    (pTx |>> Transaction |>> Some)
    <|> (pDefaultCommodity |>> Some)
    // <|> (pEmptyLine |>> fun () -> None)
    <?> "journal item"

let pJournal: Parser<Journal, UserState> =
    many pJournalItem .>> eof
    |>> fun items ->
            let actualItems = filterOutNone items
            { Items = actualItems }
    <?> "journal"
