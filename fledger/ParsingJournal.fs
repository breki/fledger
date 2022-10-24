module fledger.ParsingJournal

open FParsec

open fledger.Journal
open fledger.ParsingBasics
open fledger.ParsingDefaultCommodity
open fledger.ParsingTransactions


let pJournalItem<'T> : Parser<JournalItem option, 'T> =
    (pTx |>> Transaction |>> Some)
    <|> (pDefaultCommodity |>> Some)
    <|> (pEmptyLine |>> fun _ -> None)
    <??> "journal item"

let pJournal<'T> : Parser<Journal, 'T> =
    many pJournalItem .>> eof
    |>> fun items ->
            let actualItems = filterOutNone items
            { Items = actualItems }
    <??> "journal"
