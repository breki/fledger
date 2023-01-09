// taken from https://stackoverflow.com/a/55592995/55408

module fledger.Parsing.ParsingUtils

open FParsec

type WithPos<'T> =
    { Value: 'T
      Start: Position
      Finish: Position }

module Position =
    /// Get the previous position on the same line.
    let leftOf (p: Position) =
        if p.Column > 1L then
            Position(p.StreamName, p.Index - 1L, p.Line, p.Column - 1L)
        else
            p

/// Wrap a parser to include the position
let withPos (p: Parser<'T, 'U>) : Parser<WithPos<'T>, 'U> =
    // Get the position before and after parsing
    pipe3 getPosition p getPosition
    <| fun start value finish ->
        { Value = value
          Start = start
          Finish = Position.leftOf finish }
