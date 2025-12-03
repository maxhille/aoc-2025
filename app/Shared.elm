module Shared exposing (linesParser)

import Parser exposing ((|.), (|=), Parser, Trailing(..))


linesParser : Parser a -> Parser (List a)
linesParser itemParser =
    Parser.loop [] (linesParserHelp itemParser)


linesParserHelp : Parser a -> List a -> Parser (Parser.Step (List a) (List a))
linesParserHelp itemParser revItems =
    Parser.oneOf
        [ Parser.succeed ()
            |. Parser.end
            |> Parser.map (\_ -> Parser.Done (List.reverse revItems))
        , Parser.succeed (\_ -> Parser.Loop revItems)
            |= Parser.symbol "\n"
        , Parser.succeed (\stmt -> Parser.Loop (stmt :: revItems))
            |= itemParser
        ]
