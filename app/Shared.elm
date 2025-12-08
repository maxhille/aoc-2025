module Shared exposing (linesParser)

import Parser exposing ((|.), (|=), Parser, Trailing(..))


linesParser : Parser a -> Parser (List a)
linesParser itemParser =
    Parser.sequence
        { start = ""
        , separator = "\n"
        , end = ""
        , spaces = Parser.chompWhile (always False)
        , item = itemParser
        , trailing = Mandatory
        }
