module Util exposing (sanitize)


sanitize : String -> String
sanitize =
    String.lines
        -- remove first newline
        >> List.drop 1
        -- remove last newline
        >> List.reverse
        >> List.drop 1
        >> List.reverse
        -- remove indentation
        >> List.map String.trimLeft
        -- readd trailing newlines
        >> List.map (\str -> str ++ "\n")
        >> String.concat
