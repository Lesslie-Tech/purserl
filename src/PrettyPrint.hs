-- |
module PrettyPrint where

import Prelude

-- |
sShow :: Show a => a -> String
sShow a = pformat 0 $! show a -- T.unpack $! pShow a

-- |
pformat :: Int -> String -> String
pformat ident s =
  let
    nextIsDedent =
      case s of
        ')':_ -> 1
        ']':_ -> 1
        '}':_ -> 1
        _ -> 0
    ind = replicate ((ident-nextIsDedent)*2) ' '
    indl = replicate (((ident-nextIsDedent)-1)*2) ' '
  in
  case s of
    '\n':rest -> "\n" ++ ind ++ pformat ident rest
    ':':rest -> ":\n" ++ ind ++ pformat ident rest
    ';':rest -> ";\n" ++ ind ++ pformat ident rest
    ',':rest -> "\n" ++ indl ++ "," ++ pformat ident rest
    '(':')':rest -> "()" ++ pformat ident rest
    '[':']':rest -> "[]" ++ pformat ident rest
    '(':rest -> "\n" ++ ind ++ "(" ++ pformat (ident+1) rest
    '[':rest -> "\n" ++ ind ++ "[" ++ pformat (ident+1) rest
    '{':rest -> "\n" ++ ind ++ "{" ++ pformat (ident+1) rest
    ')':rest -> ")\n" ++ indl ++ pformat (ident-1) rest
    ']':rest -> "]\n" ++ indl ++ pformat (ident-1) rest
    '}':rest -> "}\n" ++ indl ++ pformat (ident-1) rest
    x:rest -> x : pformat ident rest
    [] -> ""


