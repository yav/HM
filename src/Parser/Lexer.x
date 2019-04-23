{
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Parser.Lexer
  ( lexer
  , Lexeme(..)
  , Token(..)
  , Input(..), initialInput
  , SourceRange(..)
  , SourcePos(..)
  , prettySourceRange
  ) where
import Data.Text(Text)
import qualified Data.Text as Text
import qualified Data.Char as Char
import Data.Ratio((%))

import AlexTools
}

$uniupper       = \x1
$unilower       = \x2
$unidigit       = \x3
$unisymbol      = \x4
$unispace       = \x5
$uniother       = \x6
$unitick        = \x7


$letter         = [a-zA-Z_]
$octdigit       = 0-7
$digit          = 0-9
$hexdigit       = [0-9a-fA-F]

@id_first       = [a-zA-Z_] | $unilower | $uniupper
@id_next        = [a-zA-Z0-9_'] | $unilower | $uniupper | $unidigit | $unitick
@ident          = @id_first @id_next*

@op             =  ([\!\#\$\%\&\*\+\-\.\/\:\<\=\>\?\@\\\^\|\~] | $unisymbol)+

@digs2          = (_*[0-1])+
@digs8          = (_*[0-7])+
@digs10         = (_*[0-9])+
@digs16         = (_*[0-9A-Fa-f])+

@sign           = [\+\-]
@num2           = "0b"  @digs2
@num8           = "0o"  @digs8
@num10          = [0-9] @digs10
@num16          = "0x"  @digs16

@exp10          = [Ee] @sign? @num10
@exp16          = [Pp] @sign? @num10
@float10        = @num10 @exp10
                | (@num10  "." @num10?) @exp10?
                | (@num10? "." @num10)  @exp10?
@float16        = @num16 @exp16
                | (@num16        "." @digs16?) @exp16?
                | ("0x" @digs16? "." @digs16) @exp16?

@line_comment   = "--".*


:-

$white+             { return [] }
@line_comment       { return [] }

"if"                { lexeme TokKwIf      }
"case"              { lexeme TokKwCase    }
"then"              { lexeme TokKwThen    }
"else"              { lexeme TokKwElse    }
"do"                { lexeme TokKwDo      }
"let"               { lexeme TokKwLet     }
"in"                { lexeme TokKwIn      }

"("                 { lexeme TokParenL    }
")"                 { lexeme TokParenR    }
"{"                 { lexeme TokBraceL    }
"}"                 { lexeme TokBraceR    }
"["                 { lexeme TokBracketL  }
"]"                 { lexeme TokBracketR  }
","                 { lexeme TokComma     }
";"                 { lexeme TokSemi      }
"`"                 { lexeme TokBackTick  }

"<-"                { lexeme TokArrowL    }
"->"                { lexeme TokArrowR    }

"\\"                { lexeme TokBackSlash }
"|"                 { lexeme TokBar       }
"="                 { lexeme TokEq        }

@ident              { lexeme TokIdent     }
@op                 { lexeme TokOp        }

@num8               { lexeme' . TokInt  . integerAtBase 8  =<< matchText }
@num10              { lexeme' . TokInt  . integerAtBase 10 =<< matchText }
@num16              { lexeme' . TokInt  . integerAtBase 16 =<< matchText }
@float10            { lexeme' . TokReal . floating      10 =<< matchText }
@float16            { lexeme' . TokReal . floating      16 =<< matchText }

.                   { lexeme TokError }


{

data Token =
    TokIdent | TokOp

  | TokInt !Integer | TokReal !Rational

  | TokKwIf | TokKwCase | TokKwThen | TokKwElse | TokKwDo | TokKwLet | TokKwIn

  | TokParenL | TokParenR
  | TokBraceL | TokBraceR
  | TokBracketL | TokBracketR
  | TokSemi | TokComma
  | TokBackTick

  | TokArrowL | TokArrowR
  | TokBackSlash | TokBar
  | TokEq

  | TokEOF
  | TokError
    deriving (Eq,Show)

lexeme' :: Token -> Action () [Lexeme Token]
lexeme' t = lexeme $! t

integerAtBase :: Integer -> Text -> Integer
integerAtBase base txt = if sgn == "-" then negate aval else aval
  where
  aval = Text.foldl' addDig 0 digs
  (sgn,txt0) = splitSign (Text.map Char.toLower txt)
  digs = Text.dropWhile (\x -> x == '0' || x == 'x' || x == 'o') txt0

  addDig s x = case x of
                 '_' -> s
                 _   -> s * base + (if y < a then y - z else 10 + (y - a))
    where
    y = val x
    a = val 'a'
    z = val '0'
    val = fromIntegral . fromEnum

splitSign :: Text -> (Text,Text)
splitSign = Text.span (\x -> x == '+' || x == '-')

floating :: Integer -> Text -> Rational
floating fb txt =
  case Text.splitOn exSym (Text.map Char.toLower txt) of
    [base] -> parseBase base
    [base,ex]
      | e >= 0    -> b * fromInteger exVal ^ e
      | otherwise -> b / fromInteger exVal ^ abs e
        where
        e = integerAtBase 10 ex
        b = parseBase base

    _ -> error "[bug] unexpected floating number"
  where
  (exSym,exVal,dbase) = if fb == 10 then ("e",10,10) else ("p",2,16)

  parseBase base =
    let (sign,rest) = splitSign base
        addSign = if sign == "-" then negate else id
    in addSign
     $ case Text.splitOn "." rest of
         [x]    -> fromInteger (integerAtBase dbase x)
         [x,y]  -> fromInteger (integerAtBase dbase x) + 
                   integerAtBase dbase y % dbase ^ Text.length y
         _ -> error "[bug] unexpected floating number base"

-- | Collapse characters into a single Word8, identifying ASCII, and classes of
-- unicode.  This came from:
--
-- https://github.com/glguy/config-value/blob/master/src/Config/LexerUtils.hs
--
-- Which adapted:
--
-- https://github.com/ghc/ghc/blob/master/compiler/parser/Lexer.x
byteForChar :: Char -> Word8
byteForChar c
  | c <= '\6' = non_graphic
  | Char.isAscii c = fromIntegral (fromEnum c)
  | otherwise = case Char.generalCategory c of
                  Char.LowercaseLetter       -> lower
                  Char.OtherLetter           -> lower
                  Char.UppercaseLetter       -> upper
                  Char.TitlecaseLetter       -> upper
                  Char.DecimalNumber         -> digit
                  Char.OtherNumber           -> digit
                  Char.ConnectorPunctuation  -> symbol
                  Char.DashPunctuation       -> symbol
                  Char.OtherPunctuation      -> symbol
                  Char.MathSymbol            -> symbol
                  Char.CurrencySymbol        -> symbol
                  Char.ModifierSymbol        -> symbol
                  Char.OtherSymbol           -> symbol
                  Char.Space                 -> sp
                  Char.ModifierLetter        -> other
                  Char.NonSpacingMark        -> other
                  Char.SpacingCombiningMark  -> other
                  Char.EnclosingMark         -> other
                  Char.LetterNumber          -> other
                  Char.OpenPunctuation       -> other
                  Char.ClosePunctuation      -> other
                  Char.InitialQuote          -> other
                  Char.FinalQuote            -> tick
                  _                          -> non_graphic
  where
  non_graphic     = 0
  upper           = 1
  lower           = 2
  digit           = 3
  symbol          = 4
  sp              = 5
  other           = 6
  tick            = 7



alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte = makeAlexGetByte byteForChar

lexer :: Input -> [Lexeme Token]
lexer = $makeLexer simpleLexer { lexerEOF = \_ p -> [eof p] }
  where eof p = Lexeme { lexemeToken = TokEOF
                       , lexemeText  = ""
                       , lexemeRange = AlexTools.range p
                       }
}


