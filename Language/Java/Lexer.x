{
{-# LANGUAGE BangPatterns #-}
module Language.Java.Lexer (L(..), Token(..), lexer) where

import Control.Monad
import Data.Char
import Debug.Trace (trace)
import Numeric

import Language.Java.LexerUtils
}

%wrapper "posn"

$digit      = [0-9]
$nonzero    = [1-9]
$octdig     = [0-7]
$hexdig     = [0-9A-Fa-f]

@lineterm = [\n\r] | \r\n

-- TODO: this doesn't notice a comment that ends "**/"
@tradcomm = "/*" ( ~[\*] | \*+ (~[\/\*] | \n) | \n )* \*+ "/"
@linecomm = "//" .* @lineterm
@comm = @tradcomm | @linecomm

$javaLetter = [a-zA-Z\_\$]
$javaDigit = $digit
$javaLetterOrDigit = [a-zA-Z0-9\_\$]

@octEscape = [0123]? $octdig{1,2}
@hexEscape = u $hexdig{4}
@charEscape = \\ (@octEscape | @hexEscape | [btnfr\"\'\\])

@expsuffix = [\+\-]? $digit+
@exponent = [eE] @expsuffix
@pexponent = [pP] @expsuffix

tokens  :-

    $white+         ;
    @comm           ;

    abstract        { \p _ -> lmkL p KW_Abstract     }
    assert          { \p _ -> lmkL p KW_Assert       }
    boolean         { \p _ -> lmkL p KW_Boolean      }
    break           { \p _ -> lmkL p KW_Break        }
    byte            { \p _ -> lmkL p KW_Byte         }
    case            { \p _ -> lmkL p KW_Case         }
    catch           { \p _ -> lmkL p KW_Catch        }
    char            { \p _ -> lmkL p KW_Char         }
    class           { \p _ -> lmkL p KW_Class        }
    const           { \p _ -> lmkL p KW_Const        }
    continue        { \p _ -> lmkL p KW_Continue     }
    default         { \p _ -> lmkL p KW_Default      }
    do              { \p _ -> lmkL p KW_Do           }
    double          { \p _ -> lmkL p KW_Double       }
    else            { \p _ -> lmkL p KW_Else         }
    enum            { \p _ -> lmkL p KW_Enum         }
    extends         { \p _ -> lmkL p KW_Extends      }
    final           { \p _ -> lmkL p KW_Final        }
    finally         { \p _ -> lmkL p KW_Finally      }
    float           { \p _ -> lmkL p KW_Float        }
    for             { \p _ -> lmkL p KW_For          }
    goto            { \p _ -> lmkL p KW_Goto         }
    if              { \p _ -> lmkL p KW_If           }
    implements      { \p _ -> lmkL p KW_Implements   }
    import          { \p _ -> lmkL p KW_Import       }
    instanceof      { \p _ -> lmkL p KW_Instanceof   }
    int             { \p _ -> lmkL p KW_Int          }
    interface       { \p _ -> lmkL p KW_Interface    }
    long            { \p _ -> lmkL p KW_Long         }
    native          { \p _ -> lmkL p KW_Native       }
    new             { \p _ -> lmkL p KW_New          }
    package         { \p _ -> lmkL p KW_Package      }
    private         { \p _ -> lmkL p KW_Private      }
    protected       { \p _ -> lmkL p KW_Protected    }
    public          { \p _ -> lmkL p KW_Public       }
    return          { \p _ -> lmkL p KW_Return       }
    short           { \p _ -> lmkL p KW_Short        }
    static          { \p _ -> lmkL p KW_Static       }
    strictfp        { \p _ -> lmkL p KW_Strictfp     }
    super           { \p _ -> lmkL p KW_Super        }
    switch          { \p _ -> lmkL p KW_Switch       }
    synchronized    { \p _ -> lmkL p KW_Synchronized }
    this            { \p _ -> lmkL p KW_This         }
    throw           { \p _ -> lmkL p KW_Throw        }
    throws          { \p _ -> lmkL p KW_Throws       }
    transient       { \p _ -> lmkL p KW_Transient    }
    try             { \p _ -> lmkL p KW_Try          }
    void            { \p _ -> lmkL p KW_Void         }
    volatile        { \p _ -> lmkL p KW_Volatile     }
    while           { \p _ -> lmkL p KW_While        }

    0               { \p _ -> lmkL p $ IntTok 0        }
    0 [lL]          { \p _ -> lmkL p $ LongTok 0       }
    0 $digit+       { \p s -> liftM (mkL p . IntTok) (pickyReadOct s) }
    0 $digit+ [lL]  { \p s -> liftM (mkL p . LongTok) (pickyReadOct (init s)) }
    $nonzero $digit*        { \p s -> lmkL p $ IntTok (read s) }
    $nonzero $digit* [lL]   { \p s -> lmkL p $ LongTok (read (init s)) }
    0 [xX] $hexdig+         { \p s -> lmkL p $ IntTok (fst . head $ readHex (drop 2 s)) }
    0 [xX] $hexdig+ [lL]    { \p s -> lmkL p $ LongTok (fst . head $ readHex (init (drop 2 s))) }

    $digit+ \. $digit* @exponent? [dD]?           { \p s -> lmkL p $ DoubleTok (fst . head $ readFloat $ '0':s) }
            \. $digit+ @exponent? [dD]?           { \p s -> lmkL p $ DoubleTok (fst . head $ readFloat $ '0':s) }
    $digit+ \. $digit* @exponent? [fF]            { \p s -> lmkL p $ FloatTok  (fst . head $ readFloat $ '0':s) }
            \. $digit+ @exponent? [fF]            { \p s -> lmkL p $ FloatTok  (fst . head $ readFloat $ '0':s) }
    $digit+ @exponent                             { \p s -> lmkL p $ DoubleTok (fst . head $ readFloat s) }
    $digit+ @exponent? [dD]                       { \p s -> lmkL p $ DoubleTok (fst . head $ readFloat s) }
    $digit+ @exponent? [fF]                       { \p s -> lmkL p $ FloatTok  (fst . head $ readFloat s) }
    0 [xX] $hexdig* \.? $hexdig* @pexponent [dD]? { \p s -> lmkL p $ DoubleTok (readHexExp (drop 2 s)) }
    0 [xX] $hexdig* \.? $hexdig* @pexponent [fF]  { \p s -> lmkL p $ FloatTok  (readHexExp (drop 2 s)) }

    true            { \p _ -> lmkL p $ BoolTok True    }
    false           { \p _ -> lmkL p $ BoolTok False   }

    ' (@charEscape | ~[\\\']) '               { \p s -> liftM (mkL p . CharTok) (readCharTok s) }

    \" (@charEscape | ~[\\\"])* \"            { \p s -> liftM (mkL p . StringTok) (readStringTok s) }

    null            {\p _ -> lmkL p $ NullTok }

    $javaLetter $javaLetterOrDigit*     { \p s -> lmkL p $ IdentTok s }

    \(              { \p _ -> lmkL p OpenParen       }
    \)              { \p _ -> lmkL p CloseParen      }
    \[              { \p _ -> lmkL p OpenSquare      }
    \]              { \p _ -> lmkL p CloseSquare     }
    \{              { \p _ -> lmkL p OpenCurly       }
    \}              { \p _ -> lmkL p CloseCurly      }
    \;              { \p _ -> lmkL p SemiColon       }
    \,              { \p _ -> lmkL p Comma           }
    \.              { \p _ -> lmkL p Period          }

    "="             { \p _ -> lmkL p Op_Equal        }
    ">"             { \p _ -> lmkL p Op_GThan        }
    "<"             { \p _ -> lmkL p Op_LThan        }
    "!"             { \p _ -> lmkL p Op_Bang         }
    "~"             { \p _ -> lmkL p Op_Tilde        }
    "?"             { \p _ -> lmkL p Op_Query        }
    ":"             { \p _ -> lmkL p Op_Colon        }
    "=="            { \p _ -> lmkL p Op_Equals       }
    "<="            { \p _ -> lmkL p Op_LThanE       }
    ">="            { \p _ -> lmkL p Op_GThanE       }
    "!="            { \p _ -> lmkL p Op_BangE        }
    "&&"            { \p _ -> lmkL p Op_AAnd         }
    "||"            { \p _ -> lmkL p Op_OOr          }
    "++"            { \p _ -> lmkL p Op_PPlus        }
    "--"            { \p _ -> lmkL p Op_MMinus       }
    "+"             { \p _ -> lmkL p Op_Plus         }
    "-"             { \p _ -> lmkL p Op_Minus        }
    "*"             { \p _ -> lmkL p Op_Star         }
    "/"             { \p _ -> lmkL p Op_Slash        }
    "&"             { \p _ -> lmkL p Op_And          }
    "|"             { \p _ -> lmkL p Op_Or           }
    "^"             { \p _ -> lmkL p Op_Caret        }
    "%"             { \p _ -> lmkL p Op_Percent      }
    "<<"            { \p _ -> lmkL p Op_LShift       }
    "+="            { \p _ -> lmkL p Op_PlusE        }
    "-="            { \p _ -> lmkL p Op_MinusE       }
    "*="            { \p _ -> lmkL p Op_StarE        }
    "/="            { \p _ -> lmkL p Op_SlashE       }
    "&="            { \p _ -> lmkL p Op_AndE         }
    "|="            { \p _ -> lmkL p Op_OrE          }
    "^="            { \p _ -> lmkL p Op_CaretE       }
    "%="            { \p _ -> lmkL p Op_PercentE     }
    "<<="           { \p _ -> lmkL p Op_LShiftE      }
    ">>="           { \p _ -> lmkL p Op_RShiftE      }
    ">>>="          { \p _ -> lmkL p Op_RRShiftE     }
    "@"             { \p _ -> lmkL p Op_AtSign       }
    $printable	    { \p s -> Left $ "asdfat pos " ++ (show p) ++ ": " ++ (show s) ++ " is not a recognized java lexeme" }

{

pos :: AlexPosn -> Pos
pos (AlexPn _ l c) = (l,c)

mkL :: AlexPosn -> Token -> L Token
mkL p t = L (pos p) t

lmkL :: AlexPosn -> Token -> LexError (L Token)
lmkL p t = return $ mkL p t

data Token
    -- Keywords
    = KW_Abstract
    | KW_Assert
    | KW_Boolean
    | KW_Break
    | KW_Byte
    | KW_Case
    | KW_Catch
    | KW_Char
    | KW_Class
    | KW_Const
    | KW_Continue
    | KW_Default
    | KW_Do
    | KW_Double
    | KW_Else
    | KW_Enum
    | KW_Extends
    | KW_Final
    | KW_Finally
    | KW_Float
    | KW_For
    | KW_Goto
    | KW_If
    | KW_Implements
    | KW_Import
    | KW_Instanceof
    | KW_Int
    | KW_Interface
    | KW_Long
    | KW_Native
    | KW_New
    | KW_Package
    | KW_Private
    | KW_Protected
    | KW_Public
    | KW_Return
    | KW_Short
    | KW_Static
    | KW_Strictfp
    | KW_Super
    | KW_Switch
    | KW_Synchronized
    | KW_This
    | KW_Throw
    | KW_Throws
    | KW_Transient
    | KW_Try
    | KW_Void
    | KW_Volatile
    | KW_While

    -- Separators
    | OpenParen
    | CloseParen
    | OpenSquare
    | CloseSquare
    | OpenCurly
    | CloseCurly
    | SemiColon
    | Comma
    | Period

    -- Literals
    | IntTok  Integer
    | LongTok Integer
    | DoubleTok Double
    | FloatTok Double
    | CharTok Char
    | StringTok String
    | BoolTok Bool
    | NullTok

    -- Identifiers
    | IdentTok String

    -- Operators
    | Op_Equal
    | Op_GThan
    | Op_LThan
    | Op_Bang
    | Op_Tilde
    | Op_Query
    | Op_Colon
    | Op_Equals
    | Op_LThanE
    | Op_GThanE
    | Op_BangE
    | Op_AAnd
    | Op_OOr
    | Op_PPlus
    | Op_MMinus
    | Op_Plus
    | Op_Minus
    | Op_Star
    | Op_Slash
    | Op_And
    | Op_Or
    | Op_Caret
    | Op_Percent
    | Op_LShift
    | Op_PlusE
    | Op_MinusE
    | Op_StarE
    | Op_SlashE
    | Op_AndE
    | Op_OrE
    | Op_CaretE
    | Op_PercentE
    | Op_LShiftE
    | Op_RShiftE
    | Op_RRShiftE
    | Op_AtSign
  deriving (Show, Eq)

lexer :: String -> LexError [L Token]
lexer = sequence . alexScanTokens

}
