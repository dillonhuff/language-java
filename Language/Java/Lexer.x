{
{-# LANGUAGE BangPatterns #-}
module Language.Java.Lexer (L(..), Token(..), lexer) where

import Numeric
import Data.Char

import Debug.Trace (trace)

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

    abstract        { \p _ -> mkL p KW_Abstract     }
    assert          { \p _ -> mkL p KW_Assert       }
    boolean         { \p _ -> mkL p KW_Boolean      }
    break           { \p _ -> mkL p KW_Break        }
    byte            { \p _ -> mkL p KW_Byte         }
    case            { \p _ -> mkL p KW_Case         }
    catch           { \p _ -> mkL p KW_Catch        }
    char            { \p _ -> mkL p KW_Char         }
    class           { \p _ -> mkL p KW_Class        }
    const           { \p _ -> mkL p KW_Const        }
    continue        { \p _ -> mkL p KW_Continue     }
    default         { \p _ -> mkL p KW_Default      }
    do              { \p _ -> mkL p KW_Do           }
    double          { \p _ -> mkL p KW_Double       }
    else            { \p _ -> mkL p KW_Else         }
    enum            { \p _ -> mkL p KW_Enum         }
    extends         { \p _ -> mkL p KW_Extends      }
    final           { \p _ -> mkL p KW_Final        }
    finally         { \p _ -> mkL p KW_Finally      }
    float           { \p _ -> mkL p KW_Float        }
    for             { \p _ -> mkL p KW_For          }
    goto            { \p _ -> mkL p KW_Goto         }
    if              { \p _ -> mkL p KW_If           }
    implements      { \p _ -> mkL p KW_Implements   }
    import          { \p _ -> mkL p KW_Import       }
    instanceof      { \p _ -> mkL p KW_Instanceof   }
    int             { \p _ -> mkL p KW_Int          }
    interface       { \p _ -> mkL p KW_Interface    }
    long            { \p _ -> mkL p KW_Long         }
    native          { \p _ -> mkL p KW_Native       }
    new             { \p _ -> mkL p KW_New          }
    package         { \p _ -> mkL p KW_Package      }
    private         { \p _ -> mkL p KW_Private      }
    protected       { \p _ -> mkL p KW_Protected    }
    public          { \p _ -> mkL p KW_Public       }
    return          { \p _ -> mkL p KW_Return       }
    short           { \p _ -> mkL p KW_Short        }
    static          { \p _ -> mkL p KW_Static       }
    strictfp        { \p _ -> mkL p KW_Strictfp     }
    super           { \p _ -> mkL p KW_Super        }
    switch          { \p _ -> mkL p KW_Switch       }
    synchronized    { \p _ -> mkL p KW_Synchronized }
    this            { \p _ -> mkL p KW_This         }
    throw           { \p _ -> mkL p KW_Throw        }
    throws          { \p _ -> mkL p KW_Throws       }
    transient       { \p _ -> mkL p KW_Transient    }
    try             { \p _ -> mkL p KW_Try          }
    void            { \p _ -> mkL p KW_Void         }
    volatile        { \p _ -> mkL p KW_Volatile     }
    while           { \p _ -> mkL p KW_While        }

    0               { \p _ -> mkL p $ IntTok 0        }
    0 [lL]          { \p _ -> mkL p $ LongTok 0       }
    0 $digit+       { \p s -> mkL p $ IntTok (pickyReadOct s) }
    0 $digit+ [lL]  { \p s -> mkL p $ LongTok (pickyReadOct (init s)) }
    $nonzero $digit*        { \p s -> mkL p $ IntTok (read s) }
    $nonzero $digit* [lL]   { \p s -> mkL p $ LongTok (read (init s)) }
    0 [xX] $hexdig+         { \p s -> mkL p $ IntTok (fst . head $ readHex (drop 2 s)) }
    0 [xX] $hexdig+ [lL]    { \p s -> mkL p $ LongTok (fst . head $ readHex (init (drop 2 s))) }

    $digit+ \. $digit* @exponent? [dD]?           { \p s -> mkL p $ DoubleTok (fst . head $ readFloat $ '0':s) }
            \. $digit+ @exponent? [dD]?           { \p s -> mkL p $ DoubleTok (fst . head $ readFloat $ '0':s) }
    $digit+ \. $digit* @exponent? [fF]            { \p s -> mkL p $ FloatTok  (fst . head $ readFloat $ '0':s) }
            \. $digit+ @exponent? [fF]            { \p s -> mkL p $ FloatTok  (fst . head $ readFloat $ '0':s) }
    $digit+ @exponent                             { \p s -> mkL p $ DoubleTok (fst . head $ readFloat s) }
    $digit+ @exponent? [dD]                       { \p s -> mkL p $ DoubleTok (fst . head $ readFloat s) }
    $digit+ @exponent? [fF]                       { \p s -> mkL p $ FloatTok  (fst . head $ readFloat s) }
    0 [xX] $hexdig* \.? $hexdig* @pexponent [dD]? { \p s -> mkL p $ DoubleTok (readHexExp (drop 2 s)) }
    0 [xX] $hexdig* \.? $hexdig* @pexponent [fF]  { \p s -> mkL p $ FloatTok  (readHexExp (drop 2 s)) }

    true            { \p _ -> mkL p $ BoolTok True    }
    false           { \p _ -> mkL p $ BoolTok False   }

    ' (@charEscape | ~[\\\']) '               { \p s -> mkL p $ CharTok (readCharTok s) }

    \" (@charEscape | ~[\\\"])* \"            { \p s -> mkL p $ StringTok (readStringTok s) }

    null            {\p _ -> mkL p $ NullTok }

    $javaLetter $javaLetterOrDigit*     { \p s -> mkL p $ IdentTok s }

    \(              { \p _ -> mkL p OpenParen       }
    \)              { \p _ -> mkL p CloseParen      }
    \[              { \p _ -> mkL p OpenSquare      }
    \]              { \p _ -> mkL p CloseSquare     }
    \{              { \p _ -> mkL p OpenCurly       }
    \}              { \p _ -> mkL p CloseCurly      }
    \;              { \p _ -> mkL p SemiColon       }
    \,              { \p _ -> mkL p Comma           }
    \.              { \p _ -> mkL p Period          }

    "="             { \p _ -> mkL p Op_Equal        }
    ">"             { \p _ -> mkL p Op_GThan        }
    "<"             { \p _ -> mkL p Op_LThan        }
    "!"             { \p _ -> mkL p Op_Bang         }
    "~"             { \p _ -> mkL p Op_Tilde        }
    "?"             { \p _ -> mkL p Op_Query        }
    ":"             { \p _ -> mkL p Op_Colon        }
    "=="            { \p _ -> mkL p Op_Equals       }
    "<="            { \p _ -> mkL p Op_LThanE       }
    ">="            { \p _ -> mkL p Op_GThanE       }
    "!="            { \p _ -> mkL p Op_BangE        }
    "&&"            { \p _ -> mkL p Op_AAnd         }
    "||"            { \p _ -> mkL p Op_OOr          }
    "++"            { \p _ -> mkL p Op_PPlus        }
    "--"            { \p _ -> mkL p Op_MMinus       }
    "+"             { \p _ -> mkL p Op_Plus         }
    "-"             { \p _ -> mkL p Op_Minus        }
    "*"             { \p _ -> mkL p Op_Star         }
    "/"             { \p _ -> mkL p Op_Slash        }
    "&"             { \p _ -> mkL p Op_And          }
    "|"             { \p _ -> mkL p Op_Or           }
    "^"             { \p _ -> mkL p Op_Caret        }
    "%"             { \p _ -> mkL p Op_Percent      }
    "<<"            { \p _ -> mkL p Op_LShift       }
    "+="            { \p _ -> mkL p Op_PlusE        }
    "-="            { \p _ -> mkL p Op_MinusE       }
    "*="            { \p _ -> mkL p Op_StarE        }
    "/="            { \p _ -> mkL p Op_SlashE       }
    "&="            { \p _ -> mkL p Op_AndE         }
    "|="            { \p _ -> mkL p Op_OrE          }
    "^="            { \p _ -> mkL p Op_CaretE       }
    "%="            { \p _ -> mkL p Op_PercentE     }
    "<<="           { \p _ -> mkL p Op_LShiftE      }
    ">>="           { \p _ -> mkL p Op_RShiftE      }
    ">>>="          { \p _ -> mkL p Op_RRShiftE     }
    "@"             { \p _ -> mkL p Op_AtSign       }
--    $printable	    { \p s -> fail $ (show s) ++ " is not a recognized java lexeme" }

{

pos :: AlexPosn -> Pos
pos (AlexPn _ l c) = (l,c)

mkL p t = L (pos p) t

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

lexer :: String -> [L Token]
lexer = alexScanTokens

}
