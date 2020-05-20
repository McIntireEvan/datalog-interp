{
module Lexer (Token(..), lexer) where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$upper = [A-Z]
@ident = [a-z][^\(\)\`\'\=\:\.\~\?\"\%\,\ ]*
@variable = $upper ($alpha | $digit | '_')*
@string = \" (\\ | \" | .)+\"

tokens :-
  $white+				;
  "%".*				    ;

  @variable                         { \s -> TokenVar (s) }
  @ident                            { \s -> TokenIdent (s) }
  @string+                          { \s -> TokenString (read s)}

  "."                   { \s -> TokenPeriod   }
  "("			              { \s -> TokenLParen   }
  ")"                   { \s -> TokenRParen   }
  "~"                   { \s -> TokenTilde    }
  "!="                  { \s -> TokenNotEq    }
  "="                   { \s -> TokenEq       }
  "?"                   { \s -> TokenQuestion }
  ":-"                  { \s -> TokenAssign   }
  ","                   { \s -> TokenComma    }

{

data Token =
    TokenIdent String
  | TokenVar String
  | TokenString String
  | TokenPeriod
  | TokenTilde
  | TokenQuestion
  | TokenAssign
  | TokenEq
  | TokenNotEq
  | TokenComma
  | TokenLParen
  | TokenRParen
    deriving (Eq,Show)

lexer :: String -> [Token]
lexer = alexScanTokens
}