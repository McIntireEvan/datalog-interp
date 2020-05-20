{
module Parser(parseTokens) where
import Lexer
}

%name datalog Program
%tokentype { Token }
%error { parseError }

%token
  var                       { TokenVar  $$ }
  ident                     { TokenIdent $$ }
  string                    { TokenString $$ }
  '.'                       { TokenPeriod }
  '('		                { TokenLParen }
  ')'                       { TokenRParen }
  '~'                       { TokenTilde }
  '!='                      { TokenNotEq }
  '='                       { TokenEq }
  '?'                       { TokenQuestion }
  ':-'                      { TokenAssign }
  ','                       { TokenComma }

%%

Program :: { Program }
  : Statements { Statements $1 }

Statements :: { [Statement] }
  : { [] }
  | Statements Statement { $1 : $2 }

Statement :: { Statement }
  : Clause '.' { Assertion $1 }
  | Clause '~' { Retraction $1 }
  | Literal '?' { Query $1 }

Clause :: { Clause }
  : Literal ':-' Body { Assign $1 (Body $3) }
  | Literal { Head $1 }

Body :: { [Literal] }
  : Literal { [$1] }
  | Literal ',' Body { $1 : $3 }

Literal :: { Literal }
  : PredicateSym '(' ')' { PredicateFull $1 [] }
  | PredicateSym '(' Terms ')' { PredicateFull $1 $3 }
  | PredicateSym { PredicateFull $1 [] }
  | Term '=' Term { Equal $1 $3 }
  | Term '!=' Term { NotEqual $1 $3 }

PredicateSym :: { PredicateSym }
  : ident { PIdent $1 }
  | string { PString $1 }

Terms :: { [Term] }
  : Term { [$1] }
  | Terms ',' Term { $1 : $3 }

Term :: { Term }
  : var { TermVar $1 }
  | Constant { TermConst $1 }

Constant :: { Constant }
  : ident { ConstIdent $1 }
  | string { ConstString $1 }


{
parseError :: [Token] -> Except String a
parseError (l:ls) = throwError (show l)
parseError [] = throwError "Unexpected end of Input"

-- data types that represent parsed expression:

data Program
      = Statements [Statement]

data Statement
      = Assertion Clause
      | Retraction Clause
      | Query Literal

data Clause
      = Assign Literal Body
      | Head Literal

data Body
      = Body [Literal]

data Literal
      = PredicateFull PredicateSym [Term]
      | Equal Term Term
      | NotEqual Term Term

data PredicateSym
      = Identifier String
      | String String

data Term
      = Variable String
      | Constant string

parseTokens :: ByteString -> Either String Program
parseTokens = datalog . getTokens
}