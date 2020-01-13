{
module Parser where
import qualified Lexer as L
import qualified AST as S
}

%name      parseTokens 
%tokentype { L.Token }
%error     { parseError }

%left      '||'
%left      '&&'
%left      '==' '!='
%left      '+' '-'
%left      '*' '/' '%'
%nonassoc  '<' '>'
%nonassoc  '<=' '>='
%nonassoc  '(' ')'

%token
    '*'        { L.ReservedOp _ "*" }
    '/'        { L.ReservedOp _ "/" }
    '%'        { L.ReservedOp _ "%" }
    '+'        { L.ReservedOp _ "+" }
    '-'        { L.ReservedOp _ "-" }
    '<'        { L.ReservedOp _ "<" }
    '>'        { L.ReservedOp _ ">" }
    '<='       { L.ReservedOp _ "<=" }
    '>='       { L.ReservedOp _ ">=" }
    '=='       { L.ReservedOp _ "==" }
    '!='       { L.ReservedOp _ "!=" }
    '&&'       { L.ReservedOp _ "&&" }
    '||'       { L.ReservedOp _ "||" }

    ':='       { L.ReservedOp _ ":=" }
    '='        { L.ReservedOp _ "=" }

    fn         { L.Reserved _ "fn" }
    if         { L.Reserved _ "if" }
    else       { L.Reserved _ "else" }
    true       { L.Reserved _ "true" }
    false      { L.Reserved _ "false" }
    while      { L.Reserved _ "while" }
    return     { L.Reserved _ "return" }

    int        { L.Int _ $$ }
    ident      { L.Ident _ $$ }
    string     { L.String _ $$ }

    '('        { L.Sym _ '(' }
    ')'        { L.Sym _ ')' }
    '{'        { L.Sym _ '{' }
    '}'        { L.Sym _ '}' }
    ','        { L.Sym _ ',' }
    ';'        { L.Sym _ ';' }

%%

Prog : Stmt                    { [$1] }
     | Stmt Prog               { $1 : $2 }


Stmt : Stmt1 ';'               { $1 }
     | while Expr Block        { S.While $2 $3 }
     | If                      { $1 }
      
Stmt1 : ident ':=' Expr        { S.Assign $1 $3 }
      | ident '=' Expr         { S.Set $1 $3 }
      | return Expr            { S.Return $2 }
      | Expr                   { S.ExprStmt $1 }


If : if Expr Block Else        { S.If $2 $3 $4 }

Else : {- empty -}             { Nothing }
     | else Block              { Just $2 }
     | else If                 { Just $2 }


Block : '{' Block1 '}'         { S.Block $2 } 

Block1 : {- empty -}           { [] }
       | Stmt Block1           { $1 : $2 }


Expr : int                     { S.Int $1 }
     | true                    { S.Bool True }
     | false                   { S.Bool False }
     | fn '(' Params ')' Block { S.Func $3 $5 }
     | string                  { S.String $1 }
     | ident                   { S.Ident $1 }
     | '(' Expr ')'            { $2 }
     | Expr '(' Args ')'       { S.Call $1 $3 }
     | Expr '+' Expr           { S.Infix S.Plus $1 $3 }
     | Expr '-' Expr           { S.Infix S.Minus $1 $3 }
     | Expr '*' Expr           { S.Infix S.Times $1 $3 }
     | Expr '/' Expr           { S.Infix S.Divide $1 $3 }
     | Expr '%' Expr           { S.Infix S.Mod $1 $3 }
     | Expr '<' Expr           { S.Infix S.LThan $1 $3 }
     | Expr '>' Expr           { S.Infix S.GThan $1 $3 }
     | Expr '<=' Expr          { S.Infix S.LTEq $1 $3 }
     | Expr '>=' Expr          { S.Infix S.GTEq $1 $3 }
     | Expr '==' Expr          { S.Infix S.EqEq $1 $3 }
     | Expr '||' Expr          { S.Infix S.OrOr $1 $3 }

Args : {- empty -}             { [] }
     | Expr                    { [$1] }
     | Expr ',' Args           { $1 : $3 }

Params : {- empty -}           { [] }
       | ident ',' Params      { $1 : $3 }

{
parseError :: [L.Token] -> a
parseError (x:_) =
    error $ "parser error: " ++ show x
}
