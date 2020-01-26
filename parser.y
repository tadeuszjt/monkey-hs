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
%nonassoc  '[' ']'


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
	let        { L.Reserved _ "let" }
    else       { L.Reserved _ "else" }
    true       { L.Reserved _ "true" }
    false      { L.Reserved _ "false" }
    while      { L.Reserved _ "while" }
    return     { L.Reserved _ "return" }
    print      { L.Reserved _ "print" }

    int        { L.Int _ _ }
    ident      { L.Ident _ _ }
    string     { L.String _ _ }

    '('        { L.Sym _ '(' }
    ')'        { L.Sym _ ')' }
    '{'        { L.Sym _ '{' }
    '}'        { L.Sym _ '}' }
    '['        { L.Sym _ '[' }
    ']'        { L.Sym _ ']' }
    ','        { L.Sym _ ',' }
    ';'        { L.Sym _ ';' }

%%

Prog : Stmt                    { [$1] }
     | Stmt Prog               { $1 : $2 }


Stmt : Stmt1 ';'               { $1 }
     | while Expr Block        { S.While (L.tokPosn $1) $2 $3 }
     | If                      { $1 }
	 | Block                   { $1 }
      
Stmt1 : let ident '=' Expr     { let (L.Ident _ s) = $2 in S.Assign (L.tokPosn $1) s $4 }
      | ident '=' Expr         { let (L.Ident _ s) = $1 in S.Set (L.tokPosn $2) s $3 }
      | return Expr            { S.Return (L.tokPosn $1) $2 }
      | Expr                   { S.ExprStmt $1 }


If : if Expr Block Else        { S.If (L.tokPosn $1) $2 $3 $4 }

Else : {- empty -}             { Nothing }
     | else Block              { Just $2 }
     | else If                 { Just $2 }


Block : '{' Block1 '}'         { S.Block $2 } 
	  | '{' Expr '}'           { S.BlockExpr $2 }

Block1 : {- empty -}           { [] }
       | Stmt Block1           { $1 : $2 }


Expr : int                     { let (L.Int p i) = $1 in S.Int p i }
     | true                    { S.Bool (L.tokPosn $1) True }
     | false                   { S.Bool (L.tokPosn $1) False }
     | fn '(' Params ')' Block { S.Func (L.tokPosn $1) $3 $5 }
     | string                  { let (L.String p s) = $1 in S.String p s }
     | ident                   { (\(L.Ident p s) -> S.Ident p s) $1 }
	 | Array                   { $1 }
     | '(' Expr ')'            { $2 }
	 | Expr '[' Expr ']'       { S.Subscript (S.exprPosn $3) $1 $3 }
     | Expr '(' Args ')'       { S.Call  (L.tokPosn $2) $1 $3 }
     | print '(' Args ')'      { S.Call  (L.tokPosn $2) (S.Ident (L.tokPosn $1) "print") $3 }
     | Expr '+' Expr           { S.Infix (L.tokPosn $2) S.Plus $1 $3 }
     | Expr '-' Expr           { S.Infix (L.tokPosn $2) S.Minus $1 $3 }
     | Expr '*' Expr           { S.Infix (L.tokPosn $2) S.Times $1 $3 }
     | Expr '/' Expr           { S.Infix (L.tokPosn $2) S.Divide $1 $3 }
     | Expr '%' Expr           { S.Infix (L.tokPosn $2) S.Mod $1 $3 }
     | Expr '<' Expr           { S.Infix (L.tokPosn $2) S.LT $1 $3 }
     | Expr '>' Expr           { S.Infix (L.tokPosn $2) S.GT $1 $3 }
     | Expr '<=' Expr          { S.Infix (L.tokPosn $2) S.LTEq $1 $3 }
     | Expr '>=' Expr          { S.Infix (L.tokPosn $2) S.GTEq $1 $3 }
     | Expr '==' Expr          { S.Infix (L.tokPosn $2) S.EqEq $1 $3 }
     | Expr '||' Expr          { S.Infix (L.tokPosn $2) S.OrOr $1 $3 }

Array : '[' Args ']'           { S.Array (L.tokPosn $1) $2 }

Args : {- empty -}             { [] }
     | Expr                    { [$1] }
     | Expr ',' Args           { $1 : $3 }

Params : {- empty -}           { [] }
	   | ident                 { let (L.Ident p s) = $1 in [s] }
       | ident ',' Params      { let (L.Ident p s) = $1 in s : $3 }

{
parseError :: [L.Token] -> a
parseError (x:_) =
    error $ "parser error: " ++ show x
}
