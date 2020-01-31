main: Parser.hs Lexer.hs IR.hs AST.hs Typechecker.hs Main.hs CGen.hs SymTab.hs Compiler.hs test.monkey 
	ghc *.hs -outputdir build

parser.hs: parser.y
	happy parser.y

lexer.hs: lexer.x
	alex lexer.x
