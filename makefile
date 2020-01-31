main: Parser.hs Lexer.hs IR.hs AST.hs Typechecker.hs Evaluate.hs Main.hs CGen.hs Cmplib.hs SymTab.hs Compiler.hs test.monkey 
	ghc *.hs -outputdir build

parser.hs: parser.y
	happy parser.y

lexer.hs: lexer.x
	alex lexer.x
