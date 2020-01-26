main: parser.hs lexer.hs ir.hs ast.hs typechecker.hs evaluate.hs main.hs cgen.hs cmplib.hs symtab.hs compiler.hs test.monkey 
	ghc *.hs -outputdir build

parser.hs: parser.y
	happy parser.y

lexer.hs: lexer.x
	alex lexer.x
