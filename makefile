main: parser.hs lexer.hs ir.hs ast.hs compiler.hs evaluate.hs main.hs cgen.hs test.monkey
	ghc *.hs -outputdir build
	./main < test.monkey

parser.hs: parser.y
	happy parser.y

lexer.hs: lexer.x
	alex lexer.x
