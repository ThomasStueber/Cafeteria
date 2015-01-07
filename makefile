cafeteria: Scanner.x Parser.y Abs.hs Main.hs
	alex Scanner.x -g -iscanner_info.txt
	happy Parser.y -a -g -c -iparser_info.txt
	ghc Main.hs -fglasgow-exts