sn2parser: y.tab.c lex.yy.c y.tab.h
	gcc -w -g y.tab.c -ly -ll -o sn2parser
lex.yy.c: lex.l
	lex lex.l
y.tab.c: parser.y
	yacc -v -d -g parser.y
clean:
	rm -f sn2parser y.tab.c lex.yy.c y.tab.h y.output
