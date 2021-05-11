all: parser

clean:
	rm -rf parser.cpp parser.hpp parser lexer.cpp parser.o codegen.o main.o lexer.o native.o  native2.o native3.o

parser.cpp: parser.y
	bison -d -o parser.cpp parser.y
	
parser.hpp: parser.cpp

lexer.cpp: lex.l parser.hpp
	flex -o lexer.cpp lex.l parser.hpp

%.o: %.cpp
	g++ -c `llvm-config --cppflags` -std=c++14 -o $@ $<


parser:  parser.o codegen.o main.o lexer.o native.o  native2.o  native3.o
	g++ -o parser parser.o codegen.o main.o lexer.o native.o native2.o native3.o `llvm-config --libs` `llvm-config --ldflags` -lpthread -ldl -lz -lncurses -rdynamic

