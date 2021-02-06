all:
	lex gpp_interpreter.l 
	yacc -d gpp_interpreter.y 
	gcc lex.yy.c y.tab.c -w -lm -o gpp_interpreter.out
	
run:
	./gpp_interpreter.out inputFile.g++ 

clean:
	rm -rf *.out *.c *.h 