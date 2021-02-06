%{ 

/* CAN DUYAR - 171044075 */


/* DEFINITION */
#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include "y.tab.h" 
%} 

COMMENT ";".*
VAL [1-9][0-9]*|"0"
Id [a-zA-Z][a-zA-Z0-9]*
IDCASE [a-zA-Z_][a-z.A-Z0-9_]*
PLUSPLUS [a-zA-Z_][a-z.A-Z0-9++_]*
FLOATING_POINT [0-9]+"."[0-9]*

%%
	/* RULES PART */
^\n                     {exit(1);}
" "|"\t" { } 
"\n" {printf("%s\n",yytext); return 0; }

disp { return KW_DISP; }
true { return KW_TRUE; }
false { return KW_FALSE; }
or { return KW_OR ;}
not { return  KW_NOT ;}
equal { return KW_EQUAL ;} 
less { return KW_LESS ;}
nil { return KW_NIL ;}
list { return KW_LIST ;}
append {return KW_APPEND ;}
concat {return KW_CONCAT ;}
set { return KW_SET ;}
deffun { return KW_DEFFUN ;}   // DEFFUN -> ADDITIONAL FEATURE, YOU EXPLAINED IT ON MOODLE
for  { return KW_FOR; }
while { return KW_WHILE; }
\+ { return OP_PLUS;}
\- { return OP_MINUS;}
\/ { return OP_DIV;}
\* { return OP_MULT;}
\'\( { return KW_OPLIST;}
\*\* {return KW_DBLMULT;}
and { return KW_AND ;}
load { return KW_LOAD; }
defvar { return KW_DEFVAR; }
setq {return KW_SETQ ;}    //ASSIGNMENT IMPLEMENTATION....  -> ADDITIONAL FEATURE, YOU EXPLAINED IT ON MOODLE
if { return KW_IF; }
\“	{return OP_OC;}
\” 	{return OP_CC;}
\(  { return OP_OP;}
\) { return OP_CP;} 
\" {return OP_DOUBLEQUOTE;}
{Id}|{IDCASE}|{PLUSPLUS} { return Id;}
{COMMENT} {return COMMENT;}

{VAL}|{FLOATING_POINT} {
    yylval=atoi(yytext); 
    return IntegerValue; 
}

[0-9][0-9a-zA-Z]*	{printf("SYNTAX_ERROR: Expression not recognized!\n"); return 0;} // ERROR CHECKING
.	 	  	  	    {}

%% 


/* MAIN FUNCTION */
int main(int argc, char const *argv[]){ 
    FILE *fipo; 
   
   /*g++ mode*/   
      if(argc < 1){
        yyin = stdin; //interpreter is doing lexical analysis.
        while (1){
            yyparse(); 
        }         
    }
/*reading from file*/
/*g++ filename.g++ mode*/
    else{   
        fipo = fopen(argv[1],"r");  
        yyin = fipo; //file is readed and lexical analysis is doing.
        while (1){
            yyparse(); 
        }   
     }    
    return 0; 
}


int yywrap(){ return 1;} 
