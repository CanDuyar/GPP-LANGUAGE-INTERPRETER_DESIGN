%{
#include <stdio.h>
#include <math.h>
extern FILE *yyin;

int A[500]; //array for append and result of list operations
int keep_ind = 0, temp = 0, number = 0, ind = 0;
int condition = 0, else_condition=0, checkPrint=0;
int check = 0;
int set_control = 0;


// I used it for KW_APPEND operations
int* operationAppend(int A[],int size,int num){
    for (int t = size-1; t>=0; t--){
        A[t+1] = A[t];
    }
    A[0] = num;
    return A;
}

// I used it to print it an Array 
void showArray(int A[],int size){
    for(int t=0;t< size;t++){
        if(t!= size-1)
            printf("%d ",A[t]);
        else    
             printf("%d",A[t]);    
    }
}

%}

/* it includes my tokens */

%start START

%token IntegerValue
%token Id OP_PLUS OP_MINUS OP_MULT OP_DIV OP_OP OP_CP OP_OC OP_CC
%token OP_DOUBLEQUOTE KW_AND KW_OR KW_NOT KW_EQUAL KW_LESS 
KW_NIL KW_APPEND KW_CONCAT KW_SET KW_DEFFUN KW_OPLIST KW_DBLMULT 
%token KW_FOR KW_WHILE KW_DEFVAR KW_IF 
KW_EXIT KW_LOAD KW_DISP KW_TRUE KW_FALSE KW_LIST KW_SETQ
%token COMMENT


/* RULES PART */
%% 

START: | INPUT;        

INPUT: 
EXPI { 
    if(!check && set_control == 0){    // if syntax is correct...
        printf("SYNTAX OK.\n");
    }

    if(checkPrint == 1 && condition == 1){
        printf ("\nResult: (" );
        showArray(A,ind);
        printf (")");
        ind=0;
        condition=0;
        checkPrint=0;
    }


    if(condition==1 && checkPrint==1 && temp==0 && else_condition==0 && !check){ 
        printf ("Result: (" );
        showArray(A,ind);
        printf (")");
        ind=0;
        condition=0;
        checkPrint=0;
    }

    else if(temp==0 && else_condition==1 && checkPrint==1 && !check ){
        for(int i= 0; i<ind-keep_ind ; i++){
            A[i]=A[keep_ind+i];
        }
        printf ("Result: (" );
        ind=ind-keep_ind;
        showArray(A,ind);
        printf (")");
        ind=0;
        else_condition=0;
        checkPrint=0;
    }
   

    else if(temp==1 && checkPrint==1 && !check){ 
        printf ("Result: (" );
        ind = keep_ind;
        showArray(A,ind);
        printf (")");
        ind=0;
        temp=0;
        condition=0;
        checkPrint=0;
    }
    else if (condition==0 && checkPrint==1 && number==0 && !check){ 
        printf("Result: Nil"); 
        checkPrint=0;  
    } 
     if(number==1 && checkPrint==1 && !check){
         printf("Result: %d\n",$$);
         number=0; 
         checkPrint=0;
    } 

    set_control = 0;     
}

| EXPLISTI {                    
    if(checkPrint==1 && condition==1 && !check){ 
        printf("SYNTAX OK.\n");
        printf ("Result: (" );
        showArray(A,ind);
        printf (")");
        ind=0;
        checkPrint=0;
        condition=0;
    }
    set_control = 0;     

}

| EXPB {                            
    if (!check)
        printf("SYNTAX OK."); 
    if(checkPrint==1 && !check){
        printf("Result: ");
        if($$ == 1){
            printf ("True");
        }
        else
            printf("False");  
        checkPrint=0; 
    }
    set_control = 0;           
} 

LISTVALUE: 
KW_OPLIST VALUES OP_CP {  if (keep_ind == 0 && !check) { keep_ind = ind; }}
| KW_LIST OP_CP ;

VALUES: 
VALUES IntegerValue { A[ind]=$2; ind=ind+1; }
| IntegerValue  { A[ind]=$1 ; ind=ind+1; };

EXPI: 
OP_OP OP_PLUS EXPI EXPI OP_CP {$$=$3+$4; checkPrint=1; number=1; }
| OP_OP OP_DIV EXPI EXPI OP_CP {$$=$3/$4; checkPrint=1; number=1;}
| OP_OP OP_MULT EXPI EXPI OP_CP {$$=$3*$4; checkPrint=1; number=1;}
| OP_OP KW_DBLMULT EXPI EXPI OP_CP {$$ = pow($3,$4); checkPrint=1; number=1;}
| OP_OP OP_MINUS EXPI EXPI OP_CP {$$=$3-$4; checkPrint=1; number=1;}
| OP_OP Id EXPLISTI OP_CP { $$= $3; condition=1; checkPrint=1;  }
| OP_OP KW_SET Id EXPI OP_CP { $$ = $4; checkPrint=0;printf("SYNTAX OK.\n");printf("Result: %d\n",$$);set_control = 1;}
| OP_OP KW_DEFFUN Id IDLIST EXPLISTI OP_CP
| OP_OP KW_SETQ Id EXPI OP_CP {checkPrint=1; number=1; condition=0; $$=$4;} /////ADDITIONAL FEATURE FOR ASSIGNMENT
| OP_OP KW_LOAD OP_DOUBLEQUOTE Id OP_DOUBLEQUOTE OP_CP
| Id 
| OP_OP KW_LOAD OP_OC Id OP_CC OP_CP
| OP_OP KW_DISP  OP_DOUBLEQUOTE Id OP_DOUBLEQUOTE  OP_CP
| IntegerValue  {$$=$1;  }
| COMMENT 
| OP_OP KW_IF EXPB EXPLISTI OP_CP {//JUST ONE CASE FOR "IF" IMPLEMENTATION -> YOU ALSO EXPLAINED IT ON MOODLE PAGE...
    checkPrint=1;
    if($3==1){ 
         condition=1; 
    } else{
        condition=0;
    } 
}

| OP_OP KW_WHILE EXPB  EXPLISTI OP_CP  
{ 
    checkPrint=1;
    if($3==1){ 
        condition=1; 
    } 
    else {
        condition=0;
    } 
}

| OP_OP KW_FOR OP_OP Id EXPI EXPI OP_CP EXPLISTI OP_CP 
{ 
    checkPrint=1;
    condition=1;    
}

| OP_OP KW_DEFVAR Id EXPI OP_CP {  checkPrint=1; number=1; condition=0; $$=$4; }
| OP_OP KW_LIST VALUES OP_CP {checkPrint =1;condition =1;}
| OP_OP KW_EXIT OP_CP { exit(1);}
;

EXPB: 
OP_OP KW_AND EXPB EXPB OP_CP  {$$=($3 && $4);  checkPrint=1;} 
| OP_OP KW_NOT EXPB  OP_CP  {$$=!$3;  checkPrint=1;} 
| OP_OP KW_OR EXPB EXPB OP_CP   {$$=($3 || $4); checkPrint=1; } 
| OP_OP KW_EQUAL EXPB EXPB OP_CP  {$$=($3==$4);  checkPrint=1; } 
| OP_OP KW_LESS EXPI EXPI OP_CP  {$$=($3 < $4);  checkPrint=1; } 
|OP_OP KW_EQUAL EXPI EXPI OP_CP  {$$=($3==$4);  checkPrint=1; } 
| BinaryVal

;

BinaryVal:
 KW_TRUE { $$=1;}
| KW_FALSE   {$$=0; }
;

IDList:
 IDList Id 
| Id
;

EXPLISTI: 
OP_OP KW_CONCAT EXPLISTI EXPLISTI OP_CP 
| OP_OP KW_APPEND EXPI EXPLISTI OP_CP { operationAppend(A,ind,$3); ind=ind+1; }
| LISTVALUE { condition=1; checkPrint=1;};

IDLIST: 
OP_OP IDList OP_CP;

%% 

int yyerror(const char * ch) { 
    printf("\nSYNTAX_ERROR: Expression not recognized!\n");    // ERROR CASE 
    check = 1; 
    exit(1);
} 





