/* 
    pankiewicza.y

    flex pankiewicza.l
    bison pankiewicza.y
    g++ pankiewicza.tab.c -o parser
    ./parser < inputFileName
    
*/

%{
#include <stdio.h>

int line_num = 1;

void printRule(const char *, const char *);

int yyerror(const char *s) {
    printf("Line %d: %s\n", line_num, s);
}

extern "C" {
    int yyparse(void);
    int yylex(void);
    int yywrap() { return 1; }
}

%}

%token T_IDENT T_INTCONST T_FLOATCONST T_UNKNOWN T_STRCONST T_IF T_ELSE
%token T_WHILE T_FUNCTION T_FOR T_IN T_NEXT T_BREAK T_TRUE T_FALSE T_QUIT
%token T_PRINT T_CAT T_READ T_LPAREN T_RPAREN T_LBRACE T_RBRACE T_LBRACKET
%token T_RBRACKET T_COLON T_SEMICOLON T_COMMA T_ADD T_SUB T_MULT T_DIV T_MOD
%token T_POWER T_LT T_LE T_GT T_GE T_EQ T_NE T_NOT T_AND T_OR T_ASSIGN

%start N_START

%%

N_START         : N_EXPR
                {
                    printRule("START", "EXPR");
                    printf("\n---- Completed parsing ----\n\n");
                    return 0;
                }
                ;
N_EXPR          : N_CONST
                {
                    printRule("EXPR", "CONST");
                }
                | N_VAR
                {
                    printRule("EXPR", "VAR");
                }
                | N_IF_EXPR
                {
                    printRule("EXPR", "IF_EXPR");
                }
                | N_WHILE_EXPR
                {
                    printRule("EXPR", "WHILE_EXPR");
                }
                | N_FOR_EXPR
                {
                    printRule("EXPR", "FOR_EXPR");
                }
                | N_COMPOUND_EXPR
                {
                    printRule("EXPR", "COMPOUND_EXPR");
                }
                | N_ARITHLOGIC_EXPR
                {
                    printRule("EXPR", "ARITHLOGIC_EXPR");
                }
                | N_ASSIGNMENT_EXPR
                {
                    printRule("EXPR", "ASSIGNMENT_EXPR");
                }
                | N_PRINT_EXPR
                {
                    printRule("EXPR", "PRINT_EXPR");
                }
                | N_INPUT_EXPR
                {
                    printRule("EXPR", "INPUT_EXPR");
                }
                | N_FUNCTION_DEF
                {
                    printRule("EXPR", "FUNCTION_DEF");
                }
                | N_FUNCTION_CALL
                {
                    printRule("EXPR", "FUNCTION_CALL");
                }
                | N_QUIT_STMT
                {
                    printRule("EXPR", "QUIT_STMT");
                }
                ;

%%

#include "lex.yy.c"
extern FILE *yyin;

void printRule(const char *lhs, const char *rhs)
{
    printf("%s -> %s\n", lhs, rhs);
    return;
}

int main() {
    do {
        yyparse();
    } while (!feof(yyin));

    return 0;
}