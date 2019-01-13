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

