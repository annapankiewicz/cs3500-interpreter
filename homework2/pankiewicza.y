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

N_CONST         : T_INTCONST
                {
                    printRule("CONST", "INTCONST");
                }
                | T_STRCONST
                {
                    printRule("CONST", "STRCONST");
                }
                | T_FLOATCONST
                {
                    printRule("CONST", "STRCONST");
                }
                | T_TRUE
                {
                    printRule("CONST", "TRUE");
                }
                | T_FALSE
                {
                    printRule("CONST", "FALSE");
                }
                ;

N_ARITHLOGIC_EXPR : N_UN_OP N_EXPR
                {
                    printRule("ARITHLOGIC_EXPR", "UN_OP N_EXPR");
                }
                | N_EXPR N_BIN_OP N_EXPR
                {
                    printRule("ARITHLOGIC_EXPR", "EXPR BIN_OP EXPR");
                }
                ;

N_COMPOUND_EXPR : T_LBRACE N_EXPR N_EXPR_LIST T_RBRACE
                {
                    printRule("COMPOUND_EXPR",
                              "{ EXPR EXPR_LIST }");
                }
                ;

N_EXPR_LIST     : T_SEMICOLON N_EXPR N_EXPR_LIST
                {
                    printRule("EXPR_LIST", "; EXPR EXPR_LIST");
                }
                | /* epsilon */
                ;

N_IF_EXPR       : T_IF T_LPAREN N_EXPR T_RPAREN N_EXPR
                {
                    printRule("IF_EXPR", "IF ( EXPR ) EXPR")
                }
                | T_IF T_LPAREN N_EXPR T_RPAREN N_EXPR T_ELSE N_EXPR
                {
                    printRule("IF_EXPR", "IF ( EXPR ) ");
                }
                ;

N_WHILE_EXPR    : T_WHILE T_LPAREN N_EXPR T_RPAREN N_LOOP_EXPR
                {
                    printRule("WHILE_EXPR", "WHILE ( VAR IN LIST ) LOOP_EXPR";)
                }
                ;

N_FOR_EXPR      : T_FOR T_LPAREN N_VAR T_IN N_LIST T_RPAREN N_LOOP_EXPR
                {
                    printRule("FOR_EXPR", "FOR ( VAR IN LIST ) LOOP_EXPR");
                }
                ;

N_LOOP_EXPR     : N_EXPR
                {
                    printRule("LOOP_EXPR", "EXPR");
                }
                | N_BREAK_EXPR
                {
                    printRule("LOOP_EXPR", "BREAK_EXPR");
                }
                | N_NEXT_EXPR
                {
                    printRule("LOOP_EXPR", "NEXT_EXPR");
                }
                ;

N_BREAK_EXPR    : T_BREAK
                {
                    printRule("BREAK_EXPR", "BREAK");
                }
                ;

N_NEXT_EXPR     : T_NEXT
                {
                    printRule("NEXT_EXPR", "NEXT");
                }
                ;

N_ASSIGNMENT_EXPR : T_IDENT T_ASSIGN N_EXPR
                {
                    printRule("IDENT = EXPR");
                }
                ;

N_QUIT_STMT     : T_QUIT T_LPAREN T_RPAREN
                {
                    printRule("QUIT_STMT", "QUIT()");
                }
                ;

N_OUTPUT_EXPR   : T_PRINT N_EXPR
                {
                    printRule("OUTPUT_EXPR", "PRINT EXPR");
                }
                | T_CAT N_EXPR
                {
                    printRule("OUTPUT_EXPR", "CAT EXPR");
                }
                ;

N_INPUT_EXPR    : T_READ T_LPAREN N_VAR T_RPAREN
                {
                    printRule("INPUT_EXPR", "READ ( VAR )");
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