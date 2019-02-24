/* 
    minir.y

    flex minir.l
    bison minir.y
    g++ minir.tab.c -o parser
    ./parser < inputFileName
    
*/

%{
#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <string>
#include <stack>
#include "SymbolTable.h"
using namespace std;

int line_num = 1;
int numParams = 0;
int numExprs = 0;

stack<SYMBOL_TABLE> scopeStack; // stack of scope hashtables

void beginScope();
void endScope();
void cleanUp();
TYPE_INFO findEntryInAnyScope(const string the_name);

void printTokenInfo(const char* token_type, const char* lexeme);

void printRule(const char *, const char *);

int yyerror(const char *s) 
{
    printf("Line %d: %s\n", line_num, s);
    cleanUp();
    exit(1);
}

extern "C" 
{
    int yyparse(void);
    int yylex(void);
    int yywrap() { return 1; }
}

%}

%union {
    char* text;
    int num;
    TYPE_INFO typeInfo;
};

%token T_IDENT T_INTCONST T_FLOATCONST T_UNKNOWN T_STRCONST 
%token T_IF T_ELSE
%token T_WHILE T_FUNCTION T_FOR T_IN T_NEXT T_BREAK 
%token T_TRUE T_FALSE T_QUIT
%token T_PRINT T_CAT T_READ T_LPAREN T_RPAREN T_LBRACE 
%token T_RBRACE T_LBRACKET
%token T_RBRACKET T_SEMICOLON T_COMMA T_ADD T_SUB 
%token T_MULT T_DIV T_MOD
%token T_POW T_LT T_LE T_GT T_GE T_EQ T_NE T_NOT T_AND 
%token T_OR T_ASSIGN T_LIST

%type <text> T_IDENT

%type <typeInfo> N_EXPR N_IF_EXPR N_WHILE_EXPR N_FOR_EXPR
%type <typeInfo> N_COMPOUND_EXPR N_ARITHLOGIC_EXPR N_ASSIGNMENT_EXPR
%type <typeInfo> N_OUTPUT_EXPR N_INPUT_EXPR N_LIST_EXPR N_FUNCTION_DEF
%type <typeInfo> N_FUNCTION_CALL N_QUIT_EXPR N_CONST N_EXPR_LIST
%type <typeInfo> N_LOOP_EXPR N_BREAK_EXPR N_NEXT_EXPR

%type <num> N_PARAM_LIST N_PARAMS

/*
 *  To eliminate ambiguity in if/else
 */
%nonassoc   T_RPAREN 
%nonassoc   T_ELSE


%start N_START

%%

N_START         : N_EXPR
                {
                    printRule("START", "EXPR");
                    endScope();
                    printf("\n---- Completed parsing ----\n\n");
                    return 0;
                }
                ;
N_EXPR          : N_IF_EXPR
                {
                    printRule("EXPR", "IF_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_WHILE_EXPR
                {
                    printRule("EXPR", "WHILE_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_FOR_EXPR
                {
                    printRule("EXPR", "FOR_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_COMPOUND_EXPR
                {
                    printRule("EXPR", "COMPOUND_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_ARITHLOGIC_EXPR
                {
                    printRule("EXPR", "ARITHLOGIC_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_ASSIGNMENT_EXPR
                {
                    printRule("EXPR", "ASSIGNMENT_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_OUTPUT_EXPR
                {
                    printRule("EXPR", "OUTPUT_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_INPUT_EXPR
                {
                    printRule("EXPR", "INPUT_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_LIST_EXPR
                {
                    printRule("EXPR", "LIST_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_FUNCTION_DEF
                {
                    printRule("EXPR", "FUNCTION_DEF");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_FUNCTION_CALL
                {
                    printRule("EXPR", "FUNCTION_CALL");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_QUIT_EXPR
                {
                    printRule("EXPR", "QUIT_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                    exit(1);
                }
                ;

N_CONST         : T_INTCONST
                {
                    printRule("CONST", "INTCONST");
                    $$.type = INT;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
                }
                | T_STRCONST
                {
                    printRule("CONST", "STRCONST");
                    $$.type = STR;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
                }
                | T_FLOATCONST
                {
                    printRule("CONST", "FLOATCONST");
                    $$.type = FLOAT;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
                }
                | T_TRUE
                {
                    printRule("CONST", "TRUE");
                    $$.type = BOOL;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;

                }
                | T_FALSE
                {
                    printRule("CONST", "FALSE");
                    $$.type = BOOL;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
                }
                ;

N_COMPOUND_EXPR : T_LBRACE N_EXPR N_EXPR_LIST T_RBRACE
                {
                    printRule("COMPOUND_EXPR",
                              "{ EXPR EXPR_LIST }");
                    $$.type = $2.type;
                    $$.numParams = $2.numParams;
                    $$.returnType = $2.returnType;
                }
                ;

N_EXPR_LIST     : T_SEMICOLON N_EXPR N_EXPR_LIST
                {
                    printRule("EXPR_LIST", "; EXPR EXPR_LIST");
                    $$.type = $2.type;
                    $$.numParams = $2.numParams;
                    $$.returnType = $2.returnType;
                }
                | /* epsilon */
                {
                    printRule("EXPR_LIST", "epsilon");
                    // worry about this type later
                }
                ;

N_IF_EXPR       : T_IF T_LPAREN N_EXPR T_RPAREN N_EXPR
                {
                    printRule("IF_EXPR", "IF ( EXPR ) EXPR");
                    if(($3.type == FUNCTION) || ($3.type == LIST)) {
                        yyerror("Arg 1 cannot be function or list");
                    }
                    if($5.type == FUNCTION) {
                        yyerror("Arg 2 cannot be function");
                    }

                    $$.type = $3.type ^ $5.type;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
                }
                | T_IF T_LPAREN N_EXPR T_RPAREN N_EXPR T_ELSE
                  N_EXPR
                {
                    printRule("IF_EXPR", "IF ( EXPR ) EXPR ELSE EXPR");
                    if(($3.type == FUNCTION) || ($3.type == LIST)) {
                        yyerror("Arg 1 cannot be function or list");
                    }
                    if($5.type == FUNCTION) {
                        yyerror("Arg 2 cannot be function");
                    }
                }
                ;

N_WHILE_EXPR    : T_WHILE T_LPAREN N_EXPR T_RPAREN N_LOOP_EXPR
                {
                    printRule("WHILE_EXPR", 
                              "WHILE ( EXPR ) "
                              "LOOP_EXPR");
                    if(($3.type == FUNCTION) || ($3.type == LIST) ||
                       ($3.type == NULL_TYPE)) {
                           yyerror("Arg 3 cannot be function or null or list");
                       }
                    $$.type = $5.type;
                    $$.numParams = $5.numParams;
                    $$.returnType = $$.returnType;
                }
                ;

N_FOR_EXPR      : T_FOR T_LPAREN T_IDENT T_IN N_EXPR T_RPAREN
                  N_LOOP_EXPR
                {
                    printRule("FOR_EXPR", 
                              "FOR ( IDENT IN EXPR ) "
                              "LOOP_EXPR");
                    if(($5.type == FUNCTION) || ($5.type == NULL_TYPE)) {
                        yyerror("Arg 5 cannot be function or null");
                    }
                    $$.type = $7.type;
                    $$.numParams = $7.numParams;
                    $$.returnType = $7.returnType;
                }
                ;

N_LOOP_EXPR     : N_EXPR
                {
                    printRule("LOOP_EXPR", "EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_BREAK_EXPR
                {
                    printRule("LOOP_EXPR", "BREAK_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_NEXT_EXPR
                {
                    printRule("LOOP_EXPR", "NEXT_EXPR");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                ;

N_BREAK_EXPR    : T_BREAK
                {
                    printRule("BREAK_EXPR", "BREAK");
                    $$.type = NULL_TYPE;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
                }
                ;

N_NEXT_EXPR     : T_NEXT
                {
                    printRule("NEXT_EXPR", "NEXT");
                    $$.type = NULL_TYPE;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
                }
                ;

N_LIST_EXPR     : T_LIST T_LPAREN N_CONST_LIST T_RPAREN
                {
                    printRule("LIST_EXPR", 
                              "LIST ( CONST_LIST )");
                    $$.type = LIST;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
                }
                ;

N_CONST_LIST    : N_CONST T_COMMA N_CONST_LIST
                {
                    printRule("CONST_LIST", 
                              "CONST, CONST_LIST");
                }
                | N_CONST
                {
                    printRule("CONST_LIST", "CONST");
                }
                ;

N_ASSIGNMENT_EXPR : T_IDENT N_INDEX T_ASSIGN N_EXPR
                {
                    printRule("ASSIGNMENT_EXPR", 
                              "IDENT INDEX ASSIGN EXPR");
                    string lexeme = string($1);
                    printf("___Adding %s to symbol table\n", $1);
                    bool success = scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY
                        (lexeme, {$4.type, NOT_APPLICABLE, NOT_APPLICABLE}));
                    if(!success) {
                      yyerror("Multiply defined identifier");
                      return(0);
                    }
                    $$.type = $4.type;
                    $$.numParams = $4.numParams;
                    $$.returnType = $4.returnType;
                }
                ;

N_INDEX :       T_LBRACKET T_LBRACKET N_EXPR T_RBRACKET T_RBRACKET
			    {
                    printRule("INDEX", " [[ EXPR ]]");
			    }
			    | /* epsilon */
                {
                    printRule("INDEX", " epsilon");
                }
                ;

N_QUIT_EXPR     : T_QUIT T_LPAREN T_RPAREN
                {
                    printRule("QUIT_EXPR", "QUIT()");
                    $$.type = NULL_TYPE;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
                }
                ;

N_OUTPUT_EXPR   : T_PRINT T_LPAREN N_EXPR T_RPAREN
                {
                    printRule("OUTPUT_EXPR", 
                              "PRINT ( EXPR )");
                    if(($3.type == FUNCTION) || ($3.type == NULL_TYPE)) {
                        yyerror("Arg 3 cannot be function or null");
                    }
                    $$.type = $3.type;
                    $$.numParams = $3.numParams;
                    $$.returnType = $3.returnType;
                }
                | T_CAT T_LPAREN N_EXPR T_RPAREN
                {
                    printRule("OUTPUT_EXPR", 
                              "CAT ( EXPR )");
                    if(($3.type == FUNCTION) || ($3.type == NULL_TYPE)) {
                        yyerror("Arg 3 cannot be function or null");
                    }
                    $$.type = NULL_TYPE;
                    $$.numParams = $3.numParams;
                    $$.returnType = $3.returnType;
                }
                ;

N_INPUT_EXPR    : T_READ T_LPAREN N_VAR T_RPAREN
                {
                    printRule("INPUT_EXPR", "READ ( VAR )");
                    $$.type = INT_OR_STR_OR_FLOAT;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
                }
                ;

N_FUNCTION_DEF  : T_FUNCTION T_LPAREN
                {
                    beginScope();
                }
                N_PARAM_LIST T_RPAREN N_COMPOUND_EXPR
                {
                    printRule("FUNCTION_DEF",
                              "FUNCTION ( PARAM_LIST )"
                              " COMPOUND_EXPR");

                    endScope();
                    if($6.type == FUNCTION) {
                        yyerror("Arg 6 cannot be function");
                    }
                    $$.type = FUNCTION;
                    $$.numParams = $4;
                    $$.returnType = $6.returnType;
                }
                ;

N_PARAM_LIST    : N_PARAMS
                {
                    printRule("PARAM_LIST", "PARAMS");
                    $$ = $1;
                }
                | N_NO_PARAMS
                {
                    printRule("PARAM_LIST", "NO PARAMS");
                    numParams = 0;
                    $$ = numParams;
                }
                ;

N_NO_PARAMS     : /* epsilon */
                {
                    printRule("NO_PARAMS", "epsilon");
                }
                ;

N_PARAMS        : T_IDENT
                {
                    printRule("PARAMS", "IDENT");
                    string lexeme = string($1);
                    printf("___Adding %s to symbol table\n", $1);
                    // assuming params are ints according to assignment description
                    TYPE_INFO exprTypeInfo = {INT, NOT_APPLICABLE, NOT_APPLICABLE};
                    bool success = scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY
                        (lexeme, exprTypeInfo));
                    if(!success) {
                        yyerror("Multiply defined identifier");
                        return(1);
                    }
                    numParams++;
                    $$ = numParams;
                }
                | T_IDENT T_COMMA N_PARAMS
                {
                    printRule("PARAMS", "IDENT, PARAMS");
                    string lexeme = string($1);
                    printf("___Adding %s to symbol table\n", $1);
                    // assuming params are ints according to assignment description
                    TYPE_INFO exprTypeInfo = {INT, NOT_APPLICABLE, NOT_APPLICABLE};
                    bool success = scopeStack.top().addEntry(SYMBOL_TABLE_ENTRY
                        (lexeme, exprTypeInfo));
                    if(!success) {
                        yyerror("Multiply defined identifier");
                        return(1);
                    }
                    numParams++;
                    $$ = numParams;
                }
                ;

N_FUNCTION_CALL : T_IDENT T_LPAREN N_ARG_LIST T_RPAREN
                {
                    printRule("FUNCTION_CALL", "IDENT"
                              " ( ARG_LIST )");
                }
                ;

N_ARG_LIST      : N_ARGS
                {
                    printRule("ARG_LIST", "ARGS");
                }
                | N_NO_ARGS
                {
                    printRule("ARG_LIST", "NO_ARGS");
                }
                ;

N_NO_ARGS       : /* epsilon */
                {
                    printRule("NO_ARGS", "epsilon");
                }
                ;

N_ARGS          : N_EXPR
                {
                    printRule("ARGS", "EXPR");
                }
                | N_EXPR T_COMMA N_ARGS
                {
                    printRule("ARGS", "EXPR, ARGS");
                }
                ;

N_ARITHLOGIC_EXPR : N_SIMPLE_ARITHLOGIC
                {
                    printRule("ARITHLOGIC_EXPR",
                              "SIMPLE_ARITHLOGIC");
                }
                | N_SIMPLE_ARITHLOGIC N_REL_OP
                  N_SIMPLE_ARITHLOGIC
                {
                    printRule("ARITHLOGIC_EXPR",
                              "SIMPLE_ARITHLOGIC REL_OP "
                              "SIMPLE_ARITHLOGIC");
                }
                ;

N_SIMPLE_ARITHLOGIC : N_TERM N_ADD_OP_LIST
                {
                    printRule("SIMPLE_ARITHLOGIC",
                              "TERM ADD_OP_LIST");
                }
                ;

N_ADD_OP_LIST	: N_ADD_OP N_TERM N_ADD_OP_LIST
                {
                    printRule("ADD_OP_LIST",
                              "ADD_OP TERM ADD_OP_LIST");
                }
                | /* epsilon */
                {
                    printRule("ADD_OP_LIST", "epsilon");
                }
                ;

N_TERM		: N_FACTOR N_MULT_OP_LIST
                {
                    printRule("TERM",
                              "FACTOR MULT_OP_LIST");
                }
                ;

N_MULT_OP_LIST	: N_MULT_OP N_FACTOR N_MULT_OP_LIST
                {
                    printRule("MULT_OP_LIST",
                              "MULT_OP FACTOR MULT_OP_LIST");
                }
                | /* epsilon */
                {
                    printRule("MULT_OP_LIST", "epsilon");
                }
                ;

N_FACTOR		: N_VAR
                {
                    printRule("FACTOR", "VAR");
                }
                | N_CONST
                {
                    printRule("FACTOR", "CONST");
                }
                | T_LPAREN N_EXPR T_RPAREN
                {
                    printRule("FACTOR", "( EXPR )");
                }
                | T_NOT N_FACTOR
                {
                    printRule("FACTOR", "! FACTOR");
                }
                ;

N_ADD_OP	     : T_ADD
                {
                    printRule("ADD_OP", "+");
                }
                | T_SUB
                {
                    printRule("ADD_OP", "-");
                }
                | T_OR
                {
                    printRule("ADD_OP", "|");
                }
                ;

N_MULT_OP      : T_MULT
                {
                    printRule("MULT_OP", "*");
                }
                | T_DIV
                {
                    printRule("MULT_OP", "/");
                }
                | T_AND
                {
                    printRule("MULT_OP", "&");
                }
                | T_MOD
                {
                    printRule("MULT_OP", "\%\%");
                }
                | T_POW
                {
                    printRule("MULT_OP", "^");
                }
                ;

N_REL_OP        : T_LT
                {
                    printRule("REL_OP", "<");
                }
                | T_GT
                {
                    printRule("REL_OP", ">");
                }
                | T_LE
                {
                    printRule("REL_OP", "<=");
                }
                | T_GE
                {
                    printRule("REL_OP", ">=");
                }
                | T_EQ
                {
                    printRule("REL_OP", "==");
                }
                | T_NE
                {
                    printRule("REL_OP", "!=");
                }
                ;

N_VAR           : N_ENTIRE_VAR
                {
                    printRule("VAR", "ENTIRE_VAR");
                }
                | N_SINGLE_ELEMENT
                {
                    printRule("VAR", "SINGLE_ELEMENT");
                }
                ;

N_SINGLE_ELEMENT : T_IDENT T_LBRACKET T_LBRACKET N_EXPR
                   T_RBRACKET T_RBRACKET
                {
                    printRule("SINGLE_ELEMENT", "IDENT"
                              " [[ EXPR ]]");
                    string ident = string($1);
                    TYPE_INFO exprTypeInfo = findEntryInAnyScope(ident);
                    if(exprTypeInfo.type == UNDEFINED) {
                        yyerror("Undefined identifier");
                        return(0);
                    }
                }
                ;

N_ENTIRE_VAR    : T_IDENT
                {
                    printRule("ENTIRE_VAR", "IDENT");
                    string ident = string($1);
                    TYPE_INFO exprTypeInfo = findEntryInAnyScope(ident);
                    if(exprTypeInfo.type == UNDEFINED) {
                        yyerror("Undefined identifier");
                        return(0);
                    }
                }
                ;

%%

#include "lex.yy.c"
extern FILE *yyin;

void printTokenInfo(const char* token_type, const char* lexeme)
{
    printf("TOKEN: %s \t\t LEXEME: %s\n", token_type, lexeme);
}

void printRule(const char *lhs, const char *rhs)
{
    printf("%s -> %s\n", lhs, rhs);
    return;
}

void beginScope() {
    scopeStack.push(SYMBOL_TABLE());
    printf("\n___Entering new scope...\n\n");
}

void endScope() {
    scopeStack.pop();
    printf("\n___Exiting scope...\n\n");
}

void cleanUp() {
    if (scopeStack.empty())
        return;
    else {
        scopeStack.pop();
        cleanUp();
    }
}

TYPE_INFO findEntryInAnyScope(const string the_name) {
    TYPE_INFO info = {UNDEFINED, NOT_APPLICABLE, NOT_APPLICABLE};
    if (scopeStack.empty()) return(info);
    info = scopeStack.top().findEntry(the_name);
    if (info.type != UNDEFINED) {
        return(info);
    }
    else { // check in "next higher" scope
        SYMBOL_TABLE symbolTable = scopeStack.top();
        scopeStack.pop();
        info = findEntryInAnyScope(the_name);
        scopeStack.push(symbolTable); // restore the stack
        return(info);
    }
}

int main() 
{
    beginScope();
    do {
        yyparse();
    } while (!feof(yyin));

    cleanUp();

    return 0;
}