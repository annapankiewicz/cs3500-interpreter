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

#define ARITHMETIC_OP   1
#define LOGICAL_OP      2
#define RELATIONAL_OP   3
#define INDEX_PROD      4

// constant to suppress token printing
const bool suppressTokenOutput = false;

int line_num = 1;
int numParams = 0;
int numExprs = 0;
bool identAlreadyExisted = false;

stack<SYMBOL_TABLE> scopeStack; // stack of scope hashtables

bool isIntCompatible(const int theType);
bool isStrCompatible(const int theType);
bool isBoolCompatible(const int theType);
bool isFloatCompatible(const int theType);
bool isListCompatible(const int theType);

bool isIntOrFloatOrBoolCompatible(const int theType);
bool isIntOrStrOrFloatOrBoolCompatible(const int theType);

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
%type <typeInfo> N_SIMPLE_ARITHLOGIC N_TERM N_ADD_OP_LIST
%type <typeInfo> N_FACTOR N_MULT_OP_LIST N_VAR
%type <typeInfo> N_SINGLE_ELEMENT N_ENTIRE_VAR

%type <num> N_PARAM_LIST N_PARAMS N_ARG_LIST N_ARGS N_INDEX
%type <num> N_REL_OP N_ADD_OP N_MULT_OP

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

                    $$.type = $5.type;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
                }
                | T_IF T_LPAREN N_EXPR T_RPAREN N_EXPR T_ELSE
                  N_EXPR
                {
                    printRule("IF_EXPR", "IF ( EXPR ) EXPR ELSE EXPR");
                    if(($3.type == FUNCTION) || ($3.type == LIST)) {
                        yyerror("Arg 1 cannot be function or list");
                        return(0);
                    }
                    if($5.type == FUNCTION) {
                        yyerror("Arg 2 cannot be function");
                        return(0);
                    }
                    if($7.type == FUNCTION) {
                        yyerror("Arg 3 cannot be function");
                        return(0);
                    }

                    // type is combination of the second and third arg types
                    $$.type = $5.type ^ $7.type;
                    $$.numParams = NOT_APPLICABLE;
                    $$.returnType = NOT_APPLICABLE;
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

N_FOR_EXPR      : T_FOR T_LPAREN T_IDENT
                {
                    printRule("FOR_EXPR", 
                              "FOR ( IDENT IN EXPR ) "
                              "LOOP_EXPR");
                    string lexeme = string($3);
                    TYPE_INFO exprTypeInfo = findEntryInAnyScope(lexeme);
                    if(exprTypeInfo.type == UNDEFINED) {
                        printf("___Adding %s to symbol table\n", $3);
                        bool success = scopeStack.top().addEntry(
                            SYMBOL_TABLE_ENTRY(lexeme,
                            {NOT_APPLICABLE, NOT_APPLICABLE, NOT_APPLICABLE}));
                    }
                    else {
                        identAlreadyExisted = true;
                    }
                }
                T_IN N_EXPR T_RPAREN N_LOOP_EXPR
                {
                    string lexeme = string($3);
                    TYPE_INFO exprTypeInfo = findEntryInAnyScope(lexeme);
                    if(($6.type == FUNCTION) || ($6.type == NULL_TYPE)) {
                        yyerror("Arg 5 cannot be function or null");
                    }
                    // check to make sure the T_IDENT type is compatible with
                    // N_EXPR's type
                    if(($6.type == LIST) && (identAlreadyExisted)) {
                        // make sure it's compatible with INT/STRING/BOOL/FLOAT
                        if(isIntOrStrOrFloatOrBoolCompatible(exprTypeInfo.type))
                        // change IDENT's entry to be INT_OR_STR_OR_FLOAT_OR_BOOL
                            scopeStack.top().changeEntry(SYMBOL_TABLE_ENTRY(lexeme,
                            {INT_OR_STR_OR_FLOAT_OR_BOOL,
                            NOT_APPLICABLE, NOT_APPLICABLE}));
                    }
                    else {
                        // ident needs to be compatible with type of N_EXPR, but
                        // since it just got created, we assign it the same type
                        // as N_EXPR
                        scopeStack.top().changeEntry(SYMBOL_TABLE_ENTRY(lexeme,
                            {$6.type, NOT_APPLICABLE, NOT_APPLICABLE}));
                    }
                    identAlreadyExisted = false;
                    $$.type = $8.type;
                    $$.numParams = $8.numParams;
                    $$.returnType = $8.returnType;
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

N_ASSIGNMENT_EXPR : T_IDENT N_INDEX
                {
                    printRule("ASSIGNMENT_EXPR", 
                              "IDENT INDEX ASSIGN EXPR");
                    string lexeme = string($1);
                    TYPE_INFO exprTypeInfo = scopeStack.top().findEntry(lexeme);
                    if(exprTypeInfo.type == UNDEFINED) {
                        printf("___Adding %s to symbol table\n", $1);
                        // add in as not applicable type until the N_EXPR can be
                        // accessed below to get the correct type
                        bool success = scopeStack.top().addEntry(
                            SYMBOL_TABLE_ENTRY(lexeme,
                            {NOT_APPLICABLE, NOT_APPLICABLE, NOT_APPLICABLE}));
                    }
                    else {
                        identAlreadyExisted = true;
                    }
                }
                T_ASSIGN N_EXPR
                {
                    // TODO(anna): this is incredibly ugly
                    // first check for compatibility if the IDENT already existed
                    string lexeme = string($1);
                    TYPE_INFO exprTypeInfo = scopeStack.top().findEntry(lexeme);
                    if(($2 == INDEX_PROD) && (exprTypeInfo.type != LIST)) {
                        yyerror("Arg 2 must be list");
                    }
                    if(identAlreadyExisted) {
                        // check for compatibility with N_EXPR
                        if(!isIntCompatible($5.type) && (isIntCompatible(exprTypeInfo.type)))
                            yyerror("Arg 4 must be an integer");
                        else if((!isStrCompatible($5.type)) && (isStrCompatible(exprTypeInfo.type)))
                            yyerror("Arg 4 must be a string");
                        else if((!isBoolCompatible($5.type)) && (isBoolCompatible(exprTypeInfo.type)))
                            yyerror("Arg 4 must be a bool");
                        else if((!isFloatCompatible($5.type)) && (isFloatCompatible(exprTypeInfo.type)))
                            yyerror("Arg 4 must be a float");
                        else if((!isListCompatible($5.type)) && (isListCompatible(exprTypeInfo.type)))
                            yyerror("Arg 4 must be a list");
                        else if(($5.type != FUNCTION) && (exprTypeInfo.type == FUNCTION))
                            yyerror("Arg 4 must be a function");
                        else if(($5.type != NULL_TYPE) && (exprTypeInfo.type == NULL_TYPE))
                            yyerror("Arg 4 must be null");
                        else {
                            // if it makes it this far, they're compatible
                            // assign the n_expr type to the ident
                            bool success = scopeStack.top().changeEntry(
                                SYMBOL_TABLE_ENTRY(lexeme,
                                {$5.type, NOT_APPLICABLE, NOT_APPLICABLE}));
                        }
                    }
                    else {
                        // if it didn't already exist, just change the type
                        bool success = scopeStack.top().changeEntry(
                            SYMBOL_TABLE_ENTRY(lexeme,
                            {$5.type, NOT_APPLICABLE, NOT_APPLICABLE}));
                    }

                    identAlreadyExisted = false;
                    $$.type = $5.type;
                    $$.numParams = $5.numParams;
                    $$.returnType = $5.returnType;
                }
                ;

N_INDEX :       T_LBRACKET T_LBRACKET N_EXPR T_RBRACKET T_RBRACKET
			    {
                    printRule("INDEX", " [[ EXPR ]]");
                    $$ = INDEX_PROD;
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

N_FUNCTION_DEF  : T_FUNCTION
                {
                    beginScope();
                }
                T_LPAREN N_PARAM_LIST T_RPAREN N_COMPOUND_EXPR
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
                    numParams = 0;
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
                    TYPE_INFO exprTypeInfo = findEntryInAnyScope($1);
                    if (exprTypeInfo.type == UNDEFINED) {
                        yyerror("Undefined identifier");
                        return(0);
                    }
                    else if(exprTypeInfo.type != FUNCTION) {
                        yyerror("Arg 1 must be function");
                    }
                    else {
                        if($3 > exprTypeInfo.numParams) {
                            yyerror("Too many parameters in function call");
                        }
                        if($3 < exprTypeInfo.numParams) {
                            yyerror("Too few parameters in function call");
                        }
                    }
                    $$.type = exprTypeInfo.returnType;
                    $$.numParams = exprTypeInfo.returnType;
                    $$.returnType = exprTypeInfo.returnType;
                    // double check this
                }
                ;

N_ARG_LIST      : N_ARGS
                {
                    printRule("ARG_LIST", "ARGS");
                    $$ = $1;
                    numExprs = 0;
                }
                | N_NO_ARGS
                {
                    printRule("ARG_LIST", "NO_ARGS");
                    numExprs = 0;
                    $$ = numExprs;
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
                    numExprs++;
                    if($1.type != INT) {
                        string errorMsg = "Arg " + to_string(numExprs) + " must be integer";
                        yyerror(errorMsg.c_str());
                    }
                    $$ = numExprs;
                }
                | N_EXPR T_COMMA N_ARGS
                {
                    printRule("ARGS", "EXPR, ARGS");
                    numExprs++;
                    if($1.type != INT) {
                        string errorMsg = "Arg " + to_string(numExprs) + " must be integer";
                        yyerror(errorMsg.c_str());
                    }
                    $$ = numExprs;
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
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                ;

N_ADD_OP_LIST	: N_ADD_OP N_TERM N_ADD_OP_LIST
                {
                    printRule("ADD_OP_LIST",
                              "ADD_OP TERM ADD_OP_LIST");
                    if(($2.type == FUNCTION) || ($2.type == NULL_TYPE) || ($2.type == LIST)) {
                        yyerror("Arg 2 cannot be function or null or list");
                    }
                    if(!(isIntOrFloatOrBoolCompatible($2.type))) {
                        yyerror("Arg 2 must be integer or float or bool");
                    }
                    switch($1) {
                        case ARITHMETIC_OP:
                            if(($2.type == INT) && ($3.type == INT)){
                                $$.type = INT;
                            }
                            else if(($2.type == FLOAT) || ($3.type == FLOAT)) {
                                $$.type = FLOAT;
                            }
                            break;
                        case LOGICAL_OP:
                            $$.type = BOOL;
                            break;
                        default:
                            break;
                    }
                    $$.numParams = $2.numParams;
                    $$.returnType = $2.returnType;
                }
                | /* epsilon */
                {
                    printRule("ADD_OP_LIST", "epsilon");
                }
                ;

N_TERM		    : N_FACTOR N_MULT_OP_LIST
                {
                    printRule("TERM",
                              "FACTOR MULT_OP_LIST");
                    $$.type = $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                ;

N_MULT_OP_LIST	: N_MULT_OP N_FACTOR N_MULT_OP_LIST
                {
                    printRule("MULT_OP_LIST",
                              "MULT_OP FACTOR MULT_OP_LIST");
                    if(($2.type == FUNCTION) || ($2.type == NULL_TYPE) || ($2.type == LIST)) {
                        yyerror("Arg 2 cannot be function or null or list");
                    }
                    if(!(isIntOrFloatOrBoolCompatible($2.type))) {
                        yyerror("Arg 2 must be integer or float or bool");
                    }
                    switch($1) {
                        case ARITHMETIC_OP:
                            if(($2.type == INT) && ($3.type == INT)){
                                $$.type = INT;
                            }
                            else if(($2.type == FLOAT) || ($3.type == FLOAT)) {
                                $$.type = FLOAT;
                            }
                            break;
                        case LOGICAL_OP:
                            $$.type = BOOL;
                        default:
                            break;
                    }
                    $$.numParams = $2.numParams;
                    $$.returnType = $2.returnType;

                }
                | /* epsilon */
                {
                    printRule("MULT_OP_LIST", "epsilon");
                }
                ;

N_FACTOR		: N_VAR
                {
                    printRule("FACTOR", "VAR");
                    $$.type == $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_CONST
                {
                    printRule("FACTOR", "CONST");
                    $$.type == $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | T_LPAREN N_EXPR T_RPAREN
                {
                    printRule("FACTOR", "( EXPR )");
                    $$.type == $2.type;
                    $$.numParams = $2.numParams;
                    $$.returnType = $2.returnType;
                }
                | T_NOT N_FACTOR
                {
                    printRule("FACTOR", "! FACTOR");
                    $$.type == $2.type;
                    $$.numParams = $2.numParams;
                    $$.returnType = $2.returnType;
                }
                ;

N_ADD_OP	     : T_ADD
                {
                    printRule("ADD_OP", "+");
                    $$ = ARITHMETIC_OP;
                }
                | T_SUB
                {
                    printRule("ADD_OP", "-");
                    $$ = ARITHMETIC_OP;
                }
                | T_OR
                {
                    printRule("ADD_OP", "|");
                    $$ = LOGICAL_OP;
                }
                ;

N_MULT_OP      : T_MULT
                {
                    printRule("MULT_OP", "*");
                    $$ = ARITHMETIC_OP;
                }
                | T_DIV
                {
                    printRule("MULT_OP", "/");
                    $$ = ARITHMETIC_OP;
                }
                | T_AND
                {
                    printRule("MULT_OP", "&");
                    $$ = LOGICAL_OP;
                }
                | T_MOD
                {
                    printRule("MULT_OP", "\%\%");
                    $$ = ARITHMETIC_OP;
                }
                | T_POW
                {
                    printRule("MULT_OP", "^");
                    $$ = ARITHMETIC_OP;
                }
                ;

N_REL_OP        : T_LT
                {
                    printRule("REL_OP", "<");
                    $$ = RELATIONAL_OP;
                }
                | T_GT
                {
                    printRule("REL_OP", ">");
                    $$ = RELATIONAL_OP;
                }
                | T_LE
                {
                    printRule("REL_OP", "<=");
                    $$ = RELATIONAL_OP;
                }
                | T_GE
                {
                    printRule("REL_OP", ">=");
                    $$ = RELATIONAL_OP;
                }
                | T_EQ
                {
                    printRule("REL_OP", "==");
                    $$ = RELATIONAL_OP;
                }
                | T_NE
                {
                    printRule("REL_OP", "!=");
                    $$ = RELATIONAL_OP;
                }
                ;

N_VAR           : N_ENTIRE_VAR
                {
                    printRule("VAR", "ENTIRE_VAR");
                    $$.type == $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                | N_SINGLE_ELEMENT
                {
                    printRule("VAR", "SINGLE_ELEMENT");
                    $$.type == $1.type;
                    $$.numParams = $1.numParams;
                    $$.returnType = $1.returnType;
                }
                ;

N_SINGLE_ELEMENT : T_IDENT T_LBRACKET T_LBRACKET N_EXPR
                   T_RBRACKET T_RBRACKET
                {
                    printRule("SINGLE_ELEMENT", "IDENT"
                              " [[ EXPR ]]");
                    TYPE_INFO exprTypeInfo = findEntryInAnyScope($1);
                    if(exprTypeInfo.type == UNDEFINED) {
                        yyerror("Undefined identifier");
                        return(0);
                    }
                    if(exprTypeInfo.type != LIST) {
                        yyerror("Arg 1 must be list");
                        return(0);
                    }
                    else {
                        $$.type = INT_OR_STR_OR_FLOAT_OR_BOOL;
                        $$.numParams = NOT_APPLICABLE;
                        $$.returnType = NOT_APPLICABLE;
                    }
                }
                ;

N_ENTIRE_VAR    : T_IDENT
                {
                    printRule("ENTIRE_VAR", "IDENT");
                    TYPE_INFO exprTypeInfo = findEntryInAnyScope($1);
                    if(exprTypeInfo.type == UNDEFINED)
                    {
                        yyerror("Undefined identifier");
                        return(0);
                    }
                    $$.type = exprTypeInfo.type;
                    $$.numParams = exprTypeInfo.numParams;
                    $$.returnType = exprTypeInfo.returnType;
                }
                ;

%%

#include "lex.yy.c"
extern FILE *yyin;

void printTokenInfo(const char* token_type, const char* lexeme)
{
    if(!suppressTokenOutput) {
        printf("TOKEN: %s \t\t LEXEME: %s\n", token_type, lexeme);
    }
}

void printRule(const char *lhs, const char *rhs)
{
    printf("%s -> %s\n", lhs, rhs);
    return;
}

bool isIntCompatible(const int theType)
{
    return((theType == INT) || (theType == INT_OR_STR) ||
        (theType == INT_OR_BOOL) || (theType == INT_OR_FLOAT) ||
        (theType == LIST_OR_INT) || (theType == INT_OR_STR_OR_BOOL) ||
        (theType == INT_OR_STR_OR_FLOAT) ||
        (theType == INT_OR_BOOL_OR_FLOAT) ||
        (theType == LIST_OR_INT_OR_FLOAT) ||
        (theType == LIST_OR_INT_OR_STR) || (theType == LIST_OR_INT_OR_BOOL) ||
        (theType == INT_OR_STR_OR_FLOAT_OR_BOOL) ||
        (theType == LIST_OR_BOOL_OR_STR_OR_INT) ||
        (theType == LIST_OR_FLOAT_OR_STR_OR_INT) ||
        (theType == INT_OR_BOOL_OR_FLOAT_OR_LIST) ||
        (theType == INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST));
}

bool isStrCompatible(const int theType)
{
    return((theType == STR) || (theType == INT_OR_STR) ||
        (theType == STR_OR_BOOL) || (theType == STR_OR_FLOAT) ||
        (theType == LIST_OR_STR) || (theType == INT_OR_STR_OR_BOOL) ||
        (theType == INT_OR_STR_OR_FLOAT) ||
        (theType == STR_OR_BOOL_OR_FLOAT) ||
        (theType == LIST_OR_INT_OR_STR) ||
        (theType == LIST_OR_STR_OR_BOOL) ||
        (theType == LIST_OR_STR_OR_FLOAT) ||
        (theType == INT_OR_STR_OR_FLOAT_OR_BOOL) ||
        (theType == LIST_OR_FLOAT_OR_BOOL_OR_STR) ||
        (theType == LIST_OR_BOOL_OR_STR_OR_INT) ||
        (theType == LIST_OR_FLOAT_OR_STR_OR_INT) ||
        (theType == INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST));
}

bool isBoolCompatible(const int theType)
{
    return((theType == BOOL) || (theType == INT_OR_BOOL) ||
           (theType == STR_OR_BOOL) || (theType == BOOL_OR_FLOAT) ||
           (theType == LIST_OR_BOOL) || (theType == INT_OR_STR_OR_BOOL) ||
           (theType == INT_OR_BOOL_OR_FLOAT) || (theType == STR_OR_BOOL_OR_FLOAT) ||
           (theType == LIST_OR_INT_OR_BOOL) || (theType == LIST_OR_STR_OR_BOOL) ||
           (theType == LIST_OR_BOOL_OR_FLOAT) ||
           (theType == INT_OR_STR_OR_FLOAT_OR_BOOL) ||
           (theType == LIST_OR_FLOAT_OR_BOOL_OR_STR) ||
           (theType == LIST_OR_BOOL_OR_STR_OR_INT) ||
           (theType == INT_OR_BOOL_OR_FLOAT_OR_LIST) ||
           (theType == INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST));
}

bool isFloatCompatible(const int theType)
{
    return((theType == FLOAT) || (theType == INT_OR_FLOAT) ||
           (theType == STR_OR_FLOAT) || (theType == BOOL_OR_FLOAT) ||
           (theType == LIST_OR_FLOAT) || (theType == INT_OR_STR_OR_FLOAT) ||
           (theType == INT_OR_BOOL_OR_FLOAT) || (theType == STR_OR_BOOL_OR_FLOAT) ||
           (theType == LIST_OR_INT_OR_FLOAT) || (theType == LIST_OR_STR_OR_FLOAT) ||
           (theType == LIST_OR_BOOL_OR_FLOAT) ||
           (theType == INT_OR_STR_OR_FLOAT_OR_BOOL) ||
           (theType == LIST_OR_FLOAT_OR_BOOL_OR_STR) ||
           (theType == LIST_OR_FLOAT_OR_STR_OR_INT) ||
           (theType == INT_OR_BOOL_OR_FLOAT_OR_LIST) ||
           (theType == INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST));
}

bool isListCompatible(const int theType)
{
    return((theType == LIST) || (theType == LIST_OR_INT) ||
           (theType == LIST_OR_STR) || (theType == LIST_OR_BOOL) ||
           (theType == LIST_OR_FLOAT) || (theType == LIST_OR_INT_OR_STR) ||
           (theType == LIST_OR_INT_OR_BOOL) || (theType == LIST_OR_INT_OR_FLOAT) ||
           (theType == LIST_OR_STR_OR_BOOL) || (theType == LIST_OR_STR_OR_FLOAT) ||
           (theType == LIST_OR_BOOL_OR_FLOAT) ||
           (theType == LIST_OR_FLOAT_OR_BOOL_OR_STR) ||
           (theType == LIST_OR_BOOL_OR_STR_OR_INT) ||
           (theType == LIST_OR_FLOAT_OR_STR_OR_INT) ||
           (theType == INT_OR_BOOL_OR_FLOAT_OR_LIST) ||
           (theType == INT_OR_BOOL_OR_STR_OR_FLOAT_OR_LIST));
}

bool isIntOrFloatOrBoolCompatible(const int theType)
{
    return((isIntCompatible(theType)) || (isFloatCompatible(theType)) ||
           (isBoolCompatible(theType)));
}

bool isIntOrStrOrFloatOrBoolCompatible(const int theType) {
    return((isIntCompatible(theType)) || (isStrCompatible(theType)) ||
           (isFloatCompatible(theType)) || (isBoolCompatible(theType)));
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