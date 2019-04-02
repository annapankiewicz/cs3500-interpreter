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
#include <string.h>
#include <string>
#include <cstring>
#include <math.h>
#include <stack>
#include <list>
#include "SymbolTable.h"
using namespace std;

#define ARITHMETIC_OP		1
#define LOGICAL_OP		2
#define RELATIONAL_OP		3
#define UNARY_OP			4

#define ADD   	1	    // encoding for individual ops
#define SUB   	2
#define MULT  	3
#define DIV   	4
#define MOD	5
#define POW	6
#define LT		10
#define GT		11
#define LE		12
#define GE		13
#define EQ		14
#define NE		15
#define AND	100
#define OR		101
#define NOT	1000

#define ERR_CANNOT_BE_FUNCT_NULL_LIST_OR_STR	0
#define ERR_CANNOT_BE_FUNCT					1 
#define ERR_CANNOT_BE_FUNCT_OR_NULL			2
#define ERR_CANNOT_BE_FUNCT_OR_NULL_OR_LIST		3
#define ERR_CANNOT_BE_LIST					4 
#define ERR_MUST_BE_LIST					5
#define ERR_MUST_BE_FUNCT					6
#define ERR_MUST_BE_INTEGER					7
#define ERR_MUST_BE_INT_FLOAT_OR_BOOL			8
#define ERR_TOO_FEW_PARAMS					9
#define ERR_TOO_MANY_PARAMS					10
#define ERR_NON_INT_FUNCT_PARAM				11
#define ERR_MULTIPLY_DEFINED_IDENT			12
#define ERR_UNDEFINED_IDENT					13
#define ERR_DIVISION_BY_ZERO				14
#define ERR_SUBSCRIPT_OUT_OF_BOUNDS			15
#define ERR_ERROR						16

const int NUM_ERR_MESSAGES = 17;  // should be ERR_ERROR + 1

const string ERR_MSG[NUM_ERR_MESSAGES] = {
"cannot be function or null or list or string",
"cannot be function",
"cannot be function or null",
"cannot be function or null or list",
"cannot be list",
"must be list",
"must be function",
"must be integer",
"must be integer or float or bool",
"Too few parameters in function call",
"Too many parameters in function call",
"Function parameters must be integer",
"Multiply defined identifier",
"Undefined identifier",
"Attempted division by zero",
"Subscript out of bounds",
"<undefined error>"
};

// set to true to suppress token, etc. printing
const bool suppressTokenOutput = true;

// info about list index
typedef struct
{
  int indexValue;
  bool indexUsed;
} INDEX_INFO;

int line_num = 1;
int numExprs = 0;

LIST_INFO_PTR listInfoPtr = NULL;

stack<SYMBOL_TABLE> scopeStack; // stack of scope hashtables

bool isIntOrFloatOrBoolCompatible(const int theType);
bool isIntCompatible(const int theType);
bool isBoolCompatible(const int theType);
bool isFloatCompatible(const int theType);
bool isListCompatible(const int theType);
bool isInvalidOperandType(const int theType);

int operatorCategory(const int opCode);
bool compareValues(const float val1, const float val2,
                   const int opCode);
float getValAsFloat(const TYPE_INFO x);
int getValAsInt(const TYPE_INFO x);
float calcValues(const float val1, const float val2,
                 const int opCode);
int calcValues(const int val1, const int val2,
               const int opCode);
void copyListInfo(LIST_INFO_PTR &target, LIST_INFO_PTR src);
void printList(LIST_INFO_PTR x);
LIST_ENTRY getNthEntry(LIST_INFO_PTR x, const int index);
void changeListInfo(LIST_INFO_PTR &x, const int n, 
                    const TYPE_INFO info);


void beginScope();
void endScope();
void cleanUp();
TYPE_INFO findEntryInAnyScope(const string the_name);

void semanticError(const int argNum, const int errNum);

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
    float float_num;
    bool boolean;
    bool flag;
    TYPE_INFO typeInfo;
    INDEX_INFO indexInfo;
};

%token T_IDENT T_INTCONST T_FLOATCONST T_UNKNOWN T_STRCONST 
%token T_IF T_ELSE
%token T_TRUE T_FALSE T_QUIT
%token T_PRINT T_CAT T_READ T_LPAREN T_RPAREN T_LBRACE 
%token T_RBRACE T_LBRACKET
%token T_RBRACKET T_SEMICOLON T_COMMA T_ADD T_SUB 
%token T_MULT T_DIV T_MOD
%token T_POW T_LT T_LE T_GT T_GE T_EQ T_NE T_NOT T_AND 
%token T_OR T_ASSIGN T_LIST

%type <typeInfo> N_EXPR N_IF_EXPR N_THEN_EXPR N_COND_IF 
%type <typeInfo> N_COMPOUND_EXPR N_ARITHLOGIC_EXPR
%type <typeInfo> N_ASSIGNMENT_EXPR 
%type <typeInfo> N_INPUT_EXPR N_OUTPUT_EXPR N_LIST_EXPR
%type <typeInfo> N_QUIT_EXPR N_CONST N_EXPR_LIST
%type <typeInfo> N_SIMPLE_ARITHLOGIC N_TERM N_ADD_OP_LIST
%type <typeInfo> N_FACTOR N_MULT_OP_LIST N_VAR
%type <typeInfo> N_SINGLE_ELEMENT N_ENTIRE_VAR

%type <num> N_REL_OP N_ADD_OP N_MULT_OP T_INTCONST
%type <boolean> T_TRUE T_FALSE
%type <float_num> T_FLOATCONST
%type <text> T_IDENT T_STRCONST
%type <indexInfo> N_INDEX

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
				printf("\n---- Completed parsing ----\n");
				printf("\nValue of the expression is: ");
				switch ($1.type)
				{
				case (INT) :
					printf("%d\n", $1.intVal);
					break;
				case (FLOAT) :
					printf("%6.2f\n", $1.floatVal);
					break;
				case (STR) :
					printf("%s\n", $1.strVal);
					break;
				case (BOOL) :
			        if ($1.boolVal)
			    		printf("TRUE\n");
			  		else printf("FALSE\n");
			  		break;
				case (NULL_TYPE) :
					printf("NULL\n");
			  		break;
				case (LIST) :
					printList($1.listVal);
			  		break;
				default:  // should never happen!
			 		 printf("***INVALID TYPE %d\n",
					        $1.type);
				} // end switch
				return 0;
                }
                ;
N_EXPR          : N_IF_EXPR
                {
                    	printRule("EXPR", "IF_EXPR");
                    	$$.type = $1.type;
                    	strcpy($$.strVal, $1.strVal);
                    	$$.intVal = $1.intVal;
                    	$$.floatVal = $1.floatVal;
                    	$$.boolVal = $1.boolVal;
                    	$$.nullVal = $1.nullVal;
				copyListInfo($$.listVal, $1.listVal);
                }
                | N_COMPOUND_EXPR
                {
                    	printRule("EXPR", "COMPOUND_EXPR");
                    	$$.type = $1.type;
                    	strcpy($$.strVal, $1.strVal);
                    	$$.intVal = $1.intVal;
                    	$$.floatVal = $1.floatVal;
                    	$$.boolVal = $1.boolVal;
                    	$$.nullVal = $1.nullVal;
				copyListInfo($$.listVal, $1.listVal);
                }
                | N_ARITHLOGIC_EXPR
                {
                    	printRule("EXPR", "ARITHLOGIC_EXPR");
                    	$$.type = $1.type;
                    	strcpy($$.strVal, $1.strVal);
                    	$$.intVal = $1.intVal;
                    	$$.floatVal = $1.floatVal;
                    	$$.boolVal = $1.boolVal;
                    	$$.nullVal = $1.nullVal;
				copyListInfo($$.listVal, $1.listVal);
                }
                | N_ASSIGNMENT_EXPR
                {
                    	printRule("EXPR", "ASSIGNMENT_EXPR");
                    	$$.type = $1.type;
                    	strcpy($$.strVal, $1.strVal);
                    	$$.intVal = $1.intVal;
                    	$$.floatVal = $1.floatVal;
                    	$$.boolVal = $1.boolVal;
                    	$$.nullVal = $1.nullVal;
				copyListInfo($$.listVal, $1.listVal);
                }
                | N_OUTPUT_EXPR
                {
                    	printRule("EXPR", "OUTPUT_EXPR");
                    	$$.type = $1.type;
                    	strcpy($$.strVal, $1.strVal);
                    	$$.intVal = $1.intVal;
                    	$$.floatVal = $1.floatVal;
                    	$$.boolVal = $1.boolVal;
                    	$$.nullVal = $1.nullVal;
				if ($1.type != NULL_TYPE)
				  copyListInfo($$.listVal, $1.listVal);
				else $$.listVal = NULL;
                }
                | N_INPUT_EXPR
                {
                    	printRule("EXPR", "INPUT_EXPR");
                    	$$.type = $1.type;
                    	strcpy($$.strVal, $1.strVal);
                    	$$.intVal = $1.intVal;
                    	$$.floatVal = $1.floatVal;
                    	$$.boolVal = $1.boolVal;
                    	$$.nullVal = $1.nullVal;
				$$.listVal = NULL;
                }
                | N_LIST_EXPR
                {
                    	printRule("EXPR", "LIST_EXPR");
                    	$$.type = $1.type;
                    	strcpy($$.strVal, $1.strVal);
                    	$$.intVal = $1.intVal;
                    	$$.floatVal = $1.floatVal;
                    	$$.boolVal = $1.boolVal;
                    	$$.nullVal = $1.nullVal;
				copyListInfo($$.listVal, $1.listVal);
                }
                | N_QUIT_EXPR
                {
                    	printRule("EXPR", "QUIT_EXPR");
                    	exit(1);
                }
                ;

N_CONST         : T_INTCONST
                {
                    	printRule("CONST", "INTCONST");
                    	$$.type = INT;
                    	strcpy($$.strVal, "");
                    	$$.intVal = $1;
                    	$$.floatVal = NOT_APPLICABLE;
                    	$$.boolVal = false;
                    	$$.nullVal = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                | T_STRCONST
                {
                    	printRule("CONST", "STRCONST");
				// Convert char* to string to CSTRING
                    	$$.type = STR;
				string s;
				s.assign($1);
                    	strcpy($$.strVal, s.c_str());
                    	$$.intVal = NOT_APPLICABLE;
                    	$$.floatVal = NOT_APPLICABLE;
                    	$$.boolVal = false;
                    	$$.nullVal = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                | T_FLOATCONST
                {
                    	printRule("CONST", "FLOATCONST");
                    	$$.type = FLOAT;
                    	strcpy($$.strVal, "");
                    	$$.intVal = NOT_APPLICABLE;
                    	$$.floatVal = $1;
                    	$$.boolVal = false;
                    	$$.nullVal = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                | T_TRUE
                {
                    	printRule("CONST", "TRUE");
                    	$$.type = BOOL;
                    	strcpy($$.strVal, "");
                    	$$.intVal = NOT_APPLICABLE;
                    	$$.floatVal = NOT_APPLICABLE;
                    	$$.boolVal = true;
                    	$$.nullVal = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                | T_FALSE
                {
                    	printRule("CONST", "FALSE");
                    	$$.type = BOOL;
                    	strcpy($$.strVal, "");
                    	$$.intVal = NOT_APPLICABLE;
                    	$$.floatVal = NOT_APPLICABLE;
                    	$$.boolVal = false;
                    	$$.nullVal = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                ;

N_COMPOUND_EXPR : T_LBRACE N_EXPR N_EXPR_LIST T_RBRACE
                {
                    	printRule("COMPOUND_EXPR",
                                "{ EXPR EXPR_LIST }");
			    	if ($3.type == NOT_APPLICABLE)
			    	{
				  $$.type = $2.type;
				  strcpy($$.strVal, $2.strVal);
				  $$.intVal = $2.intVal;
				  $$.floatVal = $2.floatVal;
				  $$.boolVal = $2.boolVal;
				  $$.nullVal = $2.nullVal;
				  copyListInfo($$.listVal, $2.listVal);
			    	}
			    	else
			    	{
				  $$.type = $3.type;
				  strcpy($$.strVal, $3.strVal);
				  $$.intVal = $3.intVal;
				  $$.floatVal = $3.floatVal;
				  $$.boolVal = $3.boolVal;
				  $$.nullVal = $3.nullVal;
				  copyListInfo($$.listVal, $3.listVal);
			    	}
                }
                ;

N_EXPR_LIST     : T_SEMICOLON N_EXPR N_EXPR_LIST
                {
				printRule("EXPR_LIST", "; EXPR EXPR_LIST");
				if ($3.type == NOT_APPLICABLE)
				{
				  $$.type = $2.type;
				  strcpy($$.strVal, $2.strVal);
				  $$.intVal = $2.intVal;
				  $$.floatVal = $2.floatVal;
				  $$.boolVal = $2.boolVal;
				  $$.nullVal = $2.nullVal;
				  copyListInfo($$.listVal, $2.listVal);
				}
				else
				{
				  $$.type = $3.type;
				  strcpy($$.strVal, $3.strVal);
				  $$.intVal = $3.intVal;
				  $$.floatVal = $3.floatVal;
				  $$.boolVal = $3.boolVal;
				  $$.nullVal = $3.nullVal;
				  copyListInfo($$.listVal, $3.listVal);
				}
                }
                | /* epsilon */
                {
				printRule("EXPR_LIST", "epsilon");
				$$.type = NOT_APPLICABLE;
				strcpy($$.strVal, "");
				$$.intVal = NOT_APPLICABLE;
				$$.floatVal = NOT_APPLICABLE;
				$$.boolVal = NOT_APPLICABLE;
				$$.nullVal = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                ;

N_IF_EXPR       : N_COND_IF T_RPAREN N_THEN_EXPR 
                {
				printRule("IF_EXPR", "IF ( EXPR ) EXPR");
				if ($1.boolVal)
				{
				  $$.type = $3.type;
				  strcpy($$.strVal, $3.strVal);
				  $$.intVal = $3.intVal;
				  $$.floatVal = $3.floatVal;
				  $$.boolVal = $3.boolVal;
				  $$.nullVal = $3.nullVal;
				  copyListInfo($$.listVal, $3.listVal);
				}
				else
				{
				  $$.type = NULL_TYPE;
				  strcpy($$.strVal, "");
				  $$.intVal = NOT_APPLICABLE;
				  $$.floatVal = NOT_APPLICABLE;
				  $$.boolVal = NOT_APPLICABLE;
				  $$.nullVal = NULL_VALUE;
				  $$.listVal = NULL;
				}
			}
                | N_COND_IF T_RPAREN N_THEN_EXPR T_ELSE N_EXPR 
                {
                    	printRule("IF_EXPR", 
                              "IF ( EXPR ) EXPR ELSE EXPR");
				if ($1.boolVal)
				{
				  $$.type = $3.type;
				  strcpy($$.strVal, $3.strVal);
				  $$.intVal = $3.intVal;
				  $$.floatVal = $3.floatVal;
				  $$.boolVal = $3.boolVal;
				  $$.nullVal = $3.nullVal;
				  copyListInfo($$.listVal, $3.listVal);
				}
				else
				{
				  $$.type = $5.type;
				  strcpy($$.strVal, $5.strVal);
				  $$.intVal = $5.intVal;
				  $$.floatVal = $5.floatVal;
				  $$.boolVal = $5.boolVal;
				  $$.nullVal = $5.nullVal;
				  copyListInfo($$.listVal, $5.listVal);
				}
			}
                ;

N_COND_IF	  	: T_IF T_LPAREN N_EXPR 
			{
                    	if(($3.type == LIST) ||
                       ($3.type == NULL_TYPE) || 
                       ($3.type == STR)) 
                     	  semanticError(1,
                         ERR_CANNOT_BE_FUNCT_NULL_LIST_OR_STR);
                    	strcpy($$.strVal, "");
                    	$$.intVal = 0;
                    	$$.floatVal = 0;
				$$.nullVal = 0;
				$$.listVal = NULL;
				if ((($3.intVal != 0) && 
                           ($3.intVal != NOT_APPLICABLE)) || 
				    (($3.floatVal != 0) && 
                           ($3.floatVal != NOT_APPLICABLE)) || 
				    ($3.boolVal == true))
				  $$.boolVal = true;
				else $$.boolVal = false;
			}
			; 

N_THEN_EXPR	: N_EXPR
			{
				$$.type = $1.type;
				strcpy($$.strVal, $1.strVal);
				$$.intVal = $1.intVal;
				$$.floatVal = $1.floatVal;
				$$.boolVal = $1.boolVal;
				$$.nullVal = $1.nullVal;
				copyListInfo($$.listVal, $1.listVal);
			}
			;

N_LIST_EXPR     : T_LIST T_LPAREN N_CONST_LIST T_RPAREN
                {
				printRule("LIST_EXPR", 
					    "LIST ( CONST_LIST )");
				$$.type = LIST;
				strcpy($$.strVal, "");
				$$.intVal = NOT_APPLICABLE;
				$$.floatVal = NOT_APPLICABLE;
				$$.boolVal = NOT_APPLICABLE;
				$$.nullVal = NOT_APPLICABLE;
				copyListInfo($$.listVal, listInfoPtr);
				listInfoPtr = NULL;
                }
                ;

N_CONST_LIST    : N_CONST T_COMMA N_CONST_LIST
                {
				printRule("CONST_LIST", 
				          "CONST, CONST_LIST");
				LIST_ENTRY entry;
				entry.type = $1.type;
				strcpy(entry.strVal, $1.strVal);
				entry.intVal = $1.intVal;
				entry.floatVal = $1.floatVal; 
				entry.boolVal = $1.boolVal;
				listInfoPtr->push_front(entry);
                }
                | N_CONST
                {
				printRule("CONST_LIST", "CONST");
				LIST_ENTRY entry;
				entry.type = $1.type;
				strcpy(entry.strVal, $1.strVal);
				entry.intVal = $1.intVal;
				entry.floatVal = $1.floatVal; 
				entry.boolVal = $1.boolVal;
				listInfoPtr = new list<LIST_ENTRY>;
				listInfoPtr->push_front(entry);
                }
                ;

N_ASSIGNMENT_EXPR : T_IDENT N_INDEX
                {
                    printRule("ASSIGNMENT_EXPR", 
                              "IDENT INDEX ASSIGN EXPR");
                    string lexeme = string($1);
                    TYPE_INFO exprTypeInfo =
                        scopeStack.top().findEntry(lexeme);
                    if(exprTypeInfo.type == UNDEFINED) 
			   {
                      if(!suppressTokenOutput)
                        printf("___Adding %s to symbol table\n", 
                               $1);
                      // add in as N/A type until the
                      // N_EXPR can be processed below to
                      // get the correct type
                      scopeStack.top().
				  addEntry(SYMBOL_TABLE_ENTRY(lexeme));
                      // set flag that ident didn't already
				// exist
			     $<flag>$ = false;
                    }
                    else 
			   {
                     // set flag that ident already existed
				$<flag>$ = true;
                    }
                }
                T_ASSIGN N_EXPR
                {
				string lexeme = string($1);
				TYPE_INFO exprTypeInfo = 
                        scopeStack.top().findEntry(lexeme);
				if ($2.indexUsed && $<flag>3 && 
			         (exprTypeInfo.type != LIST))
				  semanticError(1, ERR_MUST_BE_LIST);
				if ($2.indexUsed && ($5.type == LIST))
				  semanticError(2, ERR_CANNOT_BE_LIST);
				if (exprTypeInfo.type == LIST)
				{
				  int index = $2.indexValue;
				  changeListInfo(exprTypeInfo.listVal, 
				                 index, $5);
				}
				else
				{
				  exprTypeInfo.type = $5.type;
				  exprTypeInfo.intVal= $5.intVal;
				  strcpy(exprTypeInfo.strVal, $5.strVal);
				  exprTypeInfo.floatVal= $5.floatVal;
				  exprTypeInfo.boolVal= $5.boolVal;
				  exprTypeInfo.nullVal= $5.nullVal;
				  copyListInfo(exprTypeInfo.listVal, 
				               $5.listVal);
				}
				scopeStack.top().changeEntry(
				  SYMBOL_TABLE_ENTRY(lexeme,
							     exprTypeInfo));
				$$.type = $5.type;
				strcpy($$.strVal, $5.strVal);
				$$.intVal = $5.intVal;
				$$.floatVal = $5.floatVal;
				$$.boolVal = $5.boolVal;
				$$.nullVal = $5.nullVal;
				copyListInfo($$.listVal, $5.listVal);
                }
                ;

N_INDEX :       T_LBRACKET T_LBRACKET N_EXPR T_RBRACKET
                T_RBRACKET
			{
				printRule("INDEX", " [[ EXPR ]]");
				$$.indexValue = getValAsInt($3);
				$$.indexUsed = true;
			}
			| /* epsilon */
                {
				printRule("INDEX", " epsilon");
				$$.indexValue = 0;
				$$.indexUsed = false;
                }
                ;

N_QUIT_EXPR     : T_QUIT T_LPAREN T_RPAREN
                {
				printRule("QUIT_EXPR", "QUIT()");
                }
                ;

N_OUTPUT_EXPR   : T_PRINT T_LPAREN N_EXPR T_RPAREN
                {
				printRule("OUTPUT_EXPR", 
					     "PRINT ( EXPR )");
				if ($3.type == NULL_TYPE) 
				  semanticError(1,
					ERR_CANNOT_BE_FUNCT_OR_NULL);
				switch($3.type)
				{ 
				case (INT) :
					printf("%d\n", $3.intVal);
					break;
				case (FLOAT) :
					printf("%6.2f\n", $3.floatVal);
					break;
				case (STR) :
					printf("%s\n", $3.strVal);
					break;
				case (BOOL) :
				  	if ($3.boolVal)
			    		  printf("TRUE\n");
			  		else printf("FALSE\n");
			  		break;
				case (LIST) :
					printList($3.listVal);
			  		break;
				}
				$$.type = $3.type;
                    	strcpy($$.strVal, $3.strVal);
				$$.intVal = $3.intVal;
				$$.floatVal = $3.floatVal;
				$$.boolVal = $3.boolVal;
				$$.nullVal = $3.nullVal;
				copyListInfo($$.listVal, $3.listVal);
                }
                | T_CAT T_LPAREN N_EXPR T_RPAREN
                {
                    printRule("OUTPUT_EXPR", 
                              "CAT ( EXPR )");
				if ($3.type == NULL_TYPE) 
				  semanticError(1,
					ERR_CANNOT_BE_FUNCT_OR_NULL);
				switch($3.type)
				{ 
				case (INT) :
					printf("%d\n", $3.intVal);
					break;
				case (FLOAT) :
					printf("%6.2f\n", $3.floatVal);
					break;
				case (STR) :
					printf("%s\n", $3.strVal);
					break;
				case (BOOL) :
				  	if ($3.boolVal)
			    		  printf("TRUE\n");
			  		else printf("FALSE\n");
			  		break;
				case (LIST) :
					printList($3.listVal);
			  		break;
				}
				$$.type = NULL_TYPE;
                    	strcpy($$.strVal, "");
				$$.intVal = NOT_APPLICABLE ;
				$$.floatVal = NOT_APPLICABLE;
				$$.boolVal = NOT_APPLICABLE;
				$$.nullVal = NULL_VALUE;
				$$.listVal = NULL;
                }
                ;

N_INPUT_EXPR    : T_READ T_LPAREN T_RPAREN
                {
				printRule("INPUT_EXPR", "READ ( )");
				$$.boolVal = NOT_APPLICABLE;
				$$.nullVal = NOT_APPLICABLE;
				$$.listVal = NULL;
				char buf[256];
				cin.getline(buf, 255);
				if (isdigit(buf[0]) || (buf[0] == '+') || 
				    (buf[0] == '-')) 
				{
                    	  strcpy($$.strVal, "");
				  if (strstr(buf, ".") != NULL)
				  {
				    $$.type = FLOAT;
				    $$.intVal = NOT_APPLICABLE;
				    $$.floatVal = atof(buf);
				  }
				  else
				  {
				    $$.type = INT;
				    $$.intVal = atoi(buf);
				    $$.floatVal = NOT_APPLICABLE;
				  }
				}
				else
				{
				  $$.type = STR;
                    	  strcpy($$.strVal, buf);
				  $$.intVal = NOT_APPLICABLE ;
				  $$.floatVal = NOT_APPLICABLE;
				}
                }
                ;

N_ARITHLOGIC_EXPR : N_SIMPLE_ARITHLOGIC
                {
				printRule("ARITHLOGIC_EXPR",
				          "SIMPLE_ARITHLOGIC");
				$$.type = $1.type;
                    	strcpy($$.strVal, $1.strVal);
				$$.intVal = $1.intVal;
				$$.floatVal = $1.floatVal;
				$$.boolVal = $1.boolVal;
				$$.nullVal = $1.nullVal;
				copyListInfo($$.listVal, $1.listVal);
				$$.opStillToDo = NOT_APPLICABLE;
                }
                | N_SIMPLE_ARITHLOGIC N_REL_OP
                  N_SIMPLE_ARITHLOGIC
                {
				printRule("ARITHLOGIC_EXPR",
				          "SIMPLE_ARITHLOGIC REL_OP "
				          "SIMPLE_ARITHLOGIC");
				if (isInvalidOperandType($1.type))
				  semanticError(1,
				    ERR_MUST_BE_INT_FLOAT_OR_BOOL);
				if (isInvalidOperandType($3.type)) 
				  semanticError(2,
				    ERR_MUST_BE_INT_FLOAT_OR_BOOL);
				$$.type = BOOL;
				strcpy($$.strVal, "");
				$$.intVal = NOT_APPLICABLE ;
				$$.floatVal = NOT_APPLICABLE;
				$$.nullVal = NOT_APPLICABLE;
				float val1 = getValAsFloat($1);
				float val2 = getValAsFloat($3);
				$$.boolVal = compareValues(val1, val2, $2);
				$$.opStillToDo = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                ;

N_SIMPLE_ARITHLOGIC : N_TERM N_ADD_OP_LIST
                {
				printRule("SIMPLE_ARITHLOGIC",
				          "TERM ADD_OP_LIST");
				if ($2.type != NOT_APPLICABLE) 
				{
				  if (isInvalidOperandType($1.type))
				    semanticError(1,
				      ERR_MUST_BE_INT_FLOAT_OR_BOOL);
				  if (($2.type != NOT_APPLICABLE) &&
				      isInvalidOperandType($2.type))
				    semanticError(2,
				      ERR_MUST_BE_INT_FLOAT_OR_BOOL);
				}
				if (($2.type != NOT_APPLICABLE) &&
				    ($2.opStillToDo != NOT_APPLICABLE))	
				{
				  strcpy($$.strVal, "");
				  $$.nullVal = NOT_APPLICABLE;
				  $$.listVal = NULL;
				  if (operatorCategory($2.opStillToDo) == 
				      LOGICAL_OP)
				  {
				    $$.type = BOOL;
				    $$.intVal = NOT_APPLICABLE ;
				    $$.floatVal = NOT_APPLICABLE;
				    float val1 = getValAsFloat($1);
				    float val2 = getValAsFloat($2);
				    $$.boolVal = 
                           compareValues(val1, val2, 
				                   $2.opStillToDo);
				  }
				  else // ARITHMETIC_OP
				  {
				    $$.boolVal = NOT_APPLICABLE;
				    if (isIntCompatible($1.type) &&
				        isIntCompatible($2.type))
				    {
				      $$.type = INT;
				      $$.floatVal = NOT_APPLICABLE;
				      int val1 = getValAsInt($1);
				      int val2 = getValAsInt($2);
				      $$.intVal =
					  calcValues(val1, val2,
				                  $2.opStillToDo);
				    }
				    else
				    { 
				      $$.type = FLOAT;
				      $$.intVal = NOT_APPLICABLE;
				      float val1 = getValAsFloat($1);
				      float val2 = getValAsFloat($2);
				      $$.floatVal =
					  calcValues(val1, val2,
				                  $2.opStillToDo);
				    }
				  }
				}
				else 
				{
				  $$.type = $1.type;
                    	  strcpy($$.strVal, $1.strVal);
				  $$.intVal = $1.intVal;
				  $$.floatVal = $1.floatVal;
				  $$.boolVal = $1.boolVal;
				  $$.nullVal = $1.nullVal;
				  copyListInfo($$.listVal, $1.listVal);
				  $$.opStillToDo = NOT_APPLICABLE;
				}
                }
                ;

N_ADD_OP_LIST	: N_ADD_OP N_TERM N_ADD_OP_LIST
                {
				printRule("ADD_OP_LIST",
				          "ADD_OP TERM ADD_OP_LIST");
				$$.opStillToDo = $1;
				int argWithErr =
				 ($3.type == NOT_APPLICABLE)? 2: 1;
				if (isInvalidOperandType($2.type))                                          				  semanticError(argWithErr,
				    ERR_MUST_BE_INT_FLOAT_OR_BOOL);
				strcpy($$.strVal, "");
				$$.nullVal = NOT_APPLICABLE;
				$$.listVal = NULL;
				if (operatorCategory($1) == LOGICAL_OP)
				{
				  $$.type = BOOL;
				  $$.intVal = NOT_APPLICABLE ;
				  $$.floatVal = NOT_APPLICABLE;
				  if ($3.type == NOT_APPLICABLE)
				    $$.boolVal = $2.boolVal;
				  else
				  {
				    int opCode = $3.opStillToDo;
				    float val1 = getValAsFloat($2);
				    float val2 = getValAsFloat($3);
				    $$.boolVal = 
                           compareValues(val1, val2, opCode);
				  }
				}
				else // ARITHMETIC_OP
				{
				  if ($3.type == NOT_APPLICABLE)
				  {
				    $$.type = $2.type;
				    $$.intVal = $2.intVal;
				    $$.floatVal = $2.floatVal;
				    $$.boolVal = $2.boolVal;
				  }
				  else
				  {
				    $$.boolVal = NOT_APPLICABLE;
				    int opCode = $3.opStillToDo;
				    if (isIntCompatible($2.type) &&
				        isIntCompatible($3.type))
				    {
				      $$.type = INT;
				      $$.floatVal = NOT_APPLICABLE;
				      int val1 = getValAsInt($2);
				      int val2 = getValAsInt($3);
				      $$.intVal =
					  calcValues(val1, val2, opCode);
				    }
				    else
				    { 
				      $$.type = FLOAT;
				      $$.intVal = NOT_APPLICABLE;
				      float val1 = getValAsFloat($2);
				      float val2 = getValAsFloat($3);
				      $$.floatVal =
					  calcValues(val1, val2, opCode);
				    }
				  }
				}
                }
                | /* epsilon */
                {
				printRule("ADD_OP_LIST", "epsilon");
				$$.type = NOT_APPLICABLE;
				strcpy($$.strVal, "");
				$$.intVal = NOT_APPLICABLE ;
				$$.floatVal = NOT_APPLICABLE;
				$$.boolVal = NOT_APPLICABLE;
				$$.nullVal = NOT_APPLICABLE;
				$$.opStillToDo = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                ;

N_TERM		: N_FACTOR N_MULT_OP_LIST
                {
				printRule("TERM",
				          "FACTOR MULT_OP_LIST");
				$$.type = $1.type;
                    	strcpy($$.strVal, $1.strVal);
				$$.intVal = $1.intVal;
				$$.floatVal = $1.floatVal;
				$$.boolVal = $1.boolVal;
				$$.nullVal = $1.nullVal;
				copyListInfo($$.listVal, $1.listVal);
				$$.opStillToDo = NOT_APPLICABLE;
				if ($2.type != NOT_APPLICABLE)
				{
				  if (isInvalidOperandType($1.type))
				    semanticError(1,
				     ERR_MUST_BE_INT_FLOAT_OR_BOOL);
				  if (isInvalidOperandType($2.type))
				    semanticError(2,
				      ERR_MUST_BE_INT_FLOAT_OR_BOOL);
				  if (isBoolCompatible($1.type) &&
				      isBoolCompatible($2.type))
				    $$.type = BOOL;
				  else
				  {
				    if (isIntCompatible($1.type) &&
				        isIntCompatible($2.type))
				      $$.type = INT;
				    else $$.type = FLOAT;
				  }

				  if ($2.opStillToDo != NOT_APPLICABLE)	
				  {
				    $$.listVal = NULL;
				    if (operatorCategory($2.opStillToDo) == 
				        LOGICAL_OP)
				    {
				      $$.type = BOOL;
				      $$.intVal = NOT_APPLICABLE ;
				      $$.floatVal = NOT_APPLICABLE;
				      float val1 = getValAsFloat($1);
				      float val2 = getValAsFloat($2);
				      $$.boolVal = 
                             compareValues(val1, val2, 
				                     $2.opStillToDo);
				    }
				    else // ARITHMETIC_OP
				    {
				      $$.boolVal = NOT_APPLICABLE;
				      if (isIntCompatible($1.type) &&
				          isIntCompatible($2.type))
				      {
				        $$.type = INT;
				        $$.floatVal = NOT_APPLICABLE;
				        int val1 = getValAsInt($1);
				        int val2 = getValAsInt($2);
				        $$.intVal =
					    calcValues(val1, val2,
				                    $2.opStillToDo);
				      }
				      else
				      { 
				        $$.type = FLOAT;
				        $$.intVal = NOT_APPLICABLE;
				        float val1 = getValAsFloat($1);
				        float val2 = getValAsFloat($2);
				        $$.floatVal =
					    calcValues(val1, val2,
				                    $2.opStillToDo);
				      }
				    }
				  }
				}
                }
                ;

N_MULT_OP_LIST	: N_MULT_OP N_FACTOR N_MULT_OP_LIST
                {
				printRule("MULT_OP_LIST",
				          "MULT_OP FACTOR MULT_OP_LIST");
				$$.opStillToDo = $1;
				int argWithErr = 
				  ($3.type == NOT_APPLICABLE)? 2: 1;
				if (isInvalidOperandType($2.type))                    				  semanticError(argWithErr,
				   ERR_MUST_BE_INT_FLOAT_OR_BOOL);
				strcpy($$.strVal, "");
				$$.nullVal = NOT_APPLICABLE;
				$$.type = $2.type;
				$$.intVal = $2.intVal;
				$$.floatVal = $2.floatVal;
				$$.boolVal = $2.boolVal;
				$$.listVal = NULL;
				if ($3.type != NOT_APPLICABLE)
				{
				  if (operatorCategory($1) == LOGICAL_OP)
				  {
				    int opCode = $3.opStillToDo;
				    $$.type = BOOL;
				    $$.intVal = NOT_APPLICABLE ;
				    $$.floatVal = NOT_APPLICABLE;
				    float val1 = getValAsFloat($2);
				    float val2 = getValAsFloat($3);
				    $$.boolVal = 
                             compareValues(val1, val2, opCode);
				  }
				  else // ARITHMETIC_OP
				  {
				    $$.boolVal = NOT_APPLICABLE;
				    int opCode = $3.opStillToDo;
				    if (isIntCompatible($2.type) &&
				        isIntCompatible($3.type))
				    {
				      $$.type = INT;
				      $$.floatVal = NOT_APPLICABLE;
				      int val1 = getValAsInt($2);
				      int val2 = getValAsInt($3);
				      $$.intVal =
					  calcValues(val1, val2, opCode);
				    }
				    else
				    { 
				      $$.type = FLOAT;
				      $$.intVal = NOT_APPLICABLE;
				      float val1 = getValAsFloat($2);
				      float val2 = getValAsFloat($3);
				      $$.floatVal =
					  calcValues($2.floatVal, 
					             $3.floatVal, opCode);
				    }
				  }
				}
                }
                | /* epsilon */
                {
				printRule("MULT_OP_LIST", "epsilon");
				$$.type = NOT_APPLICABLE;
				strcpy($$.strVal, "");
				$$.intVal = NOT_APPLICABLE ;
				$$.floatVal = NOT_APPLICABLE;
				$$.boolVal = NOT_APPLICABLE;
				$$.nullVal = NOT_APPLICABLE;
				$$.opStillToDo = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                ;

N_FACTOR		: N_VAR
                {
				printRule("FACTOR", "VAR");
				$$.type = $1.type;
                    	strcpy($$.strVal, $1.strVal);
				$$.intVal = $1.intVal;
				$$.floatVal = $1.floatVal;
				$$.boolVal = $1.boolVal;
				$$.nullVal = $1.nullVal;
				copyListInfo($$.listVal, $1.listVal);
				$$.opStillToDo = NOT_APPLICABLE;
                }
                | N_CONST
                {
				printRule("FACTOR", "CONST");
				$$.type = $1.type;
                    	strcpy($$.strVal, $1.strVal);
				$$.intVal = $1.intVal;
				$$.floatVal = $1.floatVal;
				$$.boolVal = $1.boolVal;
				$$.nullVal = $1.nullVal;
				$$.opStillToDo = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                | T_LPAREN N_EXPR T_RPAREN
                {
				printRule("FACTOR", "( EXPR )");
				$$.type = $2.type;
                    	strcpy($$.strVal, $2.strVal);
				$$.intVal = $2.intVal;
				$$.floatVal = $2.floatVal;
				$$.boolVal = $2.boolVal;
				$$.nullVal = $2.nullVal;
				$$.opStillToDo = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                | T_NOT N_FACTOR
                {
				printRule("FACTOR", "! FACTOR");
				$$.type = BOOL;
                    	strcpy($$.strVal, "");
				$$.intVal = NOT_APPLICABLE;
				$$.floatVal = NOT_APPLICABLE;
				float f = getValAsFloat($2);
				$$.boolVal = ! f;
				$$.nullVal = NOT_APPLICABLE;
				$$.opStillToDo = NOT_APPLICABLE;
				$$.listVal = NULL;
                }
                ;

N_ADD_OP	     : T_ADD
                {
				printRule("ADD_OP", "+");
				$$ = ADD;
                }
                | T_SUB
                {
				printRule("ADD_OP", "-");
				$$ = SUB;
                }
                | T_OR
                {
				printRule("ADD_OP", "|");
				$$ = OR;
                }
                ;

N_MULT_OP      : T_MULT
                {
				printRule("MULT_OP", "*");
				$$ = MULT;
                }
                | T_DIV
                {
				printRule("MULT_OP", "/");
				$$ = DIV;
                }
                | T_AND
                {
				printRule("MULT_OP", "&");
				$$ = AND;
                }
                | T_MOD
                {
				printRule("MULT_OP", "\%\%");
				$$ = MOD;
                }
                | T_POW
                {
				printRule("MULT_OP", "^");
				$$ = POW;
                }
                ;

N_REL_OP        : T_LT
                {
				printRule("REL_OP", "<");
				$$ = LT;
                }
                | T_GT
                {
				printRule("REL_OP", ">");
				$$ = GT;
                }
                | T_LE
                {
				printRule("REL_OP", "<=");
				$$ = LE;
                }
                | T_GE
                {
				printRule("REL_OP", ">=");
				$$ = GE;
                }
                | T_EQ
                {
				printRule("REL_OP", "==");
				$$ = EQ;
                }
                | T_NE
                {
				printRule("REL_OP", "!=");
				$$ = NE;
                }
                ;

N_VAR           : N_ENTIRE_VAR
                {
				printRule("VAR", "ENTIRE_VAR");
				$$.type == $1.type;
                    	strcpy($$.strVal, $1.strVal);
				$$.intVal = $1.intVal;
				$$.floatVal = $1.floatVal;
				$$.boolVal = $1.boolVal;
				$$.nullVal = $1.nullVal;
				copyListInfo($$.listVal, $1.listVal);
                }
                | N_SINGLE_ELEMENT
                {
				printRule("VAR", "SINGLE_ELEMENT");
				$$.type == $1.type;
                    	strcpy($$.strVal, $1.strVal);
				$$.intVal = $1.intVal;
				$$.floatVal = $1.floatVal;
				$$.boolVal = $1.boolVal;
				$$.nullVal = $1.nullVal;
				$1.listVal = NULL;
                }
                ;

N_SINGLE_ELEMENT : T_IDENT T_LBRACKET T_LBRACKET N_EXPR
                   T_RBRACKET T_RBRACKET
                {
				printRule("SINGLE_ELEMENT", "IDENT"
				          " [[ EXPR ]]");
				TYPE_INFO exprTypeInfo =
				  findEntryInAnyScope($1);
				if(exprTypeInfo.type == UNDEFINED) 
				  semanticError(0, ERR_UNDEFINED_IDENT);
				if(!isListCompatible(exprTypeInfo.type)) 
				  semanticError(1, ERR_MUST_BE_LIST);  
				$$.nullVal = NOT_APPLICABLE;
				$$.listVal = NULL;
				int index = getValAsInt($4);
				LIST_ENTRY entry =
				  getNthEntry(exprTypeInfo.listVal, 
				              index);
				$$.type = entry.type;
				strcpy($$.strVal, entry.strVal);
				$$.intVal = entry.intVal;
				$$.floatVal = entry.floatVal;
				$$.boolVal = entry.boolVal;
                }
                ;

N_ENTIRE_VAR    : T_IDENT
                {
				printRule("ENTIRE_VAR", "IDENT");
				TYPE_INFO exprTypeInfo = 
				  findEntryInAnyScope(string($1));
				if(exprTypeInfo.type == UNDEFINED)
				  semanticError(0, ERR_UNDEFINED_IDENT);
				$$.type = exprTypeInfo.type;
                    	strcpy($$.strVal, exprTypeInfo.strVal);
				$$.intVal = exprTypeInfo.intVal;
				$$.floatVal = exprTypeInfo.floatVal;
				$$.boolVal = exprTypeInfo.boolVal;
				$$.nullVal = exprTypeInfo.nullVal;
				copyListInfo($$.listVal,
			                  exprTypeInfo.listVal);
                }
                ;

%%

#include "lex.yy.c"
extern FILE *yyin;

//  Construct a string as an argument number (argNum, 0
//  if no argument number in message) and message (errNum is
//  index position in ERR_MSG[]). Then call yyerror with that
//  string.
void semanticError(const int argNum, const int errNum)
{
  string errorMsg;
  int errNo = errNum;

  if ((errNum < 0) || (errNum > NUM_ERR_MESSAGES-1))
    errNo = ERR_ERROR;
  if (argNum > 0)
    errorMsg = "Arg " + to_string(argNum) + " ";
  else errorMsg = "";
  errorMsg += ERR_MSG[errNo];
  yyerror(errorMsg.c_str());
}

// Output type and lexeme.
void printTokenInfo(const char* token_type, const char* lexeme)
{
  if(!suppressTokenOutput) 
    printf("TOKEN: %s \t\t LEXEME: %s\n", token_type, lexeme);
}

// Output production info as nonterm on left-hand side (1st
// param) and symbols on right-hand side (2nd param).
void printRule(const char *lhs, const char *rhs)
{
  if(!suppressTokenOutput) 
    printf("%s -> %s\n", lhs, rhs);
}

// Determine whether given type is compatible with INT, FLOAT,
// or BOOL.
bool isIntOrFloatOrBoolCompatible(const int theType)
{
  return(isIntCompatible(theType) ||
         isFloatCompatible(theType) ||
         isBoolCompatible(theType)); 
}

// Determine whether given type is compatible with INT.
bool isIntCompatible(const int theType)
{
  return(isBoolCompatible(theType) ||
         ((theType & INT) == INT));
}

// Determine whether given type is compatible with BOOL.
bool isBoolCompatible(const int theType)
{
  return((theType & BOOL) == BOOL);
}

// Determine whether given type is compatible with FLOAT.
bool isFloatCompatible(const int theType)
{
  return((theType & FLOAT) == FLOAT);
}

// Determine whether given type is compatible with LIST.
bool isListCompatible(const int theType)
{
  return((theType & LIST) == LIST);
}

// Determine whether given type is considered an invalid
// operand type.
bool isInvalidOperandType(const int theType)
{
  return((theType == NULL_TYPE) ||
         (theType == LIST) ||
         (theType == STR));
}

// Push a new SYMBOL_TABLE onto scopeStack.
void beginScope() 
{
  scopeStack.push(SYMBOL_TABLE());
  if(!suppressTokenOutput)
     printf("\n___Entering new scope...\n\n");
}

// Pop a SYMBOL_TABLE from scopeStack.
void endScope() 
{
  scopeStack.pop();
  if(!suppressTokenOutput)
    printf("\n___Exiting scope...\n\n");
}

// Pop all SYMBOL_TABLE's from scopeStack.
void cleanUp() 
{
  if (scopeStack.empty())
    return;
  else 
  {
    scopeStack.pop();
    cleanUp();
  }
}

// If the_name exists in any SYMBOL_TABLE in scopeStack, return
// its TYPE_INFO; otherwise, return a TYPE_INFO that contains
// type UNDEFINED.
TYPE_INFO findEntryInAnyScope(const string the_name) 
{
  TYPE_INFO info;
  info.type = UNDEFINED;
  if (scopeStack.empty()) return(info);
  info = scopeStack.top().findEntry(the_name);
  if (info.type != UNDEFINED)
    return(info);
  else 
  { // check in "next higher" scope
    SYMBOL_TABLE symbolTable = scopeStack.top();
    scopeStack.pop();
    info = findEntryInAnyScope(the_name);
    scopeStack.push(symbolTable); // restore the stack
    return(info);
  }
}

// Given an operator code (e.g., ADD, LT, etc.), return the
// "category" code (e.g., ARITHMETIC_OP, etc.) to which it
// belongs.
int operatorCategory(const int opCode) 
{
  if (opCode < 10)
    return(ARITHMETIC_OP);
  else if (opCode < 100)
         return(RELATIONAL_OP);
       else if (opCode < 1000)
              return(LOGICAL_OP);
            else return(UNARY_OP);
}

// Given a relational or logical operator and 2 (float) 
// operand values, return the boolean result.
bool compareValues(const float val1, const float val2,
                   const int opCode) 
{
  switch (opCode) 
  {
    case LT 	: return(val1 < val2);
    case GT	: return(val1 > val2);
    case LE	: return(val1 <= val2);
    case GE	: return(val1 >= val2);
    case EQ	: return(val1 == val2);
    case NE	: return(val1 != val2);
    case AND	: return(val1 && val2);
    case OR	: return(val1 || val2);
    default	: cout << "Invalid operator " << opCode
                       << " in compareValues!\n";
			  exit(1);
  }
}

// Given an arithmetic operator and 2 (float) 
// operand values, return the float result.
float calcValues(const float val1, const float val2,
                 const int opCode) 
{
  switch (opCode) 
  {
    case ADD	: return(val1 + val2);
    case SUB	: return(val1 - val2);
    case MULT	: return(val1 * val2);
    case DIV	: if (val2 == 0)
			    semanticError(0, ERR_DIVISION_BY_ZERO);
			  else return(val1 / val2);
    case MOD	: return(fmod(val1, val2));
    case POW	: return(pow(val1, val2));
    default	: cout << "Invalid operator " << opCode
                       << " in calcValues!\n";
			  exit(1);
  }
}

// Given an arithmetic operator and 2 (int) 
// operand values, return the int result.
int calcValues(const int val1, const int val2, 
               const int opCode) 
{
  switch (opCode) 
  {
    case ADD	: return(val1 + val2);
    case SUB	: return(val1 - val2);
    case MULT	: return(val1 * val2);
    case DIV	: if (val2 == 0)
			    semanticError(0, ERR_DIVISION_BY_ZERO);
			  else return(val1 / val2);
    case MOD	: return(val1 % val2);
    case POW	: return(static_cast<int>(pow(val1, val2)));
    default	: cout << "Invalid operator " << opCode
                       << " in calcValues!\n";
			  exit(1);
  }
}

// Given a TYPE_INFO struct, return the value being used
// (which should be INT, FLOAT, or BOOL) as a float.
float getValAsFloat(const TYPE_INFO x)
{
  float f;
  switch (x.type)
  {
    case INT	: f = x.intVal; break;
    case FLOAT	: f = x.floatVal; break;
    case BOOL	: f = x.boolVal; break;
    default	: cout << "Invalid operand type " << x.type 
                       << " in getValAsFloat!\n";
			  exit(1);
  }
  return(f);
}

// Given a TYPE_INFO struct, return the value being used
// (which here should be INT or BOOL) as an INT.
int getValAsInt(const TYPE_INFO x)
{
  int i;
  switch (x.type)
  {
    case INT	: i = x.intVal; break;
    case BOOL	: i = x.boolVal; break;
    default	: cout << "Invalid operand type " << x.type 
                       << " in getValAsInt!\n";
			  exit(1);
  }
  return(i);
}

// Make the target LIST_INFO have the same contents as the
// src LIST_INFO.
void copyListInfo(LIST_INFO_PTR &target, LIST_INFO_PTR src)
{
  if (src == NULL)
  {
    target = NULL;
    return;
  }
/*
// 	FOR SOME REASON, THIS KILLS SRC LIST!!!
  if (target != NULL)
  {
//cout << "Target is being erased...\n";
    std::list<LIST_ENTRY>::iterator it1 = (*target).begin();
    std::list<LIST_ENTRY>::iterator it2 = (*target).end();
    (*target).erase(it1, it2);
  }
*/
  target = new list<LIST_ENTRY>;
  for (std::list<LIST_ENTRY>::iterator it = (*src).begin();
       it != (*src).end(); it++)
    target->push_back((*it));
}

// Return the (1-based) nth entry in the given list.
LIST_ENTRY getNthEntry(LIST_INFO_PTR x, const int n)
{
  int count = 1;
  LIST_ENTRY entry;

  if ((n < 1) || (n > (*x).size()))
    semanticError(0, ERR_SUBSCRIPT_OUT_OF_BOUNDS);

  for (std::list<LIST_ENTRY>::iterator it = (*x).begin();
       it != (*x).end(); it++)
  {
    if (count == n)
    {
      entry.type =(*it).type;
      strcpy(entry.strVal, (*it).strVal);
      entry.intVal = (*it).intVal; 
      entry.floatVal = (*it).floatVal;
      entry.boolVal = (*it).boolVal; 
      return(entry);
    }
    count++;
  }
}

// Change the (1-based) nth value in list x to be the value in
// the specified TYPE_INFO struct.
void changeListInfo(LIST_INFO_PTR &x, const int n, 
                    const TYPE_INFO info)
{
  int count = 1;
  LIST_ENTRY entry;

  if ((n < 1) || (n > (*x).size()))
    semanticError(0, ERR_SUBSCRIPT_OUT_OF_BOUNDS);

  for (std::list<LIST_ENTRY>::iterator it = (*x).begin();
       it != (*x).end(); it++)
  {
    if (count == n)
    {
      (*it).type = info.type;
      strcpy((*it).strVal, info.strVal);
      (*it).intVal = info.intVal; 
      (*it).floatVal = info.floatVal;
      (*it).boolVal = info.boolVal; 
      return;
    }
    count++;
  }
}

void printList(LIST_INFO_PTR x)
{
  if (x == NULL)
  {
    printf("( )\n");
    return;
  }

  printf("( ");
  for (std::list<LIST_ENTRY>::iterator it = (*x).begin();
       it != (*x).end(); it++)
  {
    switch ((*it).type) 
    {
	case (INT) :
	  printf("%d ", (*it).intVal); break;
	case (FLOAT) :
	  printf("%6.2f ", (*it).floatVal); break;
	case (STR) :
	  printf("%s ", (*it).strVal); break;
	case (BOOL) :
	  if ((*it).boolVal)
	    printf("TRUE ");
	  else printf("FALSE ");
	  break;
	default:  // should never happen!
	  printf("***INVALID TYPE %d\n", (*it).type);
    } // end switch
  }
  printf(")\n");
}

int main(int argc, char** argv) 
{
    if(argc < 2) 
    {
        printf("You must specify a file in the command line!\n");
        exit(1);
    }
    yyin = fopen(argv[1], "r");
    beginScope();
    do {
        yyparse();
    } while (!feof(yyin));
    endScope();

    return 0;
}
