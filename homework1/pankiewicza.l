/* pankiewicza.l
    
    flex pankiewicza.l 
    g++ lex.yy.c -o lexer
    lexer < testInput
*/

%{
#define T_IDENT         1
#define T_INTCONST      2
#define T_FLOATCONST    3  
#define T_UNKNOWN       4
#define T_STRCONST      5
#define T_IF            6
#define T_ELSE          7
#define T_WHILE         8
#define T_FUNCTION      9
#define T_FOR           10
#define T_IN            11
#define T_NEXT          12
#define T_BREAK         13
#define T_TRUE          14
#define T_FALSE         15
#define T_QUIT          16
#define T_PRINT         17
#define T_CAT           18
#define T_READ          19
#define T_LPAREN        20
#define T_RPAREN        21
#define T_LBRACE        22
#define T_RBRACE        23
#define T_LBRACKET      24
#define T_RBRACKET      25
#define T_COLON         26
#define T_SEMICOLON     27
#define T_COMMA         28
#define T_ADD           29
#define T_SUB           30
#define T_MULT          31
#define T_DIV           32
#define T_MOD           33
#define T_POWER         34
#define T_LT            35
#define T_LE            36
#define T_GT            37
#define T_GE            38
#define T_EQ            39
#define T_NE            40
#define T_FACT          41
#define T_AND           42
#define T_OR            43
#define T_ASSIGN        44

int num_lines = 0;

void printTokenInfo(const char* token_type, const char* lexeme);

%}

WSPACE [ \t\v\r]+
NEWLINE \n
DIGIT [0-9]
LETTER [a-zA-Z]
IDENT ({LETTER}|\_)+({LETTER}|{DIGIT}|\_)*
FLOATCONST (\+|-)?(({DIGIT})*\.({DIGIT})+)
INTCONST (\+|-)?{DIGIT}+
STRCONST (\")({LETTER}|{DIGIT}|{WSPACE}|.)+(\")
LPAREN (\()
RPAREN (\))
LBRACE (\{)
RBRACE (\})
LBRACKET (\[)
RBRACKET (\])
ADD (\+)
SUB (\-)
MULT (\*)
DIV (\/)
MOD (\%%)
POWER (\^)
LT (\<)
LE (\<=)
GT (\>)
GE (\>=)
EQ (\==)
NE (\!=)
FACT (\!)
AND (\&)
OR (\|)
ASSIGN (\=)
COMMENT #+.+

%%

{EQ} {
    printTokenInfo("EQ", yytext);
    return T_EQ;
}

{GE} {
    printTokenInfo("GE", yytext);
    return T_GE;
}

{GT} {
    printTokenInfo("GT", yytext);
    return T_GT;
}

{LE} {
    printTokenInfo("LE", yytext);
    return T_LE;
}

{LT} {
    printTokenInfo("LT", yytext);
    return T_LT;
}

{SUB} {
    printTokenInfo("SUB", yytext);
    return T_SUB;
}

{DIV} {
    printTokenInfo("DIV", yytext);
    return T_DIV;
}

{NE} {
    printTokenInfo("NE", yytext);
    return T_NE;
}

{MULT} {
    printTokenInfo("MULT", yytext);
    return T_MULT;
}

{ADD} {
    printTokenInfo("ADD", yytext);
    return T_ADD;
}

{RPAREN} {
    printTokenInfo("RPAREN", yytext);
    return T_RPAREN;
}

{LPAREN} { 
    printTokenInfo("LPAREN", yytext);
    return T_LPAREN;
}

{LBRACE} {
    printTokenInfo("LBRACE", yytext);
    return T_LBRACE;
}

{RBRACE} {
    printTokenInfo("RBRACE", yytext);
    return T_RBRACE;
}

{LBRACKET} {
    printTokenInfo("LBRACKET", yytext);
    return T_LBRACKET;
}

{RBRACKET} {
    printTokenInfo("RBRACKET", yytext);
    return T_RBRACKET;
}

"TRUE" {
    printTokenInfo("TRUE", yytext);
    return T_TRUE;
}

"FALSE" {
    printTokenInfo("FALSE", yytext);
    return T_FALSE;
}

"if" {
    printTokenInfo("IF", yytext);
    return T_IF;
}

"else" {
    printTokenInfo("ELSE", yytext);
    return T_ELSE;
}

"while" {
    printTokenInfo("WHILE", yytext);
    return T_WHILE;
}

"function" {
    printTokenInfo("FUNCTION", yytext);
    return T_FUNCTION;
}

"for" {
    printTokenInfo("FOR", yytext);
    return T_FOR;
}

"in" {
    printTokenInfo("IN", yytext);
    return T_IN;
}

"next" {
    printTokenInfo("NEXT", yytext);
    return T_NEXT;
}

"break" {
    printTokenInfo("BREAK", yytext);
    return T_BREAK;
}

"quit" {
    printTokenInfo("QUIT", yytext);
    return T_QUIT;
}

"print" {
    printTokenInfo("PRINT", yytext);
    return T_PRINT;
}

"cat" {
    printTokenInfo("CAT", yytext);
    return T_CAT;
}

"read" {
    printTokenInfo("READ", yytext);
    return T_READ;
}

{STRCONST} {
    printTokenInfo("STRCONST", yytext);
    return T_STRCONST;
}

{INTCONST} {
    printTokenInfo("INTCONST", yytext);
    return T_INTCONST;
}

{FLOATCONST} {
    printTokenInfo("FLOATCONST", yytext);
    return T_FLOATCONST;
}

{IDENT} {
    printTokenInfo("IDENT", yytext);
    return T_IDENT;
}

{NEWLINE} {
    num_lines++;
}

{WSPACE} {}

{COMMENT} {}

. {
    printTokenInfo("UNKNOWN", yytext);
    return T_UNKNOWN;
}

%%

void printTokenInfo(const char* token_type, const char* lexeme)
{
    printf("TOKEN: %s \t\t\t LEXEME: %s\n", token_type, lexeme);
}

int yywrap() { return(1); }

int main()
{
    while ( yylex() );

    printf("Processed %d lines\n", num_lines);
    return(0);
}