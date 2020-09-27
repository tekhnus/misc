%{
    #include "expression.hpp"
    #include "pascal.hpp"

    extern "C" int yywrap() {}
%}

%%

"{"(\\.|[^"}"])*"}"     /* comment */
"\""(\\.|[^"\""])*"\""  yylval.str_value = new std::string(yytext, 1, strlen(yytext) - 2); return T_STR_LITERAL;

[ \t\n]                 /* whitespace */

"program"               return T_PROGRAM;
"var"                   return T_VAR;
"begin"                 return T_BEGIN;
"end"                   return T_END;
"function"              return T_FUNCTION;
"array"                 return T_ARRAY;
"of"                    return T_OF;

"if"                    return T_IF;
"then"                  return T_THEN;
"else"                  return T_ELSE;
"while"                 return T_WHILE;
"do"                    return T_DO;
"for"                   return T_FOR;

"true"                  return T_TRUE;
"false"                 return T_FALSE;
"not"                   return T_NOT;

":="                    return T_ASSIGN;
"+"                     return T_PLUS;
"-"                     return T_MINUS;
"*"                     return T_STAR;
"/"                     return T_SLASH;
"%"                     return T_MOD;
"("                     return T_OPEN;
")"                     return T_CLOSE;
";"                     return T_SEMICOL;
":"                     return T_COL;
","                     return T_COMMA;
"."                     return T_DOT;
"="                     return T_EQ;
">="                    return T_GR_EQ;
"<="                    return T_LE_EQ;
">"                     return T_GREATER;
"<"                     return T_LESS;
"["                     return T_LEFT_SQ;
"]"                     return T_RIGHT_SQ;

[a-zA-Z_][a-zA-Z_]*       { yylval.str_value = new std::string(yytext); return T_IDENTIFIER; }
[0-9]+                  { yylval.int_value = std::stoi(yytext); return T_INTEGER; }

.                       fprintf(stderr, "Unknow token: %s\n", yytext); yyterminate();

%%