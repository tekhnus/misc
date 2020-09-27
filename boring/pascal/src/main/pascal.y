%{
    #include "expression.hpp"
    #include "context.hpp"
    #include "stdpas.hpp"
    #include "pascal.hpp"

    #include <map>

    extern FILE* yyin;
    extern int yylex();
    void yyerror(const char *s) { std::cerr << "Grammar error: " << std::string(s) << std::endl; }

    context_manager ctxt;
%}

%union {
    std::string* str_value;
    std::list<std::string>* str_values;
    std::pair<std::list<std::string>, std::string>* declar;

    int int_value;

    expression* expr;
}

%token <str_value> T_IDENTIFIER
%token <int_value> T_INTEGER
%token <str_value> T_STR_LITERAL
%token T_PROGRAM T_VAR T_BEGIN T_END T_FUNCTION T_ARRAY T_OF
%token T_ASSIGN T_PLUS T_MINUS T_STAR T_SLASH T_MOD
%token T_OPEN T_CLOSE T_SEMICOL T_COL T_COMMA T_DOT T_EQ
%token T_GR_EQ T_LE_EQ T_GREATER T_LESS T_LEFT_SQ T_RIGHT_SQ
%token T_IF T_THEN T_ELSE T_WHILE T_DO T_FOR
%token T_TRUE T_FALSE T_NOT

%type <str_values> id_list optional_params literal_list

%type <expr> expression expression_37 expression_35 expression_30 expression_20 expression_10 expression_5 expression_3 simple_expression
%type <expr> expressions optional_expressions codeblock expression_or_block
%type <expr> comma_expressions optional_comma_expressions
%type <expr> closed_if_expression unclosed_if_expression while_expression for_expression
%type <expr> dynamic_expression explicit_function_invoke

%type <expr> vardecl varsection vars_and_code
%type <expr> type_expression
%type <expr> signature_entries optional_signature_entries

%%

program:
    header function_decls vars_and_code T_DOT
                                            { ctxt.put_local();
                                              $3->eval(); }

header:
    T_PROGRAM T_IDENTIFIER optional_params T_SEMICOL
                                            { if ($3->size() == 2)
                                              {
                                                freopen($3->front().c_str(), "r", stdin);
                                                freopen($3->back().c_str(), "w", stdout);                                                
                                              }
                                              else if ($3->size() != 0)
                                                  throw std::logic_error("Incorrect program params");
                                            }

function_decls:
    /* empty */
    | function_decls function_decl T_SEMICOL

function_decl:
    T_FUNCTION T_IDENTIFIER
    optional_signature_entries
    T_COL type_expression
    vars_and_code                           { ctxt.get_global().declare(*$2, std::shared_ptr<type>(new invokeable_type(*dynamic_cast<expression_list*>($3), *(new var_declare_expression(*$2, *$5, ctxt)), *$6, ctxt))); }

vars_and_code:
    varsection codeblock                    { $$ = $1;
                                              dynamic_cast<expression_list*>($$)->push_all(*dynamic_cast<expression_list*>($2)); }
                        
optional_signature_entries:
    /* empty */                             { $$ = new expression_list(); }
    | T_OPEN signature_entries T_CLOSE      { $$ = $2; }

signature_entries:
    vardecl                                 { expression_list* list = new expression_list();
                                              list->push_back(*$1);
                                              $$ = list;
                                            }
    | signature_entries T_COMMA vardecl     { dynamic_cast<expression_list*>($$)->push_back(*$3); }

varsection:
    /* empty */                             { $$ = new expression_list(); }
    | varsection T_VAR vardecl T_SEMICOL    { dynamic_cast<expression_list*>($$)->push_back(*$3); }     

vardecl:
    id_list T_COL type_expression           { expression_list* list = new expression_list();
                                              for (const auto& name : *$1)
                                              {
                                                  expression* decl = new var_declare_expression(name, *$3, ctxt);
                                                  list->push_back(*decl);
                                              }
                                              $$ = list;
                                            }

type_expression:
    T_IDENTIFIER                            { $$ = new primitive_type_expression(*$1); }
    | T_ARRAY T_INTEGER T_OF type_expression
                                            { $$ = new array_type_expression(*$4, $2); }

optional_params:
    /* empty */                             { $$ = new std::list<std::string>(); }
    | T_OPEN literal_list T_CLOSE           { $$ = $2; }

id_list:
    T_IDENTIFIER                            { $$ = new std::list<std::string>(); $$->push_back(*$1); }
    | id_list T_COMMA T_IDENTIFIER          { $1->push_back(*$3); }

literal_list:
    T_STR_LITERAL                           { $$ = new std::list<std::string>(); $$->push_back(*$1); }
    | literal_list T_COMMA T_STR_LITERAL    { $1->push_back(*$3); }

codeblock:
    T_BEGIN optional_expressions T_END      { $$ = $2; }

expression_or_block:
    expression
    | codeblock

optional_expressions:
    /* empty */                             { $$ = new expression_list(); }
    | expressions

expressions:
    expression                              { $$ = new expression_list();
                                              dynamic_cast<expression_list*>($$)->push_back(*$1); }
    | expressions T_SEMICOL expression      { dynamic_cast<expression_list*>($$)->push_back(*$3); }

optional_comma_expressions:
    /* empty */                             { $$ = new expression_list(); }
    | comma_expressions                     

comma_expressions:
    expression                              { $$ = new expression_list();
                                              dynamic_cast<expression_list*>($$)->push_back(*$1); }
    | comma_expressions T_COMMA expression  { dynamic_cast<expression_list*>($$)->push_back(*$3); }

expression:
    expression_37
    | while_expression
    | for_expression

expression_37:
    expression_35
    | unclosed_if_expression

expression_35:
    expression_30
    | closed_if_expression

expression_30:
    expression_20
    | expression_30 T_ASSIGN expression_20  { $$ = new binary_expression(*$1, 'a', *$3); }
    | expression_30 T_EQ expression_20      { $$ = new binary_expression(*$1, '=', *$3); }
    | expression_30 T_GREATER expression_20 { $$ = new binary_expression(*$1, '>', *$3); }
    | expression_30 T_LESS expression_20    { $$ = new binary_expression(*$1, '<', *$3); }
    | expression_30 T_GR_EQ expression_20   { $$ = new binary_expression(*$1, 'g', *$3); }
    | expression_30 T_LE_EQ expression_20   { $$ = new binary_expression(*$1, 'l', *$3); }

expression_20:
    expression_10
    | expression_20 T_PLUS expression_10    { $$ = new binary_expression(*$1, '+', *$3); }
    | expression_20 T_MINUS expression_10   { $$ = new binary_expression(*$1, '-', *$3); }

expression_10:
    expression_5
    | expression_10 T_STAR expression_5
                                            { $$ = new binary_expression(*$1, '*', *$3); }
    | expression_10 T_SLASH expression_5
                                            { $$ = new binary_expression(*$1, '/', *$3); }
    | expression_10 T_MOD expression_5
                                            { $$ = new binary_expression(*$1, '%', *$3); }

expression_5:
    expression_3
    | T_NOT expression_5                    { $$ = new unary_expression('~', *$2); }

expression_3:
    simple_expression
    | expression_3 T_LEFT_SQ expression T_RIGHT_SQ
                                            { $$ = new binary_expression(*$1, '[', *$3); }
                                            
simple_expression:
    T_INTEGER                               { $$ = new const_expression(std::shared_ptr<type>(new int_type($1))); }
    | T_TRUE                                { $$ = new const_expression(std::shared_ptr<type>(new bool_type(true))); }
    | T_FALSE                               { $$ = new const_expression(std::shared_ptr<type>(new bool_type(false))); }
    | T_STR_LITERAL                         { $$ = new const_expression(std::shared_ptr<type>(new string_type(*$1))); }
    | T_OPEN expression T_CLOSE             { $$ = $2; }
    | dynamic_expression
    | explicit_function_invoke

dynamic_expression:
    T_IDENTIFIER                            { $$ = new dynamic_expression(*$1, ctxt); }

explicit_function_invoke:
    T_IDENTIFIER T_OPEN optional_comma_expressions T_CLOSE
                                            { $$ = new function_invoke_expression(*$1, *dynamic_cast<expression_list*>($3), ctxt); }

unclosed_if_expression:
    T_IF T_BEGIN expression T_END T_THEN expression_or_block     
                                            { $$ = new if_expression(*$3, *$6); }
closed_if_expression:
    T_IF expression T_THEN expression_or_block
    T_ELSE expression_or_block              { $$ = new if_expression(*$2, *$4, *$6); }

while_expression:
    T_WHILE expression T_DO expression_or_block 
                                            { $$ = new while_expression(*$2, *$4); }

for_expression:
    T_FOR expression T_IDENTIFIER expression T_DO expression_or_block
                                            { binary_expression* init = dynamic_cast<binary_expression*>($2);
                                              if (init == nullptr)
                                              {
                                                throw std::logic_error("Invalid for loop initialization");
                                              }
                                              else
                                              {
                                                expression& counter = init->get_left();
                                                expression_list* counters = new expression_list();
                                                counters->push_back(counter);
                                                binary_expression* condition;
                                                expression* iter;
                                                if (*$3 == "to")
                                                {
                                                    condition = new binary_expression(counter, 'l', *$4);
                                                    iter = new function_invoke_expression("inc", *counters, ctxt);
                                                }
                                                else if (*$3 == "downto")   
                                                {   
                                                    condition = new binary_expression(counter, 'g', *$4);                                              
                                                    iter = new function_invoke_expression("dec", *counters, ctxt);
                                                }
                                                else
                                                    throw std::logic_error("Invalid for loop iteration");
                                                $$ = new for_expression(*init, *condition, *iter, *$6);
                                              }
                                            }

%%

int main(int argc, char** argv)
{
    try
    {
        if (argc < 2)
        {
            std::cerr   << "Interpreter exception: please provide input file" << std::endl;
            return 1;
        }
        FILE* source = fopen(argv[1], "r");
        yyin = source;
        stdpas::load_all(ctxt);
        yyparse();
        fclose(stdin);
        fclose(stdout);
        return 0;
    }
    catch(std::exception& ex)
    {
        std::cerr   << "Runtime exception:" << std::endl
                    << ex.what() << std::endl;
        return 1;
    }
}