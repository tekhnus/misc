#pragma once

#include "types.hpp"
#include "context.hpp"

#include <string>
#include <list>
#include <memory>
#include <iostream>
#include <stdexcept>

class expression
{
public:
    virtual ~expression();
    virtual std::shared_ptr<type> eval() = 0;
};

class const_expression : public expression
{
public:
    const_expression(std::shared_ptr<type> value);
    std::shared_ptr<type> eval();
private:
    std::shared_ptr<type> value;
};

class dynamic_expression : public expression
{
public:
    dynamic_expression(const std::string& id, context_manager& ctxt);
    std::shared_ptr<type> eval();
protected:
    std::string id;
    context_manager& ctxt;
};

class binary_expression : public expression
{
public:
    binary_expression(expression& left,
                      char op,
                      expression& right);
    std::shared_ptr<type> eval();
    expression& get_left();
    expression& get_right();
private:
    expression& left;
    char op;
    expression& right;
};

class unary_expression : public expression
{
public:
    unary_expression(char op,
                     expression& expr);
    std::shared_ptr<type> eval();
private:
    char op;
    expression& expr;
};

class expression_list : public expression
{
public:
    void push_back(expression& next);
    void push_all(expression_list& next_list);
    const std::list<std::shared_ptr<expression>>& get_list();
    std::shared_ptr<type> eval();
private:
    std::list<std::shared_ptr<expression>> list;
};

class if_expression : public expression
{
public:
    if_expression(expression& condition,
                  expression& body,
                  expression& otherwise = *(new const_expression(std::shared_ptr<type>(new void_type()))));
    std::shared_ptr<type> eval();
private:
    expression& condition;
    expression& body;
    expression& otherwise;
};

class while_expression : public expression
{
public:
    while_expression(expression& condition,
                     expression& body);
    std::shared_ptr<type> eval();
private:
    expression& condition;
    expression& body;
};

class for_expression : public expression
{
public:
    for_expression(expression& init,
                   expression& condition,
                   expression& iter,
                   expression& body);
    std::shared_ptr<type> eval();
private:
    expression& init;
    expression& condition;
    expression& iter;
    expression& body;
};

class var_declare_expression : public expression
{
public:
    var_declare_expression(const std::string& name,
                           expression& type_decl,
                           context_manager& ctxt);
    std::shared_ptr<type> eval();
    const std::string& get_name();
private:
    std::string name;
    expression& type_decl;
    context_manager& ctxt;
};

class function_invoke_expression : public expression
{
public:
    function_invoke_expression(const std::string& id,
                               expression_list& args,
                               context_manager& ctxt);
    std::shared_ptr<type> eval();
private:
    std::string id;
    expression_list& args;
    context_manager& ctxt;
};

class primitive_type_expression : public expression
{
public:
    primitive_type_expression(const std::string& type_name);
    std::shared_ptr<type> eval();
private:
    std::string type_name;
};

class array_type_expression : public expression
{
public:
    array_type_expression(expression& inner_type, int size);
    std::shared_ptr<type> eval();
private:
    expression& inner_type;
    int size;
};