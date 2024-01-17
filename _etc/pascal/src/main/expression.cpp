#include "expression.hpp"

expression::~expression()
{}

const_expression::const_expression(std::shared_ptr<type> value) : value(value)
{}

std::shared_ptr<type> const_expression::eval()
{
    return value;
}

dynamic_expression::dynamic_expression(const std::string& id, context_manager& ctxt) :
id(id), ctxt(ctxt)
{}

std::shared_ptr<type> dynamic_expression::eval()
{
    try
    {
        std::shared_ptr<type> expr = ctxt.get_global().get(id);
        return expr->invoke(std::list<std::shared_ptr<type>>());
    }
    catch (std::exception e)
    {
        return ctxt.get_local().get(id);
    }
}

binary_expression::binary_expression(expression& left,
                                     char op,
                                     expression& right) :
    left(left), op(op), right(right)
{}

std::shared_ptr<type> binary_expression::eval()
{
    switch (op)
    {
        case '+':
            return *left.eval() + *right.eval();
        case '-':
            return *left.eval() - *right.eval();
        case '*':
            return *left.eval() * *right.eval();
        case '/':
            return *left.eval() / *right.eval();
        case '%':
            return *left.eval() % *right.eval();
        case '=':
            return *left.eval() == *right.eval();
        case 'a':
            return left.eval()->assign(*right.eval());
        case '>':
            return *left.eval() > *right.eval();
        case '<':
            return *left.eval() < *right.eval();
        case '[':
            return left.eval()->at(*right.eval());
        case 'g':
            return *left.eval() >= *right.eval();
        case 'l':
            return *left.eval() <= *right.eval();
        default:
            throw std::invalid_argument("Unimplemented operator: " + std::string(&op, 1));
    }
}

expression& binary_expression::get_left()
{
    return left;
}

expression& binary_expression::get_right()
{
    return right;
}


unary_expression::unary_expression(char op, expression& expr) :
    op(op), expr(expr)
{}

std::shared_ptr<type> unary_expression::eval()
{
    switch(op)
    {
        case '~':
            return ~*expr.eval();
        default:
            throw std::invalid_argument("Unimplemented operator: " + std::string(&op, 1));
    }
}

void expression_list::push_back(expression& next)
{
    list.push_back(std::shared_ptr<expression>(&next));
}

void expression_list::push_all(expression_list& next_list)
{
    list.merge(next_list.list);
}

std::shared_ptr<type> expression_list::eval()
{
    std::shared_ptr<type> ret;
    for (auto& next : list)
        ret = next->eval();
    if (ret != nullptr)
        return ret;
    else
        return std::shared_ptr<type>(new void_type());
}

const std::list<std::shared_ptr<expression>>& expression_list::get_list()
{
    return list;
}

if_expression::if_expression(expression& condition,
                             expression& body,
                             expression& otherwise) :
    condition(condition), body(body), otherwise(otherwise)
{}

std::shared_ptr<type> if_expression::eval()
{
    if (condition.eval()->to_bool())
        return body.eval();
    else
        return otherwise.eval();
}

while_expression::while_expression(expression& condition,
                                   expression& body) :
    condition(condition), body(body)
{}

std::shared_ptr<type> while_expression::eval()
{
    while (condition.eval()->to_bool())
        body.eval();
    return std::shared_ptr<type>(new void_type());
}

for_expression::for_expression(expression& init,
               expression& condition,
               expression& iter,
               expression& body) :
    init(init), condition(condition), iter(iter), body(body)
{}

std::shared_ptr<type> for_expression::eval()
{
    for (init.eval(); condition.eval()->to_bool(); iter.eval())
    {
        body.eval();
    }
    return std::shared_ptr<type>(new void_type());
}

var_declare_expression::var_declare_expression(const std::string& name,
                                               expression& type_decl,
                                               context_manager& ctxt) :
    name(name), type_decl(type_decl), ctxt(ctxt)
{}

std::shared_ptr<type> var_declare_expression::eval()
{
    ctxt.get_local().declare(name, type_decl.eval());
    return std::shared_ptr<type>(new void_type());
}

const std::string& var_declare_expression::get_name()
{
    return name;    
}

function_invoke_expression::function_invoke_expression(const std::string& id,
                                                       expression_list& args,
                                                       context_manager& ctxt) :
    id(id), args(args), ctxt(ctxt)
{}

std::shared_ptr<type> function_invoke_expression::eval()
{
    std::list<std::shared_ptr<type>> arg_values;
    for (const auto& arg : args.get_list())
        arg_values.push_back(arg->eval());
    std::shared_ptr<type> func = ctxt.get_global().get(id);
    ctxt.put_local();
    std::shared_ptr<type> result = func->invoke(arg_values);
    ctxt.pop_local();
    return result;
}

primitive_type_expression::primitive_type_expression(const std::string& type_name) :
    type_name(type_name)
{}

std::shared_ptr<type> primitive_type_expression::eval()
{
    if (type_name == "integer")
        return std::shared_ptr<type>(new mutable_int_type());
    else if (type_name == "boolean")
        return std::shared_ptr<type>(new mutable_bool_type());
    else if (type_name == "string")
        return std::shared_ptr<type>(new mutable_string_type());
    else if (type_name == "void")
        return std::shared_ptr<type>(new mutable_void_type());
    else
        throw std::logic_error("Invalid type name: " + type_name);
}

array_type_expression::array_type_expression(expression& inner_type, int size) :
    inner_type(inner_type), size(size)
{}

std::shared_ptr<type> array_type_expression::eval()
{
    std::vector<std::shared_ptr<type>> array;
    for (int i = 0; i < size; ++i)
    {
        array.push_back(inner_type.eval());
    }
    return std::shared_ptr<type>(new array_type(std::move(array)));
}