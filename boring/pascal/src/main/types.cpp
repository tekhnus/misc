#include "types.hpp"

#include "expression.hpp"
#include "context.hpp"
#include <stdexcept>

std::shared_ptr<type> value_of(int value)
{
    return std::shared_ptr<type>(new int_type(value));
}

std::shared_ptr<type> value_of(bool value)
{
    return std::shared_ptr<type>(new bool_type(value));
}

std::shared_ptr<type> value_of(const std::string& value)
{
    return std::shared_ptr<type>(new string_type(value));
}

std::shared_ptr<type> type::operator+(type& another)
{
    throw std::logic_error("Operator + is not applicable");
}

std::shared_ptr<type> type::operator-(type& another)
{
    throw std::logic_error("Operator - is not applicable");
}

std::shared_ptr<type> type::operator*(type& another)
{
    throw std::logic_error("Operator * is not applicable");
}

std::shared_ptr<type> type::operator/(type& another)
{
    throw std::logic_error("Operator / is not applicable");
}

std::shared_ptr<type> type::operator%(type& another)
{
    throw std::logic_error("Operator % is not applicable");
}

std::shared_ptr<type> type::operator==(type& another)
{
    throw std::logic_error("Operator = is not applicable");
}

std::shared_ptr<type> type::operator>(type& another)
{
    throw std::logic_error("Operator > is not applicable");
}

std::shared_ptr<type> type::operator<(type& another)
{
    throw std::logic_error("Operator < is not applicable");
}

std::shared_ptr<type> type::operator>=(type& another)
{
    return value_of((*this > another)->to_bool() || (*this == another)->to_bool());
}

std::shared_ptr<type> type::operator<=(type& another)
{
    return value_of((*this < another)->to_bool() || (*this == another)->to_bool());
}

std::shared_ptr<type> type::operator~()
{
    throw std::logic_error("Operator ~ is not applicable");
}

std::shared_ptr<type> type::assign(type& another)
{
    throw std::logic_error("Cannot perform assignment");
}

std::shared_ptr<type> type::at(type& index)
{
    throw std::logic_error("Cannot access as an array");
}

bool type::is_mutable()
{
    return false;
}

int type::to_int()
{
    throw std::logic_error("Not integer type");
}

bool type::to_bool()
{
    throw std::logic_error("Not boolean type");
}

std::string type::to_string()
{
    throw std::logic_error("Not string type");
}

void type::from_string(const std::string& string)
{
    throw std::logic_error("Cannot instantiate from string");
}

std::shared_ptr<type>type::invoke(const std::list<std::shared_ptr<type>>& arg_values)
{
    throw std::logic_error("Cannot invoke this type");
}

int_type::int_type(int value) : value(value)
{}

int int_type::to_int()
{
    return value;
}

std::string int_type::to_string()
{
    return std::to_string(value);
}

std::shared_ptr<type> int_type::operator+(type& another)
{
    int_type& casted = dynamic_cast<int_type&>(another);
    return std::shared_ptr<type>(new int_type(value + casted.value));
}

std::shared_ptr<type> int_type::operator-(type& another)
{
    int_type& casted = dynamic_cast<int_type&>(another);
    return std::shared_ptr<type>(new int_type(value - casted.value));
}

std::shared_ptr<type> int_type::operator*(type& another)
{
    int_type& casted = dynamic_cast<int_type&>(another);
    return std::shared_ptr<type>(new int_type(value * casted.value));
}

std::shared_ptr<type> int_type::operator/(type& another)
{
    int_type&& casted = dynamic_cast<int_type&&>(another);
    return std::shared_ptr<type>(new int_type(value / casted.value));
}

std::shared_ptr<type> int_type::operator%(type& another)
{
    int_type&& casted = dynamic_cast<int_type&&>(another);
    return std::shared_ptr<type>(new int_type(value % casted.value));
}

std::shared_ptr<type> int_type::operator==(type& another)
{
    int_type& casted = dynamic_cast<int_type&>(another);
    return std::shared_ptr<type>(new bool_type(value == casted.value));
}

std::shared_ptr<type> int_type::operator>(type& another)
{
    int_type& casted = dynamic_cast<int_type&>(another);
    return std::shared_ptr<type>(new bool_type(value > casted.value));
}

std::shared_ptr<type> int_type::operator<(type& another)
{
    int_type& casted = dynamic_cast<int_type&>(another);
    return std::shared_ptr<type>(new bool_type(value < casted.value));
}

mutable_int_type::mutable_int_type() : int_type(0)
{}

std::shared_ptr<type> mutable_int_type::assign(type& another)
{
    int_type& casted = dynamic_cast<int_type&>(another);
    value = casted.value;
    return std::shared_ptr<type>(new void_type());
}

void mutable_int_type::from_string(const std::string& string)
{
    try
    {
        value = std::stoi(string);
    }
    catch (std::exception e)
    {
        throw std::logic_error("Cannot convert \"" + string + "\" to integer");
    }
}

bool mutable_int_type::is_mutable()
{
    return true;
}

bool_type::bool_type(bool value) : value(value)
{}

bool bool_type::to_bool()
{
    return value;
}

std::string bool_type::to_string()
{
    return std::to_string(value);
}

std::shared_ptr<type> bool_type::operator~()
{
    return std::shared_ptr<type>(new bool_type(!value));
}

mutable_bool_type::mutable_bool_type() : bool_type(false)
{}

std::shared_ptr<type> mutable_bool_type::assign(type& another)
{
    bool_type& casted = dynamic_cast<bool_type&>(another);
    value = casted.value;
    return std::shared_ptr<type>(new void_type()); 
}

bool mutable_bool_type::is_mutable()
{
    return true;
}

string_type::string_type(const std::string& value) :
    value(value)
{}

std::string string_type::to_string()
{
    return value;
}

std::shared_ptr<type> string_type::operator+(type& another)
{
    string_type& casted = dynamic_cast<string_type&>(another);
    return std::shared_ptr<type>(new string_type(value + casted.value));
}

void mutable_string_type::from_string(const std::string& string)
{
    value = string;
}

mutable_string_type::mutable_string_type() : string_type("")
{}

std::shared_ptr<type> mutable_string_type::assign(type& another)
{
    string_type& casted = dynamic_cast<string_type&>(another);
    value = casted.value;
    return std::shared_ptr<type>(new void_type());
}

bool mutable_string_type::is_mutable()
{
    return true;
}

std::shared_ptr<type> mutable_void_type::assign(type& another)
{
    void_type& casted = dynamic_cast<void_type&>(another);
    return std::shared_ptr<type>(new void_type());
}

array_type::array_type(std::vector<std::shared_ptr<type>>&& value) :
    value(value)
{}

std::shared_ptr<type> array_type::at(type& index)
{
    return value.at(index.to_int());
}

invokeable_type::invokeable_type(expression_list& arguments, var_declare_expression& return_type, expression& body, context_manager& ctxt) :
    arguments(arguments), return_type(return_type), body(body), ctxt(ctxt)
{
    for (const auto& expr : arguments.get_list())
    {
        expression_list* list
            = dynamic_cast<expression_list*>(expr.get());
        for (const auto& decl : list->get_list())
        {
            std::shared_ptr<var_declare_expression> casted
                = std::dynamic_pointer_cast<var_declare_expression>(decl);
            signature.push_back(casted);
        }
    }
}

std::shared_ptr<type> invokeable_type::invoke(const std::list<std::shared_ptr<type>>& arg_values)
{
    if (arg_values.size() != signature.size())
        throw std::logic_error("Wrong number of arguments");
    return_type.eval();
    arguments.eval();
    auto iter = arg_values.begin();
    for (const auto& decl : signature)
    {
        // if ((*iter)->is_mutable())
            ctxt.get_local().redeclare(decl->get_name(), *iter);
        // else
            // ctxt.get(decl->get_name())->assign(**iter);
        ++iter;
    }
    body.eval();
    return ctxt.get(return_type.get_name());
}

native_invokeable_type::native_invokeable_type(native_func call) :
    call(call)
{}

std::shared_ptr<type> native_invokeable_type::invoke(native_sign arg_values)
{
    return call(arg_values);
}