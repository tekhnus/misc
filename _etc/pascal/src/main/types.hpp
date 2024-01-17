#pragma once

#include <list>
#include <memory>
#include <string>
#include <vector>
#include <stdexcept>

class expression;
class expression_list;
class var_declare_expression;
class context_manager;

class type
{
public:
    virtual std::shared_ptr<type> operator+(type& another);
    virtual std::shared_ptr<type> operator-(type& another);
    virtual std::shared_ptr<type> operator*(type& another);
    virtual std::shared_ptr<type> operator/(type& another);
    virtual std::shared_ptr<type> operator%(type& another);
    virtual std::shared_ptr<type> operator==(type& another);
    virtual std::shared_ptr<type> operator>(type& another);
    virtual std::shared_ptr<type> operator<(type& another);
    virtual std::shared_ptr<type> operator>=(type& another);
    virtual std::shared_ptr<type> operator<=(type& another);
    virtual std::shared_ptr<type> operator~();
    virtual std::shared_ptr<type> assign(type& another);
    virtual std::shared_ptr<type> at(type& index);

    virtual bool is_mutable();

    virtual int to_int();
    virtual bool to_bool();
    virtual std::string to_string();

    virtual void from_string(const std::string& string);

    virtual std::shared_ptr<type> invoke(const std::list<std::shared_ptr<type>>& arg_values);
};


std::shared_ptr<type> value_of(int value);
std::shared_ptr<type> value_of(bool value);
std::shared_ptr<type> value_of(const std::string& value);

class int_type : public type
{
public:
    int_type(int value);
    int to_int();
    std::string to_string();
    std::shared_ptr<type> operator+(type& another);
    std::shared_ptr<type> operator-(type& another);
    std::shared_ptr<type> operator*(type& another);
    std::shared_ptr<type> operator/(type& another);
    std::shared_ptr<type> operator%(type& another);
    std::shared_ptr<type> operator==(type& another);
    std::shared_ptr<type> operator>(type& another);
    std::shared_ptr<type> operator<(type& another);
// protected:
    int value;
};

class mutable_int_type : public int_type
{
public:
    mutable_int_type();
    std::shared_ptr<type> assign(type& another);
    void from_string(const std::string& string);
    bool is_mutable();
};

class bool_type : public type
{
public:
    bool_type(bool value);
    bool to_bool();
    std::string to_string();

    std::shared_ptr<type> operator~();
// protected:
    bool value;
};

class mutable_bool_type : public bool_type
{
public:
    mutable_bool_type();
    std::shared_ptr<type> assign(type& another);
    bool is_mutable();
};

class string_type : public type
{
public:
    string_type(const std::string& value);
    std::string to_string();
    std::shared_ptr<type> operator+(type& another);
// protected:
    std::string value;
};

class mutable_string_type : public string_type
{
public:
    mutable_string_type();
    std::shared_ptr<type> assign(type& another);
    void from_string(const std::string& string);
    bool is_mutable();
};

class void_type : public type
{};

class mutable_void_type : public void_type
{
public:
    std::shared_ptr<type> assign(type& another);
};

class array_type : public type
{
public:
    array_type(std::vector<std::shared_ptr<type>>&& value);
    std::shared_ptr<type> at(type& index);
private:
    std::vector<std::shared_ptr<type>> value;
};

class invokeable_type : public type
{
public:
    invokeable_type(expression_list& arguments, var_declare_expression& return_type, expression& body, context_manager& ctxt);
    std::shared_ptr<type> invoke(const std::list<std::shared_ptr<type>>& arg_values);
private:
    expression& arguments;
    var_declare_expression& return_type;
    expression& body;
    std::list<std::shared_ptr<var_declare_expression>> signature;
    context_manager& ctxt;
};

typedef const std::list<std::shared_ptr<type>>& native_sign;
typedef std::function<std::shared_ptr<type>(native_sign)> native_func;

class native_invokeable_type : public type
{
public:
    native_invokeable_type(native_func call);
    std::shared_ptr<type> invoke(native_sign arg_values);
protected:
    native_func call;
};