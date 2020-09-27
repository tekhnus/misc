#include "stdpas.hpp"

#include <iostream>
#include <stdexcept>

namespace stdpas
{

void declare(const std::string& name, native_func call, context_manager& ctxt)
{
    ctxt.get_global().declare(name, std::shared_ptr<type>(new native_invokeable_type(call)));
}

void load_all(context_manager& ctxt)
{
    declare("write", write, ctxt);
    declare("read", read, ctxt);
    declare("inc", inc, ctxt);
    declare("dec", dec, ctxt);
}

void check_count(native_sign values, int correct)
{
    if (values.size() != correct)
        throw std::logic_error("Wrong number of arguments");
}

std::shared_ptr<type> write(native_sign values)
{
    for (const auto& value : values)
    {
        std::cout << value->to_string() << " ";
    }
    std::cout << std::endl;
    return std::shared_ptr<type>(new void_type());
}

std::shared_ptr<type> read(native_sign values)
{
    for (const auto& value : values)
    {
        std::string input;
        std::cin >> input;
        value->from_string(input);
    }
    return std::shared_ptr<type>(new void_type());
}

std::shared_ptr<type> inc(native_sign values)
{
    check_count(values, 1);
    std::shared_ptr<type> value = values.front();
    value->assign(*std::shared_ptr<type>(new int_type(value->to_int() + 1)));
    return std::shared_ptr<type>(new void_type());
}

std::shared_ptr<type> dec(native_sign values)
{
    check_count(values, 1);
    std::shared_ptr<type> value = values.front();
    value->assign(*std::shared_ptr<type>(new int_type(value->to_int() - 1)));
    return std::shared_ptr<type>(new void_type());
}

}