#include "context.hpp"

#include <stdexcept>
#include <iostream>

std::shared_ptr<type> context::get(const std::string& name)
{
    try
    {
        return vars.at(name);
    }
    catch (std::exception e)
    {
        throw std::logic_error("Variable " + name + " was not declared");
    }
}

void context::declare(const std::string& name, std::shared_ptr<type> init_value)
{
    if (!vars.insert(std::make_pair(name, init_value)).second)
    {
        throw std::logic_error("Redeclaration of variable " + name);
    }
}

void context::redeclare(const std::string& name, std::shared_ptr<type> init_value)
{
    vars.at(name) = init_value;
}

void context::trace()
{
    for (const auto& var : vars)
        std::cout << var.first << " = " << var.second->to_string() << std::endl;
}

std::shared_ptr<type> context_manager::get(const std::string& name)
{
    try
    {
        return local.top().get(name);
    }
    catch (std::exception e)
    {
        return global.get(name);
    }
}

context& context_manager::get_global()
{
    return global;
}

context& context_manager::get_local()
{
    return local.top();
}

void context_manager::put_local()
{
    local.push(context());
}

void context_manager::pop_local()
{
    local.pop();
}

void context_manager::trace()
{
    std::stack<context> copy(local);
    while (!copy.empty())
    {
        copy.top().trace();
        copy.pop();
        if (!copy.empty())
            std::cout << "------" << std::endl;
    }
    std::cout << "======" << std::endl;
}