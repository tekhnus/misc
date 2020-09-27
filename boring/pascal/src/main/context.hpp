#pragma once

#include "types.hpp"

#include <string>
#include <map>
#include <memory>
#include <stack>

class context
{
public:
    std::shared_ptr<type> get(const std::string& name);
    void declare(const std::string& name, std::shared_ptr<type> init_value);
    void redeclare(const std::string& name, std::shared_ptr<type> init_value);
    void trace();
private:
    std::map<std::string, std::shared_ptr<type>> vars;
};

class context_manager
{
public:
    std::shared_ptr<type> get(const std::string& name);

    context& get_global();
    context& get_local();
    void put_local();
    void pop_local();
    void trace();
private:
    context global;
    std::stack<context> local;
};