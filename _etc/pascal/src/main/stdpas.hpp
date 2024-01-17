#pragma once

#include <memory>
#include "types.hpp"
#include "context.hpp"

namespace stdpas
{

void declare(const std::string& name, native_func call, context_manager& ctxt);
void load_all(context_manager& ctxt);

void check_count(native_sign values, int correct);

std::shared_ptr<type> write(native_sign values);
std::shared_ptr<type> read(native_sign values);
std::shared_ptr<type> inc(native_sign values);
std::shared_ptr<type> dec(native_sign values);

}