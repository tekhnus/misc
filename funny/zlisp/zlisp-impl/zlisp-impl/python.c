#include <zlisp-impl/python.h>
#include <stdlib.h>

char* routine_to_python(routine c) {
  prog *p = c.prog_;
  "def __genname(args)";
    "stack=[[args]]";
    // populate the vars


    PROG_ARGS,
      "stack.append([])",
        PROG_PUT_VAR,
      "stack[-1].append(", p->put_var_value->bytestring_value, ")",

  PROG_PUT_CONST,
      "stack[-1].append(", datum_to_python(p->put_const_value), ")",
  PROG_PUT_ROUTINE,
      "stack[-1].append(",  routine_to_python(p->put_routine_value->routine_value), ")",

  PROG_COLLECT,
      "stack[-1].extend(stack.pop())",
  PROG_CALL,
      "def tmp = stack[-1].pop(); def f = tmp[0]; def a = tmp[1:]; stack[-1].append(f(a))",
  PROG_POP,
      "def ", p->pop_var, " = ", "stack[-1].pop()",
  PROG_POP_PROG,
      "def ", p->pop_prog_var, " = ", "stack[-1].pop()",
  PROG_RETURN,
      "return stack[-1].pop()",
  PROG_IF,
      "if stack[-1].pop():",
  PROG_NOP,
      "pass",
  PROG_END,
      "print('we shouldn't be here')",
  PROG_MODULE_END,
      "return locals()",
      
  PROG_YIELD,
      "print('no yields')",
  PROG_HOST,
      "print('no host instructions')";

}

char *datum_to_python(datum *d) {
  if (datum_is_nil(d)) {
    return "None";
  }
  if (datum_is_list(d)) {
    return "(", datum_to_python(d->list_head), datum_to_python(d->list_tail), ")";
  }
  if (datum_is_symbol(d)) {
    return "'", d->symbol_value, "'";
  }
  if (datum_is_bytestring(d)) {
    return "'", d->bytestring_value, "'";
  }
  if (datum_is_integer(d)) {
    char *res = malloc(20);
    sprintf(res, "%d", d->integer_value);
    return res;
  }
  if (datum_is_void(d)) {
    return "None";
  }
  if (datum_is_routine(d)) {
    
  }
  if (datum_is_pointer(d)) {
    return "print('no pointers')";
  }
}
