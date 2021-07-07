// a small library which wraps zlisp interpreter implementation
// so that it can be used from within zlisp itself.
#include <zlisp-impl/main.h>

eval_result_t read(datum_t *sptr) {
  if (!datum_is_pointer(sptr) || !datum_is_symbol(sptr->pointer_descriptor) ||
      strcmp(sptr->pointer_descriptor->symbol_value, "pointer")) {
    return eval_result_make_panic("read expects a pointer argument");
  }
  read_result_t r = datum_read(*(FILE **)sptr->pointer_value);
  if (read_result_is_eof(r)) {
    return eval_result_make_ok(datum_make_list_1(datum_make_symbol(":eof")));
  }
  if (!read_result_is_ok(r)) {
    char *err_message;
    if (read_result_is_panic(r)) {
      err_message = r.panic_message;
    } else {
      err_message = "unknown read error";
    }
    datum_t *err = datum_make_list_2(datum_make_symbol(":err"),
                                     datum_make_bytestring(err_message));
    return eval_result_make_ok(err);
  }
  datum_t *ok = datum_make_list_2(datum_make_symbol(":ok"), r.ok_value);
  return eval_result_make_ok(ok);
}

eval_result_t eval(datum_t *v, datum_t *ns) {
  eval_result_t r = datum_eval(v, ns);
  if (eval_result_is_panic(r)) {
    return eval_result_make_ok(datum_make_list_2(
        datum_make_symbol(":err"), datum_make_bytestring(r.panic_message)));
  }
  if (eval_result_is_context(r)) {
    return eval_result_make_ok(
        datum_make_list_2(datum_make_symbol(":context"), r.context_value));
  }
  return eval_result_make_ok(
      datum_make_list_2(datum_make_symbol(":ok"), r.ok_value));
}

eval_result_t prelude() {
  return eval_result_make_ok(namespace_make_prelude());
}
