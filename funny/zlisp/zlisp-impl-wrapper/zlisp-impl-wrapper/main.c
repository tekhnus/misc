// a small library which wraps zlisp interpreter implementation
// so that it can be used from within zlisp itself.
#include <zlisp-impl/main.h>

val_t read(datum_t *sptr) {
  if (!datum_is_pointer(sptr) || !datum_is_symbol(sptr->pointer_descriptor) ||
      strcmp(sptr->pointer_descriptor->symbol_value, "pointer")) {
    return val_make_panic("read expects a pointer argument");
  }
  read_result_t r = datum_read(*(FILE **)sptr->pointer_value);
  if (read_result_is_eof(r)) {
    return val_make_ok(datum_make_list_1(datum_make_symbol(":eof")));
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
    return val_make_ok(err);
  }
  datum_t *ok = datum_make_list_2(datum_make_symbol(":ok"), r.ok_value);
  return val_make_ok(ok);
}

val_t eval(datum_t *v, datum_t *vars) {
  namespace_t *ns = namespace_make(vars, datum_make_nil());
  ctx_t r = datum_eval(v, ns);
  if (ctx_is_panic(r)) {
    return val_make_ok(datum_make_list_2(
        datum_make_symbol(":err"), datum_make_bytestring(r.panic_message)));
  }
  val_t val = namespace_peek(r.ok_value);
  if (val_is_panic(val)) {
    return val_make_panic(val.panic_message);
  }
  return val_make_ok(datum_make_list_3(
      datum_make_symbol(":ok"), val.ok_value, r.ok_value->vars));
}

val_t prelude() {
  ctx_t prelude = namespace_make_prelude();
  if (ctx_is_panic(prelude)) {
    return val_make_ok(
        datum_make_list_2(datum_make_symbol(":err"),
                          datum_make_bytestring(prelude.panic_message)));
  }
  return val_make_ok(
      datum_make_list_2(datum_make_symbol(":ok"), prelude.ok_value->vars));
}
