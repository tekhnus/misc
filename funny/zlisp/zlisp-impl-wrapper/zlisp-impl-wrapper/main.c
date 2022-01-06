// a small library which wraps zlisp interpreter implementation
// so that it can be used from within zlisp itself.
#include <zlisp-impl/zlisp-impl.h>
#include <stdlib.h>
#include <string.h>

fdatum read(datum *sptr) {
  if (!datum_is_pointer(sptr) || !datum_is_symbol(sptr->pointer_descriptor) ||
      strcmp(sptr->pointer_descriptor->symbol_value, "pointer")) {
    return fdatum_make_panic("read expects a pointer argument");
  }
  read_result r = datum_read(*(FILE **)sptr->pointer_value);
  if (read_result_is_eof(r)) {
    return fdatum_make_ok(datum_make_list_1(datum_make_symbol(":eof")));
  }
  if (!read_result_is_ok(r)) {
    char *err_message;
    if (read_result_is_panic(r)) {
      err_message = r.panic_message;
    } else {
      err_message = "unknown read error";
    }
    datum *err = datum_make_list_2(datum_make_symbol(":err"),
                                     datum_make_bytestring(err_message));
    return fdatum_make_ok(err);
  }
  datum *ok = datum_make_list_2(datum_make_symbol(":ok"), r.ok_value);
  return fdatum_make_ok(ok);
}

fdatum eval(datum *v, datum *nsp) {
  state *ns = *(state **)nsp->pointer_value;
  char *err = state_value_eval(&ns, v, NULL);
  if (err != NULL) {
    return fdatum_make_ok(datum_make_list_2(
        datum_make_symbol(":err"), datum_make_bytestring(err)));
  }
  datum *val = state_value_pop(&ns);
  void **new_nsp = malloc(sizeof(void **));
  *new_nsp = ns;
  return fdatum_make_ok(datum_make_list_3(datum_make_symbol(":ok"), val,
                                       datum_make_pointer_to_pointer(new_nsp)));
}

fdatum builtins() {
  state *builtins = state_make_builtins();
  void **builtins_p = malloc(sizeof(void **));
  *builtins_p = builtins;
  return fdatum_make_ok(datum_make_list_2(
      datum_make_symbol(":ok"), datum_make_pointer_to_pointer(builtins_p)));
}
