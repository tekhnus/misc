jv* jv_parse_ptr(const char *s) {
  jv *res = malloc(sizeof(jv));
  *res = jv_parse(s);
  return res;
}

void jq_start_ptr(jq_state *jq, jv *inp, int flags) {
  jq_start(jq, *inp, flags);
}

jv* jq_next_ptr(jq_state *state) {
  jv *dynres = malloc(sizeof(jv));
  *dynres = jq_next(state);
  return dynres;
}

double jv_number_value_ptr(jv *inp) {
  return jv_number_value(*inp);
}

char *jv_dump_string_trunc_ptr(jv *x, size_t bufsize) {
  char *buf = malloc(bufsize);
  return jv_dump_string_trunc(*x, buf, bufsize);
}
