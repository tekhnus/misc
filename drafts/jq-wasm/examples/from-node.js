require('../build/jq.js')().then((jq) => {
  var s = jq.jq_init();
  var st = jq.jq_compile(s, ".x");
  console.log("jq_compile=", st);
  var p = jq.jv_parse("{\"x\": [33, 77]}");
  jq.jq_start(s, p, 0);
  var v = jq.jq_next(s);
  console.log("test=", jq.jv_dump_string_trunc(v, 1024));
});
