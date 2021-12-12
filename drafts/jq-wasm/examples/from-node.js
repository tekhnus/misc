require('../build/jq.js')().then((jq) => {
    var query = ".x";
    var inputs = [`{"x": [3, 14, 15]}`, `{"x": "hello"}`];

    var state = jq.jq_init();
    var result = jq.jq_compile(state, query);
    if (result != 1) {
        console.log("could not compile");
        return;
    }

    for (var input of inputs) {
        var inp = jq.jv_parse(input);
        if (jq.jv_get_kind(inp) == 0) {
            console.log("invalid input");
            return;
        }

        jq.jq_start(state, inp, 0);
        var output;
        while (output = jq.jq_next(state), jq.jv_get_kind(output) != 0) {
            console.log(jq.jv_dump_string_trunc(output, 1024));
        }
    }
});
