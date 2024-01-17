var Module = {
    "preInit": function() {
        Module.jq_init = Module.cwrap("jq_init", "number", []);
        Module.jq_compile = Module.cwrap("jq_compile", "number", ["number", "string"]);
        Module.jv_parse = Module.cwrap("jv_parse_ptr", "number", ["string"]);
        Module.jq_start = Module.cwrap("jq_start_ptr", null, ["number", "number"]);
        Module.jq_next = Module.cwrap("jq_next_ptr", "number", ["number"]);
        Module.jv_dump_string_trunc = Module.cwrap("jv_dump_string_trunc_ptr", "string", ["number", "number"]);
        Module.jv_get_kind = Module.cwrap("jv_get_kind", "number", ["number"]);
    }
};
