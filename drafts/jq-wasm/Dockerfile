FROM emscripten/emsdk:3.0.0

RUN apt update -y
RUN apt install -y autoconf libtool

RUN git clone https://github.com/stedolan/jq
WORKDIR jq
RUN git checkout b6be13d5de6dd7d8aad5fd871eb6b0b30fc7d7f6
# This is a quite old commit.
# See https://github.com/stedolan/jq/issues/2091 for the reasons.
RUN git submodule update --init

RUN autoreconf -fi
RUN emconfigure ./configure --with-oniguruma=builtin --disable-maintainer-mode --disable-docs
COPY auxiliary.c .
RUN cat auxiliary.c >>src/main.c
RUN emmake make -j7

COPY pre-js.js .
RUN emcc src/*.o modules/oniguruma/src/*.o -o jq.js -s EXPORTED_RUNTIME_METHODS="[\"ccall\",\"cwrap\"]" -s MODULARIZE -s EXPORT_NAME=create_jq -s SINGLE_FILE=1 -s EXPORTED_FUNCTIONS="[_jv_parse, _jv_parse_ptr, _jv_number_value, _jv_number_value_ptr, _jq_next, _jq_next_ptr, _jv_get_kind, _jq_init, _jq_compile, _jq_start, _jq_start_ptr, _jv_dump_string_trunc, _jv_dump_string_trunc_ptr]" --pre-js pre-js.js