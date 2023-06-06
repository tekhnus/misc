/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
datum *extract_meta(vec sl,size_t run_main_off);
#define LOCAL static
LOCAL char *prog_build_dep(vec *sl,datum *dep_and_sym,fdatum(*module_bytecode)(char *,datum *,extension *),datum *settings,datum *compdata,extension *ext);
LOCAL void get_varname(char *res,datum *dep_and_sym);
LOCAL char *prog_build_deps(vec *sl,datum *deps,fdatum(*module_bytecode)(char *,datum *,extension *),datum *settings,datum *compdata,extension *ext);
char *prog_link_deps(vec *sl,datum *builder_compdata,datum *input_meta,fdatum(*module_bytecode)(char *,datum *,extension *),datum *settings,extension *ext);
size_t prog_build_init(vec *sl,datum *compdata,datum *builder_compdata);
#define EXPORT
