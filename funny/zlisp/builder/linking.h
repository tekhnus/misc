/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#define LOCAL static
LOCAL void get_varname(char *res,datum *dep_and_sym);
LOCAL char *prog_build_dep(vec *sl,size_t *p,datum *dep_and_sym,fdatum(*module_bytecode)(char *,datum *,extension *),datum *settings,datum *compdata,extension *ext);
LOCAL void prog_put_deps(vec *sl,size_t *p,datum *deps,datum *compdata);
LOCAL char *prog_build_deps(vec *sl,size_t *p,datum *deps,fdatum(*module_bytecode)(char *,datum *,extension *),datum *settings,datum *compdata,extension *ext);
LOCAL datum *extract_meta(vec sl,size_t run_main_off);
char *prog_link_deps(vec *sl,size_t *bdr_p,datum *builder_compdata,size_t p,fdatum(*module_bytecode)(char *,datum *,extension *),datum *settings,extension *ext);
size_t prog_build_init(vec *sl,size_t *ep,size_t *bdr_p,datum *compdata,datum *builder_compdata);
#define EXPORT
