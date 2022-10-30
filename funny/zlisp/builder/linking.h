/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
char *prog_slice_relocate(prog_slice *dst,size_t *p,datum *src);
#define LOCAL static
LOCAL datum *instruction_relocate(datum *ins,size_t delta);
LOCAL datum *offset_relocate(datum *ins,size_t delta);
LOCAL char *get_varname(datum *dep_and_sym);
LOCAL char *prog_build_dep(prog_slice *sl,size_t *p,datum *dep_and_sym,fdatum(*module_bytecode)(char *,datum *),datum *settings,datum **compdata);
LOCAL void prog_put_deps(prog_slice *sl,size_t *p,datum *deps,datum **compdata);
LOCAL char *prog_build_deps(prog_slice *sl,size_t *p,datum *deps,fdatum(*module_bytecode)(char *,datum *),datum *settings,datum **compdata);
LOCAL datum *extract_meta(prog_slice sl,size_t run_main_off);
char *prog_link_deps(prog_slice *sl,size_t *bdr_p,datum **builder_compdata,size_t p,fdatum(*module_bytecode)(char *,datum *),datum *settings);
size_t prog_build_init(prog_slice *sl,size_t *ep,size_t *bdr_p,datum **compdata,datum **builder_compdata);
#define EXPORT
