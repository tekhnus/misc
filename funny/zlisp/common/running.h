/* This file was automatically generated.  Do not edit! */
#undef INTERFACE
#define LOCAL static
LOCAL bool routine_2_is_null(routine_2 r);
LOCAL bool routine_1_is_null(routine_1 r);
LOCAL bool routine_0_is_null(routine_0 r);
LOCAL routine_2 routine_2_make_null();
LOCAL routine_1 routine_1_make_null();
LOCAL routine_0 routine_0_make_null();
LOCAL char *routine_0_step(routine_0 *r,fdatum(*perform_host_instruction)(datum *,datum *));
LOCAL routine_0 routine_0_make(prog *s,state *ctxt);
LOCAL char *routine_1_step(prog_slice sl,routine_1 *r,fdatum(*perform_host_instruction)(datum *,datum *));
LOCAL char *datum_to_routine_1(routine_1 *res,prog_slice sl,datum *fns);
LOCAL char *datum_to_routine_0(routine_0 *res,prog_slice sl,datum *fn);
LOCAL datum *routine_1_to_datum(prog_slice sl,routine_1 r);
LOCAL datum *routine_0_to_datum(prog_slice sl,routine_0 r);
LOCAL routine_0 routine_1_pop_frame(routine_1 *r);
LOCAL void routine_1_push_frame(routine_1 *r,routine_0 sub);
LOCAL routine_1 routine_2_pop_frame(routine_2 *r);
LOCAL void routine_2_push_frame(routine_2 *r,routine_1 sub);
LOCAL char *routine_2_step(prog_slice sl,routine_2 *r,fdatum(*perform_host_instruction)(datum *,datum *));
LOCAL char *routine_2_run(prog_slice sl,routine_2 *r,fdatum(*perform_host_instruction)(datum *,datum *));
fdatum routine_run_and_get_value(prog_slice sl,state **ctxt,prog *p,fdatum(*perform_host_instruction)(datum *,datum *));
#define EXPORT
