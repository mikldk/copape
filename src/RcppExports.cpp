// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// merge_pedigree
Rcpp::DataFrame merge_pedigree(const Rcpp::IntegerVector& pids, const Rcpp::IntegerVector& pids_dad, const Rcpp::IntegerVector& birthyears, const Rcpp::IntegerVector& paternalped_ids, const int pedid_to_merge, const Rcpp::ListOf<Rcpp::IntegerVector>& sons_configs, const int no_surrogate_ancestors, const int surr_pid_start, const bool verbose);
RcppExport SEXP _copape_merge_pedigree(SEXP pidsSEXP, SEXP pids_dadSEXP, SEXP birthyearsSEXP, SEXP paternalped_idsSEXP, SEXP pedid_to_mergeSEXP, SEXP sons_configsSEXP, SEXP no_surrogate_ancestorsSEXP, SEXP surr_pid_startSEXP, SEXP verboseSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type pids(pidsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type pids_dad(pids_dadSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type birthyears(birthyearsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type paternalped_ids(paternalped_idsSEXP);
    Rcpp::traits::input_parameter< const int >::type pedid_to_merge(pedid_to_mergeSEXP);
    Rcpp::traits::input_parameter< const Rcpp::ListOf<Rcpp::IntegerVector>& >::type sons_configs(sons_configsSEXP);
    Rcpp::traits::input_parameter< const int >::type no_surrogate_ancestors(no_surrogate_ancestorsSEXP);
    Rcpp::traits::input_parameter< const int >::type surr_pid_start(surr_pid_startSEXP);
    Rcpp::traits::input_parameter< const bool >::type verbose(verboseSEXP);
    rcpp_result_gen = Rcpp::wrap(merge_pedigree(pids, pids_dad, birthyears, paternalped_ids, pedid_to_merge, sons_configs, no_surrogate_ancestors, surr_pid_start, verbose));
    return rcpp_result_gen;
END_RCPP
}
// validate_merge_input
void validate_merge_input(const Rcpp::IntegerVector& pids, const Rcpp::IntegerVector& pids_dad, const Rcpp::IntegerVector& birthyears, const Rcpp::IntegerVector& paternalped_ids);
RcppExport SEXP _copape_validate_merge_input(SEXP pidsSEXP, SEXP pids_dadSEXP, SEXP birthyearsSEXP, SEXP paternalped_idsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type pids(pidsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type pids_dad(pids_dadSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type birthyears(birthyearsSEXP);
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type paternalped_ids(paternalped_idsSEXP);
    validate_merge_input(pids, pids_dad, birthyears, paternalped_ids);
    return R_NilValue;
END_RCPP
}
// validate_surr_pid_start
void validate_surr_pid_start(const Rcpp::IntegerVector& pids, const int surr_pid_start);
RcppExport SEXP _copape_validate_surr_pid_start(SEXP pidsSEXP, SEXP surr_pid_startSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::IntegerVector& >::type pids(pidsSEXP);
    Rcpp::traits::input_parameter< const int >::type surr_pid_start(surr_pid_startSEXP);
    validate_surr_pid_start(pids, surr_pid_start);
    return R_NilValue;
END_RCPP
}
// validate_sons_configs
void validate_sons_configs(const Rcpp::ListOf<Rcpp::IntegerVector>& sons_configs);
RcppExport SEXP _copape_validate_sons_configs(SEXP sons_configsSEXP) {
BEGIN_RCPP
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const Rcpp::ListOf<Rcpp::IntegerVector>& >::type sons_configs(sons_configsSEXP);
    validate_sons_configs(sons_configs);
    return R_NilValue;
END_RCPP
}
// random_index
int random_index(int n);
RcppExport SEXP _copape_random_index(SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< int >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(random_index(n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_copape_merge_pedigree", (DL_FUNC) &_copape_merge_pedigree, 9},
    {"_copape_validate_merge_input", (DL_FUNC) &_copape_validate_merge_input, 4},
    {"_copape_validate_surr_pid_start", (DL_FUNC) &_copape_validate_surr_pid_start, 2},
    {"_copape_validate_sons_configs", (DL_FUNC) &_copape_validate_sons_configs, 1},
    {"_copape_random_index", (DL_FUNC) &_copape_random_index, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_copape(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
