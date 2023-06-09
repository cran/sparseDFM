// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// fastLambda
arma::mat fastLambda(const arma::cube& D, const arma::mat& C);
RcppExport SEXP _sparseDFM_fastLambda(SEXP DSEXP, SEXP CSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::cube& >::type D(DSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type C(CSEXP);
    rcpp_result_gen = Rcpp::wrap(fastLambda(D, C));
    return rcpp_result_gen;
END_RCPP
}
// kalmanMultivariate
List kalmanMultivariate(const arma::mat& X, const arma::mat& a0_0, const arma::mat& P0_0, const arma::mat& A, const arma::mat& Lambda, const arma::mat& Sig_e, const arma::mat& Sig_u);
RcppExport SEXP _sparseDFM_kalmanMultivariate(SEXP XSEXP, SEXP a0_0SEXP, SEXP P0_0SEXP, SEXP ASEXP, SEXP LambdaSEXP, SEXP Sig_eSEXP, SEXP Sig_uSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type a0_0(a0_0SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type P0_0(P0_0SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Lambda(LambdaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Sig_e(Sig_eSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Sig_u(Sig_uSEXP);
    rcpp_result_gen = Rcpp::wrap(kalmanMultivariate(X, a0_0, P0_0, A, Lambda, Sig_e, Sig_u));
    return rcpp_result_gen;
END_RCPP
}
// kalmanUnivariate
List kalmanUnivariate(const arma::mat& X, const arma::mat& a0_0, const arma::mat& P0_0, const arma::mat& A, const arma::mat& Lambda, const arma::mat& Sig_e, const arma::mat& Sig_u);
RcppExport SEXP _sparseDFM_kalmanUnivariate(SEXP XSEXP, SEXP a0_0SEXP, SEXP P0_0SEXP, SEXP ASEXP, SEXP LambdaSEXP, SEXP Sig_eSEXP, SEXP Sig_uSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type a0_0(a0_0SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type P0_0(P0_0SEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Lambda(LambdaSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Sig_e(Sig_eSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type Sig_u(Sig_uSEXP);
    rcpp_result_gen = Rcpp::wrap(kalmanUnivariate(X, a0_0, P0_0, A, Lambda, Sig_e, Sig_u));
    return rcpp_result_gen;
END_RCPP
}
// softThreshScalar
arma::mat softThreshScalar(const arma::mat& X, const double thresh);
RcppExport SEXP _sparseDFM_softThreshScalar(SEXP XSEXP, SEXP threshSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const double >::type thresh(threshSEXP);
    rcpp_result_gen = Rcpp::wrap(softThreshScalar(X, thresh));
    return rcpp_result_gen;
END_RCPP
}
// softThreshMatrix
arma::mat softThreshMatrix(const arma::mat& X, const arma::mat& thresh);
RcppExport SEXP _sparseDFM_softThreshMatrix(SEXP XSEXP, SEXP threshSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::mat& >::type X(XSEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type thresh(threshSEXP);
    rcpp_result_gen = Rcpp::wrap(softThreshMatrix(X, thresh));
    return rcpp_result_gen;
END_RCPP
}
// solveCube
arma::cube solveCube(const arma::cube& A, const arma::mat& B, const double nu);
RcppExport SEXP _sparseDFM_solveCube(SEXP ASEXP, SEXP BSEXP, SEXP nuSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::cube& >::type A(ASEXP);
    Rcpp::traits::input_parameter< const arma::mat& >::type B(BSEXP);
    Rcpp::traits::input_parameter< const double >::type nu(nuSEXP);
    rcpp_result_gen = Rcpp::wrap(solveCube(A, B, nu));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_sparseDFM_fastLambda", (DL_FUNC) &_sparseDFM_fastLambda, 2},
    {"_sparseDFM_kalmanMultivariate", (DL_FUNC) &_sparseDFM_kalmanMultivariate, 7},
    {"_sparseDFM_kalmanUnivariate", (DL_FUNC) &_sparseDFM_kalmanUnivariate, 7},
    {"_sparseDFM_softThreshScalar", (DL_FUNC) &_sparseDFM_softThreshScalar, 2},
    {"_sparseDFM_softThreshMatrix", (DL_FUNC) &_sparseDFM_softThreshMatrix, 2},
    {"_sparseDFM_solveCube", (DL_FUNC) &_sparseDFM_solveCube, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_sparseDFM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
