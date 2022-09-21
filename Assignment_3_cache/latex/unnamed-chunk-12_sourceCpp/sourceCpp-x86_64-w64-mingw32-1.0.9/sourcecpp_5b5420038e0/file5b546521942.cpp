#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double llikef3(NumericVector par, NumericVector y) {
  double alpha = exp(par[0]);
  double beta = exp(par[1]);
  int n = y.length();
  double sum_y = 0;
  double sum_log_y = 0;
  for (int i = 0; i < n; i++){
    sum_y += y[i];
    sum_log_y += log(y[i]);
  }
  return (n*alpha*log(beta))-(n*lgamma(alpha))+((alpha-1)*sum_log_y) - (beta*sum_y);
}

// [[Rcpp::export]]
double llikef4(NumericVector par, NumericVector y) {
  double alpha = exp(par[0]);
  double beta = exp(par[1]);
  int n = y.length();
  double sum_y = sum(y);
  double sum_log_y = sum(log(y));
  return (n*alpha*log(beta))-(n*lgamma(alpha))+((alpha-1)*sum_log_y) - (beta*sum_y);
}



#include <Rcpp.h>
#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// llikef3
double llikef3(NumericVector par, NumericVector y);
RcppExport SEXP sourceCpp_1_llikef3(SEXP parSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type par(parSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(llikef3(par, y));
    return rcpp_result_gen;
END_RCPP
}
// llikef4
double llikef4(NumericVector par, NumericVector y);
RcppExport SEXP sourceCpp_1_llikef4(SEXP parSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type par(parSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(llikef4(par, y));
    return rcpp_result_gen;
END_RCPP
}
