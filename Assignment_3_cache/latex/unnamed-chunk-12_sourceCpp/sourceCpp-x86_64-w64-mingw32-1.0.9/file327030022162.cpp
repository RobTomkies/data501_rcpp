

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

