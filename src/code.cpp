#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double mod(double x, int base) {
  return x - floor(x / base) * base;
}

// [[Rcpp::export]]
double get_index(
    NumericVector &x,
    int index,
    int offset
) {
  int N = x.size();
  int j = (index + offset) % N;
  return x[j];
}

// [[Rcpp::export]]
double cosine_similarity_cpp(
    NumericVector &x,
    NumericVector &template_,
    int offset
) {
  double Sxy = 0.0;
  double Sxx = 0.0;
  double Syy = 0.0;

  int N = x.size();
  int N2 = template_.size();

  if (N != N2) {
    stop("<x> and <template_> should both have the same size.");
  }

  for (int i = 0; i < N; i ++) {
    double x_i = get_index(x, i, offset);
    double y_i = template_[i];
    Sxy += x_i * y_i;
    Sxx += x_i * x_i;
    Syy += y_i * y_i;
  }

  return Sxy / (sqrt(Sxx) * sqrt(Syy));
}

// [[Rcpp::export]]
NumericVector sweep_template_cpp(
    NumericVector &x,
    NumericVector &template_
) {
  int array_dim = x.size();
  NumericVector res = NumericVector(array_dim);

  for (int offset = 0; offset < array_dim; offset ++) {
    res[offset] = cosine_similarity_cpp(x, template_, offset);
  }

  return res;
}
