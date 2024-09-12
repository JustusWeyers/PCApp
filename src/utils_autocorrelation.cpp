#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericMatrix pairs_cpp (NumericMatrix data, NumericVector FromTo) {
  // Number of maximum rows of result data.frame
  int n = data.rows() * (FromTo[1]-FromTo[0]);
  // Empty matrix for results with n rows and two columns
  NumericMatrix values(n,2);
  // Keep track of next row to store result
  int runner = 0;

  // Iterate over each row in 'data'
  for (int i = 0; i < (data.rows()-(FromTo[1]-FromTo[0])); i++) {
    // For each row in data set j to zero
    int j = 1;
    // For each timestamp get the timestamps within by FromTo
    while (std::abs(data(i+j,0)-data(i,0)) < FromTo[1]) {
      // Break loop if index i + j exceeds data.rows()
      if ((i+j) > data.rows()) {
        break;
      }
      if (std::abs(data(i+j,0)-data(i,0)) >= FromTo[0]) {
        // Store value of current timestamp in first column
        values(runner,0) = data(i, 1);
        // Store value of FromTo-data in second column
        values(runner,1) = data(i+j, 1);
        // Increment global runner variable
        runner++;
      }
      // Increment j for the next FromTo-data
      j++;
    }
  }

  // NumericMatrix 'values' contains empty rows. The next loop serves to
  // remove them. Therefore an empty matrix of 'runner' rows and two
  // columns is created
  NumericMatrix values_clean(runner,2);

  // Iterate over rows of 'values_clean'
  for (int i = 0; i < values_clean.rows(); i++) {
    // Copy results to clean matrix
    values_clean(i,_) = values(i,_);
  }

  // Return clean matrix
  return values_clean;
}
