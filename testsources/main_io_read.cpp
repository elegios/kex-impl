#include <iostream>
#include <assert.h>

using namespace std;

typedef struct Matrix {
  int nrows;
  int ncols;
  int* data;
} Matrix;

Matrix multiply(Matrix A, Matrix B) {
  assert(A.ncols == B.nrows);
  Matrix C = {A.nrows, B.ncols, (int *) malloc(sizeof(int) * A.nrows * B.ncols)};
  for(int row = 0; row < C.nrows; row++) {
    for(int column = 0; column < C.ncols; column++) {
      int s = 0;
      for(int k = 0; k < A.ncols; k++) {
        s += A.data[k + row * A.ncols] * B.data[column + k * B.ncols];
      }
      C.data[column + row * C.nrows] = s;
    }
  }
  return C;
}

Matrix readMatrix() {
  int nrows, ncols;
  cin >> nrows >> ncols;
  int *data = new int[nrows * ncols];
  for(int i = 0; i < nrows * ncols; i++) {
    cin >> data[i];
  }
  Matrix r = {ncols, nrows, data};
  return r;
}

int main() {
  Matrix A = readMatrix();
  Matrix B = readMatrix();
  multiply(A, B);
}
