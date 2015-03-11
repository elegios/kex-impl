#include <iostream>
#include <assert.h>

using namespace std;

typedef long long intish;

typedef struct Matrix {
  intish nrows;
  intish ncols;
  intish* data;
} Matrix;

Matrix multiply(Matrix A, Matrix B) {
  assert(A.ncols == B.nrows);
  Matrix C = {A.nrows, B.ncols, (intish *) malloc(sizeof(intish) * A.nrows * B.ncols)};
  for(intish row = 0; row < C.nrows; row++) {
  for(intish column = 0; column < C.ncols; column++) {
      intish s = 0;
      for(intish k = 0; k < A.ncols; k++) {
        s += A.data[k + row * A.ncols] * B.data[column + k * B.ncols];
      }
      C.data[column + row * C.nrows] = s;
    }
  }
  return C;
}

Matrix readMatrix() {
  intish nrows, ncols;
  cin >> nrows >> ncols;
  intish *data = new intish[nrows * ncols];
  for(intish i = 0; i < nrows * ncols; i++) {
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
