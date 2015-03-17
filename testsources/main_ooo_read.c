#include "stdio.h"
#include "stdlib.h"

typedef long long intish;

typedef struct Matrix {
  int nrows;
  int ncols;
  intish* data;
} Matrix;

__attribute__ ((noinline))
Matrix multiply(Matrix A, Matrix B) {
  Matrix C = {A.nrows, B.ncols, (intish *) malloc(sizeof(intish) * A.nrows * B.ncols)};
  for(int column = 0; column < C.ncols; column++) {
    for(int row = 0; row < C.nrows; row++) {
      intish s = 0;
      for(int k = 0; k < A.ncols; k++) {
        intish a = A.data[k + row * A.ncols];
        intish b = B.data[column + k * B.ncols];
        s += a * b;
      }
      C.data[column + row * C.ncols] = s;
    }
  }
  return C;
}

void printMatrix(Matrix matrix) {
  for(int i = 0; i < matrix.nrows; i++) {
    for(int j = 0; j < matrix.ncols; j++) {
      printf("%lld ", matrix.data[j + i * matrix.ncols]);
    }
    printf("\n");
  }
}

Matrix readMatrix() {
  int nrows, ncols;
  scanf("%d", &nrows);
  scanf("%d", &ncols);
  intish *data = (intish*) malloc(nrows * ncols * sizeof(intish));
  for(intish i = 0; i < nrows * ncols; i++) {
    scanf("%lld", &data[i]);
  }
  Matrix r = {ncols, nrows, data};
  return r;
}

int main() {
  Matrix A = readMatrix();
  Matrix B = readMatrix();
  multiply(A, B);
}
