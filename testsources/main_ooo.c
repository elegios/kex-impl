#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

typedef struct Matrix {
  int nrows;
  int ncols;
  int* data;
} Matrix;

Matrix multiply(Matrix A, Matrix B) {
  assert(A.ncols == B.nrows);
  Matrix C = {A.nrows, B.ncols, (int *) malloc(sizeof(int) * A.nrows * B.ncols)};
  for(int column = 0; column < C.ncols; column++) {
    for(int row = 0; row < C.nrows; row++) {
      int s = 0;
      for(int k = 0; k < A.ncols; k++) {
	int a = A.data[k + row * A.ncols];
	int b = B.data[column + k * B.ncols];
	s += a * b;
      }
      C.data[column + row * C.ncols] = s;
    }
  }
  return C;
}

int main() {
  int a[9] = {1, 2, 3};
  Matrix A = {1, 3, a};
  int b[9] = {1, 2, 3};
  Matrix B = {3, 1, b};
  Matrix C = multiply(A, B);
  printf("Dimentions: %d %d\n", C.nrows, C.ncols);
  for(int i = 0; i < C.nrows; i++) {
    for(int j = 0; j < C.ncols; j++) {
      printf("%d\t", C.data[j + i * C.nrows]);
    }
    printf("\n");
  }
  printf("\n");
}
