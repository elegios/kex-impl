#include "stdlib.h"

int main() {
	int* data = (int*) malloc(10);
	int first = data[0];
	int second = data[1];
	int third = data[2];
	return first + second + third;
}