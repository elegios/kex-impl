#include "stdio.h"
#include "stdlib.h"

struct Object {
	int data;
	int more_data;
};
typedef struct Object Object;

int size = 1000;

int get(Object ** objects) {
	int s = 0;
	for(int i = 0; i < size; i++) {
		s += objects[i]->data;
	}
	return s;
}

int main(int argc, char* argv[]) {
	if(argc == 2) {
		sscanf(argv[1], "%d", &size);
	}
	Object ** objects = (Object **) malloc(sizeof(Object *) * size);
	for(int i = size; i >= 0; --i) {
		objects[i] = (Object *) malloc(sizeof(Object));
		Object data = {i, i};
		*objects[i] = data;
	}
	printf("%d\n", get(objects));
}