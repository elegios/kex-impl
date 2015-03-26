#include "stdio.h"
#include "stdlib.h"

struct Object {
	int data;
	int more_data;
};
typedef struct Object Object;

const int size = 1000;

void update(Object ** objects) {
	for(int i = 0; i < size; i++) {
		objects[i]->data = i;
	}
}

int main() {
	Object ** objects = (Object **) malloc(sizeof(Object *) * size);
	for(int i = size; i >= 0; --i) {
		objects[i] = (Object *) malloc(sizeof(Object));
		Object data = {0, 0};
		*objects[i] = data;
	}
	update(objects);
}