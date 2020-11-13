#include <stdlib.h>

struct thunk {
	void* (*_hoist)(void* _this, void*);
};

static void* call(struct thunk* f, void* x){
	return f->_hoist(f,x);
}
