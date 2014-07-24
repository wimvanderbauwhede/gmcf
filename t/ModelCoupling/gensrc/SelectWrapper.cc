
#include "Services.h"

void Services::select_wrapper(unsigned int code) {

	switch (code) {
		case 519:
			ls_IO();
			break;
		case 514:
			ls_SEQ();
			break;
		case 520:
			ls_ALU();
			break;
		case 524:
			ls_REG();
			break;
		default:
			none();
	};
}
