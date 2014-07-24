#include "Services.h"
#include "Dummy.h" 
#include "SystemConfiguration.h"

using namespace SBA;

void Services::kernel_Dummy() {
    Dummy* inst;
    if (init_state(SC_Dummy_Dummy)) {
        inst = new Dummy();
        store_state(SC_Dummy_Dummy,(void*)inst);
    } else {
        inst=(Dummy*)load_state(SC_Dummy_Dummy);
    }

    void* res;
	Symbol_t res_symbol = NIHIL;
    switch ( method() ) {
        case M_Dummy_Dummy_none:
		{
			int retval = inst->none((int)arg(0));
			res = (void*)retval;
			res_symbol=mkPointerSymbol(res);
			break;
		};

		default:
			std::cout << "ERROR: NO SUCH METHOD: " << method() << "for class Dummy\n";
    };
    result(res_symbol);
}	
