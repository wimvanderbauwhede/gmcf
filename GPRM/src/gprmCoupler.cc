/*
 This is the main program for model coupling. There should be no need for any I/O at this level I think.
 However, I will need to work out a way to deal with command line arguments for the original executables.

 For now, we require that the executables work without any command-line arguments.
 */

#ifdef TOP_CYCLES
#include "../cycle.h"
#endif

#ifdef TIMINGS
#include <sys/time.h>
#endif


#include <iostream>
#include "../SBA/Runtime.h"

using namespace std;
using namespace SBA;

int main(int argc, char* argv[]) {
	std::string tdc_file(argv[1]);
	unsigned int ncycles=0xFFFFFFFFUL; // 4 billion time steps by default
    if(argc>2) {
        ncycles=atoi(argv[2]);
    }
    // I think having the .td file has some merit,
    // if only to be able to select the starting point of the computation
    // And having a limit on the number of time steps is safe
    // Although I'm not sure if these are the same as the model timesteps
	Runtime coupler(tdc_file,ncycles);
#ifdef TOP_CYCLES
	ticks t0=getticks();
#endif
	coupler.run();
#ifdef TOP_CYCLES
     ticks t1=getticks();
cout << "TOP CYCLES: "<<  elapsed(t1,t0) <<"\n";
#endif

    return 0;
}
