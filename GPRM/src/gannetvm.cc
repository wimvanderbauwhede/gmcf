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

int main(int ac, char* av[]) {


	    Runtime gmcf_coupler("gmcf.tdc"); // need version without args, easy
#ifdef TOP_CYCLES
     ticks t0=getticks();
#endif
     gmcf_coupler.run();
#ifdef TOP_CYCLES
     ticks t1=getticks();
cout << "TOP CYCLES: "<<  elapsed(t1,t0) <<"\n";
#endif

    return 0;
}
