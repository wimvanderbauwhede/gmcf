/*
 This is the main program for model coupling. There should be no need for any I/O at this level I think.
 However, I will need to work out a way to deal with command line arguments for the original executables.

 For now, we require that the executables work without any command-line arguments.
 */

#include <string>
#include "gmcf.h"

using namespace std;
using namespace SBA;

int main(int argc, char* argv[]) {
	//std::string tdc_file(argv[1]);
	std::string tdc_file("gmcf.tdc64");
	unsigned int ncycles=0xFFFFFFFFUL; // 4 billion time steps by default
    if(argc>2) {
        ncycles=atoi(argv[2]);
    }
    // I think having the .td file has some merit,
    // if only to be able to select the starting point of the computation
    // And having a limit on the number of time steps is safe
    // Although I'm not sure if these are the same as the model timesteps
	gmcfCoupler(tdc_file,ncycles);

    return 0;
}
