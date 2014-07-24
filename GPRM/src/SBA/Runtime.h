#ifndef RUNTIME_H_
#define RUNTIME_H_

// SBA/Runtime.rb
//   
// :title: Gannet Service-based SoC project - SBA Runtime class
//
//
// *
// *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *  
//
//==============================================================================
//
// Gannet Service-based SoC project - SBA Runtime class 
//
//==============================================================================
//

#include "Types.h" 
#include "SystemConfiguration.h" 
#include "System.h" 
#include <string>
#include <vector>

using namespace std;

namespace SBA {
class Runtime {
public:

	std::string task_description;
	Bytecode bytecode;
	System sba;
	Runtime(std::string td_, uint ncycles_ = 0xFFFFFFFFUL ) :
			task_description(td_), sba(task_description), ncycles(ncycles_) {
	};


	void setNCycles(uint);
    void args(const vector< void* >&);
	void args(void*);
	void args(void*,void*);
	void args(void*,void*,void*);
	void args(void*,void*,void*,void*);

    void* run(const vector< void* >&);
	void* run();
	void* run(void*);
	void* run(void*,void*);
	void* run(void*,void*,void*);
	void* run(void*,void*,void*,void*);

private:
	uint ncycles;
};
// Clas Runtime
}// SBA

#endif // RUNTIME_H_
