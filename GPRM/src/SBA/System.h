
#ifndef SYSTEM_H_
#define SYSTEM_H_

//
// Gannet Service-based SoC project - SBA System (toplevel) class
//
// (c) 2004-2011 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//

#include <unordered_map>
#include <vector>
#include "Types.h"
#include "Packet.h"
#include "Base/System.h"

#include "Tile.h"
#include "GatewayTile.h"

#include "SystemConfiguration.h"

using namespace std;

namespace SBA {
class System : public Base::System {
	public:

	ServiceAddress gw_address;
	ServiceMap servicenodes;
	uint nservicenodes;
	uint finished;
	string task_data;
	string task_description;
	TaskDescList     task_descriptions;
	Bytecode     bytecode;
	vector<void*> args;
	vector<Word> regs;
	vector<pthread_mutex_t> reg_locks;
	vector<pthread_cond_t> reg_conds;
	void* result;
	uint io_mech;
	uint multi_ip;
	GatewayTile     gw_instance ;

    unordered_map<Service,Tile*> nodes;

	System(TaskDescList& tds_) : gw_address(0),
	finished(false), task_descriptions(tds_)
	,io_mech(1)
	,gw_instance(this,0,0,tds_)
    {
		// allocate some space for args and regs
		for (int regno = 0; regno< MAX_REGISTERFILE_SZ; regno++) {
			regs.push_back((Word)0);
		}
		for (unsigned int model=0; model < NSERVICES; model++) {
			reg_locks.push_back(PTHREAD_MUTEX_INITIALIZER);
			reg_conds.push_back(PTHREAD_COND_INITIALIZER);
		}
		//Services
		nservicenodes=NSERVICES;
    for (Service node_id_=1;node_id_<=NSERVICES;node_id_++) {
        ServiceAddress service_address=node_id_;
        Service node_id = node_id_;
        if  (service_address != 0) {
          // We can have duplicate node IDs, no need to create the node twice!
          if (nodes.count(node_id)==0) {
              nodes[node_id]=new Tile(this,node_id,service_address);
          }

#ifdef VERBOSE
                cout << "\nInstantiating service "<<node_id<<" ("<<service_address<<")\n";
#endif // VERBOSE
          }
    }

	};

	// --------------------------------------------------------------
	System(std::string td_) : gw_address(0),
	finished(false), task_description(td_), result(nullptr)
	,gw_instance(this,0,0,td_)
    {
		// allocate some space for args and regs
		for (int regno = 0; regno< MAX_REGISTERFILE_SZ; regno++) {
			regs.push_back((Word)0);
		}
		for (unsigned int model=0; model < NSERVICES; model++) {
			reg_locks.push_back(PTHREAD_MUTEX_INITIALIZER);
			reg_conds.push_back(PTHREAD_COND_INITIALIZER);
		}
		//Services
		nservicenodes=NSERVICES;
    for (Service node_id_=1;node_id_<=NSERVICES;node_id_++) {
        ServiceAddress service_address=node_id_;
        Service node_id = node_id_;//service_address;
        if  (service_address != 0) {
          // This check is not needed if we create contiguous instances
          // but if we use SERVICE_ADDRESSES it would be required
          if (nodes.count(node_id)==0)
              nodes[node_id]=new Tile(this,node_id,service_address);
#ifdef VERBOSE
                cout << "\nInstantiating service "<<node_id<<" ("<<service_address<<")\n";
#endif // VERBOSE
          }
    }

	};
    Service service_by_address(ServiceAddress address);
 void run();

#if USE_THREADS==1
 void run_th();
#endif // USE_THREADS

}; // System
} // namespace SBA

#endif // SYSTEM_H_
