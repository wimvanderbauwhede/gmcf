
#ifndef SERVICEMANAGER_H_
#define SERVICEMANAGER_H_

//
// :title: Gannet Service-based SoC project - Service Manager class
//
//
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
//
//
// ==============================================================================
//
//  Gannet Service-based SoC project - Service Manager class
//
// ==============================================================================
//

#include "Base/System.h" 
#include "Base/Tile.h" 
#include "ServiceManagerObjects.h" 
#ifdef CYCLES
#include "../build/cycle.h"
#endif

using namespace std;

namespace SBA {
class ServiceManager {
	public:

        	Base::System* sba_system_ptr;
        	Base::Tile* sba_tile_ptr;

	Service service;
	ServiceAddress address;
	Service     debug_service;
	bool     debug_all;
	MemAddress     service_core_ram;
	Packet_Fifo data_fifo;
	Word_List subtask_fifo;
	Packet_Fifo request_fifo; // OBSOLETE
	Packet_Fifo     subtask_reference_fifo;
	Packet_Fifo subtask_code_fifo;
	Packet_Fifo tx_fifo;
	// For GMCF
	Packet_Fifo dreq_fifo;
	Packet_Fifo treq_fifo;
	Packet_Fifo dresp_fifo;
	Packet_Fifo tresp_fifo;
	Packet_Fifo fin_fifo;

	Subtasks     pending_subtasks_fifo;
	Subtask_List     subtask_list;
	Symbol_Table     symbol_table; // OBSOLETE? NEEDED for streaming, but find a better way!
	CodeStatus_Table code_status;
	Stack<SUBTASKS_SZ>     subtasks_address_stack;
	Stack<DATA_SZ-NREGS>     data_address_stack; // OBSOLETE

    Word_List results_store; // Should be Word
    Subtask_Argument_List arg_addresses;  // OBSOLETE
    Subtask current_subtask;
    Service service_id;
    Core_Status core_status;
    uint scid,sclid;
    Packet_Type core_return_type;
    uint opcode;
    uint n_args;
    bool ack_ok;
   bool status;

RegisterEntry register_set[NREGS];
//Requests request_table[NREGS];
RequestTable request_table;

ServiceManager(Base::System* sba_s_, Base::Tile* sba_t_, Service& s_,ServiceAddress& addr_)
			: sba_system_ptr(sba_s_), sba_tile_ptr(sba_t_), service(s_), address(addr_),
			debug_service(0), debug_all(true),
#ifdef SBA_USE_ADDRESS_STACKS
			subtasks_address_stack(SUBTASKS_OF),
			data_address_stack(DATA_OF+NREGS),
#endif
            current_subtask(0),
            service_id(s_),
            core_status(CS_idle),scid(0),sclid(0),core_return_type(P_data),
            status(true)
             {
                for (uint reg=1;reg<NREGS;reg++) {
                    MemAddress reg_addr=reg+DATA_OF;
                    Word reg_symbol=mkSymbol(K_D,0,0,0,0,((M_cache << FS_Mode)+(reg_addr << FS_Reg)),service);
                    symbol_table[reg_addr]=reg_symbol;
                }
            };

 void run();
 void demux_packets_by_type(Packet_t&);
 void receive_packets();
 void activate_subtask_helper(CodeAddress,Name_t,Word_List&,bool);
 void store_subtask_code();
 void activate_subtask();
 void store_data();
// void dispatch_data_packets();
 void parse_subtask();
    #ifndef STATIC_ALLOC
 MemAddresses build_value_address_list();
    #endif

// void send_ack();
 void restart_subtask(MemAddress);
 void core_control();
 void clean_up();

}; // ServiceManager
} // namespace SBA

#endif // SERVICEMANAGER_H_
