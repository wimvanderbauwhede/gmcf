#include "gmcfF.h"
#include "SBA/Types.h"
#include "SBA/System.h"
#include "SBA/Tile.h"
#include "SBA/Packet.h"
#include "SBA/SpinLock.h"
#include "SBA/InclusionSetTable.h"
#include <cstring>
#include <pthread.h>

//#define GMCF_DEBUG
/*
Source == Return_to 16
Dest = To 16
Type = Packet_type 8
PrePost = Ctrl 5
DataId == Return_as
Timestamp == Ack_to
DataPtr == Payload

Request == P_request 4
Data == P_data 5
RequestTime == P_mm 6
RespTime == P_fragment 7
Any == P_error 0
 * */
// This doesn't need sysptr, but it could be I will have to use sysptr + address rather than tileptr
//! sba_sys, sba_tile(model_id), source, destination, packet_type, timestamp, pre_post, data_id, data_sz, data_ptr, fifo_empty
void gmcfreadfromfifoc_(
		int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* source, int* destination, int* packet_type, int* timestamp, int* pre_post, int* data_id, int* data_sz, int64_t* data_ptr,
		int *fifo_empty
) {
// So, we need to read from the FIFO. In fact, not as easy as it seems: the FIFO read operation is built into the Tile, and blocks.
	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfreadfromfifoc_: Tile address (sanity): <" << tileptr->address <<">\n";
	std::cout << "FORTRAN API C++ gmcfreadfromfifoc_: wait_for_packets\n";
#endif
	tileptr->transceiver->rx_fifo.wait_for_packets();

	// Now we know there is a packet, so get it.
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfreadfromfifoc_: get packet\n";
#endif
	SBA::Packet_t  p = tileptr->transceiver->rx_fifo.pop_front();
	SBA::Header_t ph= SBA::getHeader(p);
	 *destination = (int)SBA::getTo(ph);
	 *source = (int)SBA::getReturn_to(ph);
	 *packet_type = (int)SBA::getPacket_type(ph);
	 // For data packets (both req and resp), we packet timestamp and data_id in Ack_to and put data_sz in Return_as
	 uint64_t timestamp_data_id = getAck_to(ph);
	 if (*packet_type == P_TREQ or *packet_type==P_TRESP) {
		 *timestamp =  timestamp_data_id;
	 } else {
	 *timestamp =  (int)((timestamp_data_id >> 32) & 0xFFFFFFFFUL);
	 }
	 *data_id = (int)(timestamp_data_id & 0xFFFFFFFFUL);
	 *pre_post = (int)getCtrl(ph);
	 *data_sz = (int)SBA::getReturn_as(ph);
	 *data_ptr = (int64_t)SBA::getPayload_Word(p);
#ifdef GMCF_DEBUG
	 std::cout << "FORTRAN API C++ gmcfreadfromfifoc_: check fifo size\n";
#endif
	 *fifo_empty = 	tileptr->transceiver->rx_fifo.has_packets() ? 0 : 1;

}

void gmcfsendpacketc_(
		int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* source, int* destination, int* packet_type, int* data_id, int* pre_post, int* timestamp,
		int64_t* data_sz, int64_t* data_ptr
		) {
	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfsendrequestpacketc_: send packet from "<< *source << " to " << *destination <<"\n";
	std::cout << "FORTRAN API C++ gmcfsendpacketc_: " << *data_ptr << "\n";
#endif
	SBA::Header_t ph ;
	if (*packet_type == P_TREQ or *packet_type == P_TRESP) {
		ph = SBA::mkHeader(*packet_type,*pre_post,0,1,*destination,*source,*timestamp, 1);
	} else {
		uint64_t timestamp_data_id = (((uint64_t)(*timestamp)) << 32) + (uint64_t)(*data_id);
		ph = SBA::mkHeader(*packet_type,*pre_post,0,1,*destination,*source,timestamp_data_id, *data_sz);
	}

	SBA::Packet_t request_packet = SBA::mkPacket_new(ph,*data_ptr);
#ifdef GMCF_DEBUG
#ifdef VERBOSE
	std::cout << "FORTRAN API C++ gmcfsendrequestpacketc_: " << SBA::ppPacket(request_packet);
#endif
	std::cout << "FORTRAN API C++ gmcfsendrequestpacketc_: Tile address (sanity): <" << tileptr->address <<">\n";
	std::cout << "FORTRAN API C++ gmcfsendrequestpacketc_: FIFO size:" <<tileptr->transceiver->tx_fifo.size()<<"\n";
#endif
	tileptr->transceiver->tx_fifo.push_back(request_packet);
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfsendrequestpacketc_: running TRX\n";
#endif
	tileptr->transceiver->run();
}

Packet_Fifo *getPacketFifo(int* packet_type, int* sender, SBA::Tile* tileptr) {
    Packet_Fifo *fifo;
    switch (*packet_type) {
	case P_DREQ:
	    fifo = &tileptr->service_manager.dreq_fifo_tbl[*sender];
	    break;
	case P_TREQ:
	    fifo = &tileptr->service_manager.treq_fifo_tbl[*sender];
	    break;
	case P_DRESP:
	    fifo = &tileptr->service_manager.dresp_fifo_tbl[*sender];
	    break;
	case P_TRESP:
	    fifo = &tileptr->service_manager.tresp_fifo_tbl[*sender];
	    break;
	case P_DACK:
	    fifo = &tileptr->service_manager.dack_fifo_tbl[*sender];
	    break;
    case P_RRDY:
        fifo = &tileptr->service_manager.regrdy_fifo_tbl[*sender];
        break; 
	default:
		cerr << "Only Data/Time Req/Resp supported\n";
	};
	return fifo;
}

Packet_Fifo_Table *getPacketFifoTable(int* packet_type, SBA::Tile* tileptr) {
    Packet_Fifo_Table *fifo;
    switch (*packet_type) {
	case P_DREQ:
	    fifo = &tileptr->service_manager.dreq_fifo_tbl;
	    break;
	case P_TREQ:
	    fifo = &tileptr->service_manager.treq_fifo_tbl;
	    break;
	case P_DRESP:
	    fifo = &tileptr->service_manager.dresp_fifo_tbl;
	    break;
	case P_TRESP:
	    fifo = &tileptr->service_manager.tresp_fifo_tbl;
	    break;
	case P_DACK:
	    fifo = &tileptr->service_manager.dack_fifo_tbl;
	    break;
    case P_RRDY:
        fifo = &tileptr->service_manager.regrdy_fifo_tbl;
        break; 
	default:
		cerr << "Only Data/Time Req/Resp supported\n";
	};
	return fifo;
}

void gmcfwaitforpacketsc_(
		int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* packet_type, int* sender, int* npackets
		) {
/*
call gmcfWaitFor(RESPDATA, 2)

is implemented as:
- block on RX FIFO
- demux
- stop when we have put n packets in queue x
*/

	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfwaitforpacketsc_: Tile address (sanity): <" << tileptr->address <<">\n";
#endif
    int pending_packets=*npackets;
    Packet_Fifo *alreadyReceived = getPacketFifo(packet_type, sender, tileptr);
     
    int packetsReceived = alreadyReceived->size();
    while(packetsReceived > 0 && pending_packets > 0) {
        SBA::Packet_t p = alreadyReceived->pop_front();
        if (SBA::getPacket_type(p) == *packet_type && SBA::getReturn_to(p) == *sender) {
            --pending_packets;
        }    
    	alreadyReceived->push_back(p);
        --packetsReceived;
	}
	while(pending_packets > 0) {
		tileptr->transceiver->rx_fifo.wait_for_packets();
		SBA::Packet_t  p = tileptr->transceiver->rx_fifo.pop_front();
		if (SBA::getPacket_type(p) == *packet_type && SBA::getReturn_to(p) == *sender) {
			--pending_packets;
		} else if (SBA::getPacket_type(p) == P_FIN && SBA::getReturn_to(p) == *sender) {
			pending_packets=0;
			break; // GR: We'll lose this P_FIN packet though? Is this okay?
		}
		tileptr->service_manager.demux_packets_by_type(p);
	}

}

// ! call gmcfshiftpendingc(sba_sys, sba_tile(model_id), packet_type, source, destination, timestamp, pre_post, data_id, data_sz, data_ptr, fifo_empty)
void gmcfshiftpendingc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* src_model_id,
		int* packet_type,
		int* source, int* destination, int* timestamp, int* pre_post, int* data_id, int64_t* data_sz, int64_t* data_ptr,
		int *fifo_empty
		) {
	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfshiftpendingc_: Tile address (sanity): <" << tileptr->address <<">\n";
#endif
	// this is defensive, so it's slow. And I don't know what to do if there is not packet ...
	// TODO: I should add a status in all calls to evaluate success!
	SBA::Packet_t  p;
	Packet_Fifo* fifo = getPacketFifo(packet_type, src_model_id, tileptr);
	if (fifo->size() > 0) {
	    p = fifo->shift();
	    *fifo_empty = 1 - fifo->size();
	}
	SBA::Header_t ph= SBA::getHeader(p);
	 *destination = (int)SBA::getTo(ph);
	 *source = (int)SBA::getReturn_to(ph);
	 if (*packet_type == P_TREQ or *packet_type == P_TRESP ) {
		 *timestamp = (int)getAck_to(ph);
		 *data_id =0;
	 } else {
		 uint64_t timestamp_data_id = getAck_to(ph);
		 *timestamp =  (int)((timestamp_data_id >> 32) & 0xFFFFFFFFUL);
		 *data_id = (int)(timestamp_data_id & 0xFFFFFFFFUL);
	 }

	 *pre_post = (int)getCtrl(ph);
	 *data_sz = (int64_t)SBA::getReturn_as(ph);
	 *data_ptr = (int64_t)SBA::getPayload_Word(p);

}

void gmcfpushpendingc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* packet_type,
		int* source, int* destination, int* timestamp, int* pre_post, int* data_id, int64_t* data_sz, int64_t* data_ptr
		) {

	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfpushpendingc_: Tile address (sanity): <" << tileptr->address <<">\n";
#endif
	uint64_t timestamp_data_id = *timestamp;
	if (*packet_type == P_DREQ or *packet_type == P_DRESP) {
		timestamp_data_id = (((uint64_t)(*timestamp)) << 32) + (uint64_t)(*data_id);
	}

	SBA::Header_t ph= SBA::mkHeader(*packet_type,*pre_post,0,1,*destination,*source,timestamp_data_id, *data_sz);
    SBA::Packet_t  p = SBA::mkPacket_new(ph,*data_ptr);
    Packet_Fifo* fifo = getPacketFifo(packet_type, source, tileptr);
    fifo->push(p);
}

void gmcfsendarrayc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* data_id, int* source, int* destination, int* pre_post, int* time, int64_t* sz1d,
		float* array
		) {
#ifdef GMCF_DEBUG
    float sum = 0.0;
    for (int i=0; i < *sz1d;  i++) {
        sum += array[i];
    }
    std::cout << "FORTRAN API C++ gmcfsendarrayc_: SANITY: " << sum << std::endl; 
#endif
	// So we take the float* array pointer and cast it to a uint64_t which we then pass as a pointer into gmcfsendpacketc_:
	void* fvp = (void*)array;
	int64_t fivp = (int64_t)fvp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfsendarrayc_: " << fivp <<" 0x"<<std::hex << fivp << " WAS " << array << std::dec<< "\n";
#endif
	int ptype = P_DRESP;
	gmcfsendpacketc_(
			ivp_sysptr, ivp_tileptr,
			source, destination, &ptype, data_id,pre_post,time,
			sz1d, &fivp
			);
}

void gmcffloatarrayfromptrc_(int64_t* ptr,float* array1d, int* sz) {
	int64_t ivp = *ptr;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcffloatarrayfromptrc_: ivp: " << ivp << " 0x" <<std::hex << ivp << std::dec<< "\n";
#endif
	void* vp=(void*)ivp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcffloatarrayfromptrc_: vp:" << vp <<"\n";
#endif
//	float* array1d = (float*)vp;
	float* tmp_array1d = (float*)vp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcffloatarrayfromptrc_: SZ:" << *sz <<"\n";
#endif
	// This is an expensive copy operation. I wish I could simply overwrite the pointer, but it does not work!
	// Maybe it would work if the variable was malloc'ed
	// But that is very intrusive: regular Fortran arrays are not malloc'ed
	// A slightly better way would be if we could specify exactly what to copy
	// To make that work within Fortran's limitations, it means we need to express this as an array
//	array1d = tmp_array1d;

#ifdef GMCF_DEBUG
	float sum=0.0;
#endif
/*
	for (int i =0;i< *sz;i++) {
		array1d[i]=tmp_array1d[i];
#ifdef GMCF_DEBUG
		sum+=array1d[i];
#endif
	}
*/
	 memcpy ( array1d, tmp_array1d, sizeof(float) * (*sz));
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcffloatarrayfromptrc_: SANITY:" << sum <<"\n";
  	std::cout << "FORTRAN API C++ gmcffloatarrayfromptrc_: " << tmp_array1d[0] <<"\n";
  	std::cout << "FORTRAN API C++ gmcffloatarrayfromptrc_: " << array1d[0] <<"\n";
#endif
	// So in C space, I can access array1d, but when it gets to Fortran, it segfaults.
}


void gmcfintegerarrayfromptrc_(int64_t* ptr,int* array1d, int* sz) {
	int64_t ivp = *ptr;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfintegerarrayfromptrc_: ivp: " << ivp << " 0x" <<std::hex << ivp << std::dec<< "\n";
#endif
	void* vp=(void*)ivp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfintegerarrayfromptrc_: vp:" << vp <<"\n";
#endif
//	float* array1d = (float*)vp;
	int* tmp_array1d = (int*)vp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfintegerarrayfromptrc_: SZ:" << *sz <<"\n";
#endif
	// This is an expensive copy operation. I wish I could simply overwrite the pointer, but it does not work!
	// Maybe it would work if the variable was malloc'ed
	// But that is very intrusive: regular Fortran arrays are not malloc'ed
	// A slightly better way would be if we could specify exactly what to copy
	// To make that work within Fortran's limitations, it means we need to express this as an array
//	array1d = tmp_array1d;
#ifdef GMCF_DEBUG
	int sum=0;
#endif
	/*
	for (int i =0;i< *sz;i++) {
		array1d[i]=tmp_array1d[i];
		sum+=array1d[i];
	}
	*/
	 memcpy ( array1d, tmp_array1d, sizeof(int) * (*sz) );
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfintegerarrayfromptrc_: SANITY:" << sum <<"\n";
  	std::cout << "FORTRAN API C++ gmcfintegerarrayfromptrc_: " << tmp_array1d[0] <<"\n";
  	std::cout << "FORTRAN API C++ gmcfintegerarrayfromptrc_: " << array1d[0] <<"\n";
#endif
	// So in C space, I can access array1d, but when it gets to Fortran, it segfaults.
}

void gmcfcheckfifoc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr, int* source, int* packet_type,  int* has_packets) {

	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfcheckfifoc_: Tile address (sanity): <" << tileptr->address <<">\n";
#endif
	*has_packets=0;
	Packet_Fifo* fifo = getPacketFifo(packet_type, source, tileptr);
	if (fifo->size() > 0) {
	    *has_packets = 1;
	}
}


void gmcfcheckfifosc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr, int* packet_type,  int* has_packets) {

	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfcheckfifoc_: Tile address (sanity): <" << tileptr->address <<">\n";
#endif
	*has_packets=0;
	Packet_Fifo_Table* fifo_table = getPacketFifoTable(packet_type, tileptr);
	for (auto iter : *fifo_table) {
	    if (iter.second.size() > 0) {
	        *has_packets = 1;
	        break;
	    }
	}
}

void gmcfgettileidc_(int64_t* ivp_tileptr, int* tile_id) {
	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
	*tile_id = tileptr->address;
}

void gmcfaddtosetc_(int64_t* ivp_tileptr, int* set_id, int* model_id, int* value_to_add) {
	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
	tileptr->incl_set_tbl.add(*set_id,*model_id,*value_to_add);
}
void gmcfremovefromsetc_(int64_t* ivp_tileptr, int* set_id, int* model_id) {
	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
	tileptr->incl_set_tbl.remove(*set_id,*model_id);
}
void gmcfsetisemptyc_(int64_t* ivp_tileptr, int* set_id, int* is_empty) {
	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
	*is_empty = tileptr->incl_set_tbl.size(*set_id)==0 ? 1 : 0;
}
void gmcfsetcontainsc_(int64_t* ivp_tileptr, int* set_id, int* model_id, int* contains) { // returns #entries for model_id
	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
	*contains = tileptr->incl_set_tbl.count(*set_id,*model_id);
}

void gmcfsetsizec_(int64_t* ivp_tileptr, int* set_id, int* set_size)  {
	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
	*set_size = tileptr->incl_set_tbl.size(*set_id);
}

void gmcfsettakefirstc_(int64_t* ivp_tileptr, int* set_id, int* model_id) {
    int64_t ivp = *ivp_tileptr;
    void* vp=(void*)ivp;
    SBA::Tile* tileptr = (SBA::Tile*)vp;
    *model_id = tileptr->incl_set_tbl.takefirst(*set_id);
}

void gmcfgetpthreadidc_(int64_t*id) {
    *id = (int64_t)pthread_self();
}

void gmcfwriteregc_(int64_t* ivp_sysptr, int model_id, int regno, void* word) {
	int64_t ivp = *ivp_sysptr;
	void* vp=(void*)ivp;
	SBA::System* sysptr = (SBA::System*)vp;
	//WV: I think this is way too late to lock the mutex. We probably need an explicit ugly gmcflockregc_ call to lock the mutex at the start of the time loop
//	pthread_mutex_lock(&(sysptr->reg_locks.at(*model_id)));
	sysptr->regs.at((model_id)*REGS_PER_THREAD+(regno))=*((uint64_t*)word);
	// WV: so maybe it is best to have this in a separate call as well, so that we can do several writes before we unlock
//	pthread_mutex_unlock(&(sysptr->reg_locks.at(*model_id)));
//	pthread_cond_broadcast(&(sysptr->reg_conds.at(*model_id)));
}

void gmcfreadregc_(int64_t* ivp_sysptr, int model_id, int regno, int64_t* word) {
	int64_t ivp = *ivp_sysptr;
	void* vp=(void*)ivp;
	SBA::System* sysptr = (SBA::System*)vp;
	*word = sysptr->regs.at((model_id)*REGS_PER_THREAD+(regno));
}

// WV:  Now this is ugly! I never wanted locks!
void gmcflockregc_(int64_t* ivp_sysptr, int model_id) {
	int64_t ivp = *ivp_sysptr;
	void* vp=(void*)ivp;
	SBA::System* sysptr = (SBA::System*)vp;
	pthread_mutex_lock(&(sysptr->reg_locks.at(model_id)));
}

// WV: Now this is ugly! I never wanted locks!
void gmcfunlockregc_(int64_t* ivp_sysptr, int model_id) {
	int64_t ivp = *ivp_sysptr;
	void* vp=(void*)ivp;
	SBA::System* sysptr = (SBA::System*)vp;
	pthread_mutex_unlock(&(sysptr->reg_locks.at(model_id)));
	pthread_cond_broadcast(&(sysptr->reg_conds.at(model_id)));
}

void gmcfwaitforregsc_(int64_t* ivp_sysptr,int64_t* ivp_tileptr,  int* model_id) {
	int64_t ivp = *ivp_sysptr;
	void* vp=(void*)ivp;
	SBA::System* sysptr = (SBA::System*)vp;
	int64_t ivp2 = *ivp_tileptr;
	void* vp2=(void*)ivp2;
	SBA::Tile* tileptr = (SBA::Tile*)vp2;
	if(tileptr->incl_set_tbl.size(P_RRDY)>0) { // no need for a while
	    const std::vector<unsigned int>* regreadys = tileptr->incl_set_tbl.elts(P_RRDY);
		for (auto _iter : *regreadys) {
			unsigned int src_model_id = _iter;
            if (src_model_id != (unsigned int)(*model_id)) {
                // Since we're going to call pthread_cond_wait we need to lock the appropriate mutex.
                pthread_mutex_lock(&(sysptr->reg_locks.at(src_model_id)));
                // So, this call unlocks the mutex, blocks, unblocks after a broadcast or signal, and locks the mutex
                // So if the first one blocks, nothing happens. When it unblocks, we remove it from the inclusion set
                // But this means we'll just remove them from the inclusion set in numerical order.
                pthread_cond_wait(&(sysptr->reg_conds.at(src_model_id)), &(sysptr->reg_locks.at(src_model_id)));
                // So there the mutex is still locked. There's no reason because we only want read access. so unlock it
                pthread_mutex_unlock(&(sysptr->reg_locks.at(src_model_id)));
                tileptr->incl_set_tbl.remove(P_RRDY,src_model_id);
            }
		} // So when we get here, it means all mutexes are unlocked, no more waiting, and inclusion set is empty.
		delete regreadys;
	}
}

pthread_spinlock_t globalOpSpinLock;
int stillToWrite = 0;
int stillToRead = 0;
float opResult = 0.0;
int globalSumTag = 15, globalMaxTag = 16, globalMinTag = 17;

void gmcfinitglobalopspinlockc_() {
    pthread_spin_init(&globalOpSpinLock, PTHREAD_PROCESS_SHARED);
}

void gmcflockglobalopspinlockc_() {
    pthread_spin_lock(&globalOpSpinLock);
}

void gmcfunlockglobalopspinlockc_() {
    pthread_spin_unlock(&globalOpSpinLock);
}

void reduce(float *value, int *tag) {
    pthread_spin_lock(&globalOpSpinLock);
    if (*tag == globalSumTag) {
        opResult += *value;
    } else if (*tag == globalMaxTag) {
        if (*value > opResult) {
            opResult = *value;
        }
    } else if (*tag == globalMinTag) {
        if (*value < opResult) {
            opResult = *value;
        }
    }
    stillToWrite--;
    pthread_spin_unlock(&globalOpSpinLock);
    while (stillToWrite != 0) {
        __asm__ __volatile__ ("" ::: "memory");
    }
    *value = opResult;
}

void opAsMaster(float *value, int *tag, int *size) {
    while (stillToRead != 0) {
        __asm__ __volatile__ ("" ::: "memory");
    }
    pthread_spin_lock(&globalOpSpinLock);
    stillToWrite = *size;
    stillToRead = *size;
    opResult = 0;
    pthread_spin_unlock(&globalOpSpinLock);
    reduce(value, tag);
}

void opAsNonMaster(float *value, int *tag) {
    while (stillToWrite == 0) {
        __asm__ __volatile__ ("" ::: "memory");
    }
    reduce(value, tag);
}

void gmcfdoopc_(int *id, float *value, int *tag, int *size) {
    (*id == 1) ? opAsMaster(value, tag, size) : opAsNonMaster(value, tag);
    pthread_spin_lock(&globalOpSpinLock);
    stillToRead--;
    pthread_spin_unlock(&globalOpSpinLock);
}

