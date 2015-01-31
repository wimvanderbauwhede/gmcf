#include "gmcfF.h"
#include "SBA/Types.h"
#include "SBA/System.h"
#include "SBA/Tile.h"
#include "SBA/Packet.h"
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
    Packet_Fifo alreadyReceived;
    switch (*packet_type) {
	case P_DREQ:
	    alreadyReceived = tileptr->service_manager.dreq_fifo;
	    break;
	case P_TREQ:
	    alreadyReceived = tileptr->service_manager.treq_fifo;
	    break;
	case P_DRESP:
	    alreadyReceived = tileptr->service_manager.dresp_fifo;
	    break;
	case P_TRESP:
	    alreadyReceived = tileptr->service_manager.tresp_fifo;
	    break;
	}
     
    int packetsReceived = alreadyReceived.size();
    int i=0;
    while(i < packetsReceived && pending_packets > 0) {
        SBA::Packet_t p = alreadyReceived.pop_front();
        if (SBA::getPacket_type(p) == *packet_type && SBA::getReturn_to(p) == *sender) {
            --pending_packets;
        }    
    	alreadyReceived.push_back(p);
        i++;
	}
	while(pending_packets > 0) {
		tileptr->transceiver->rx_fifo.wait_for_packets();
		SBA::Packet_t  p = tileptr->transceiver->rx_fifo.pop_front();
		if (SBA::getPacket_type(p) == *packet_type && SBA::getReturn_to(p) == *sender) {
			--pending_packets;
		} else if (SBA::getPacket_type(p) == P_FIN && SBA::getReturn_to(p) == *sender) {
			pending_packets=0;
			break; // GR: We'll lose this P_FIN packet though? Is this okay?
		} else {
		    std::cout << tileptr->address << " received an unexpected packet from " << SBA::getReturn_to(p) << " it is of type " << SBA::getPacket_type(p) << std::endl;
		}
		tileptr->service_manager.demux_packets_by_type(p);
	}

}

// ! call gmcfshiftpendingc(sba_sys, sba_tile(model_id), packet_type, source, destination, timestamp, pre_post, data_id, data_sz, data_ptr, fifo_empty)
void gmcfshiftpendingc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,
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
	switch (*packet_type) {
	case P_DREQ:
		if (tileptr->service_manager.dreq_fifo.size()>0) {
		p = tileptr->service_manager.dreq_fifo.shift();
		*fifo_empty = 1 - tileptr->service_manager.dreq_fifo.size();
		}
		break;
	case P_TREQ:
		if (tileptr->service_manager.treq_fifo.size()>0) {
		p = tileptr->service_manager.treq_fifo.shift();
		*fifo_empty = 1 - tileptr->service_manager.treq_fifo.size();
		}
		break;
	case P_DRESP:
		if (tileptr->service_manager.dresp_fifo.size()>0) {
		p = tileptr->service_manager.dresp_fifo.shift();
		*fifo_empty = 1 - tileptr->service_manager.dresp_fifo.size();
		}
		break;
	case P_TRESP:
		if (tileptr->service_manager.tresp_fifo.size()>0) {
		p = tileptr->service_manager.tresp_fifo.shift();
		*fifo_empty = 1 - tileptr->service_manager.tresp_fifo.size();
		}
		break;
	default:
		cerr << "Only Data/Time Req/Resp supported\n";
	};
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

	switch (*packet_type) {
	case P_DREQ:
		if (tileptr->service_manager.dreq_fifo.size()>0) {
			tileptr->service_manager.dreq_fifo.push(p);
		}
		break;
	case P_TREQ:
		if (tileptr->service_manager.treq_fifo.size()>0) {
			tileptr->service_manager.treq_fifo.push(p);
		}
		break;
	case P_DRESP:
		if (tileptr->service_manager.dresp_fifo.size()>0) {
			tileptr->service_manager.dresp_fifo.push(p);
		}
		break;
	case P_TRESP:
		if (tileptr->service_manager.tresp_fifo.size()>0) {
			tileptr->service_manager.tresp_fifo.push(p);
		}
		break;
	default:
		cerr << "Only Data/Time Req/Resp supported\n";
	};

}



void gmcfsendarrayc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* data_id, int* source, int* destination, int* pre_post, int* time, int64_t* sz1d,
		float* array
		) {
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
	float sum=0.0;
	for (int i =0;i< *sz;i++) {
		array1d[i]=tmp_array1d[i];
		sum+=array1d[i];
	}

#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcffloatarrayfromptrc_: SANITY:" << sum <<"\n";
//	std::cout << "FORTRAN API C++ gmcffloatarrayfromptrc_: " << tmp_array1d[0] <<"\n";
//	std::cout << "FORTRAN API C++ gmcffloatarrayfromptrc_: " << array1d[0] <<"\n";
#endif
	// So in C space, I can access array1d, but when it gets to Fortran, it segfaults.
}

void gmcfcheckfifoc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,int* packet_type, int* has_packets) {

	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
#ifdef GMCF_DEBUG
	std::cout << "FORTRAN API C++ gmcfcheckfifoc_: Tile address (sanity): <" << tileptr->address <<">\n";
#endif
	*has_packets=0;
	switch (*packet_type) {
	case P_DREQ:
		if (tileptr->service_manager.dreq_fifo.size()>0) {
			*has_packets=1;
		}
		break;
	case P_TREQ:
		if (tileptr->service_manager.treq_fifo.size()>0) {
			*has_packets=1;
		}
		break;
	case P_DRESP:
		if (tileptr->service_manager.dresp_fifo.size()>0) {
			*has_packets=1;
		}
		break;
	case P_TRESP:
		if (tileptr->service_manager.tresp_fifo.size()>0) {
			*has_packets=1;
		}
		break;
	default:
		cerr << "Only Data/Time Req/Resp supported\n";
	};
}

void gmcfgettileidc_(int64_t* ivp_tileptr, int* tile_id) {
	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
	*tile_id = tileptr->address;
}
