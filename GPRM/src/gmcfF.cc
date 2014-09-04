#include "gmcfF.h"
#include "SBA/Types.h"
#include "SBA/System.h"
#include "SBA/Tile.h"
#include "SBA/Packet.h"

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
	std::cout << "gmcfreadfromfifoc_: Tile address (sanity): <" << tileptr->address <<">\n";
	std::cout << "gmcfreadfromfifoc_: wait_for_packets\n";

	tileptr->transceiver->rx_fifo.wait_for_packets();

	// Now we know there is a packet, so get it.
	std::cout << "gmcfreadfromfifoc_: get packet\n";

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
	 std::cout << "gmcfreadfromfifoc_: check fifo size\n";
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
	std::cout << "gmcfsendrequestpacketc_: send packet from "<< *source << " to " << *destination <<"\n";
	SBA::Header_t ph ;
	if (*packet_type == P_TREQ or *packet_type == P_TRESP) {
		ph = SBA::mkHeader(*packet_type,*pre_post,0,1,*destination,*source,*timestamp, 1);
	} else {
		uint64_t timestamp_data_id = (((uint64_t)(*timestamp)) << 32) + (uint64_t)(*data_id);
		ph = SBA::mkHeader(*packet_type,*pre_post,0,1,*destination,*source,timestamp_data_id, *data_sz);
	}

	SBA::Packet_t request_packet = SBA::mkPacket_new(ph,*data_ptr);
#ifdef VERBOSE
	std::cout << "gmcfsendrequestpacketc_: " << SBA::ppPacket(request_packet);
#endif
	std::cout << "gmcfsendrequestpacketc_: Tile address (sanity): <" << tileptr->address <<">\n";
	std::cout << "gmcfsendrequestpacketc_: FIFO size:" <<tileptr->transceiver->tx_fifo.size()<<"\n";
	tileptr->transceiver->tx_fifo.push_back(request_packet);
	std::cout << "gmcfsendrequestpacketc_: running TRX\n";
	tileptr->transceiver->run();
}

void gmcfwaitforpacketsc_(
		int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* packet_type, int* npackets
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
	std::cout << "gmcfwaitforpacketsc_: Tile address (sanity): <" << tileptr->address <<">\n";
	int pending_packets=*npackets;
	while(pending_packets!=0) {
		tileptr->transceiver->rx_fifo.wait_for_packets();
		SBA::Packet_t  p = tileptr->transceiver->rx_fifo.pop_front();
		if (SBA::getPacket_type(p) == *packet_type) {
			--pending_packets;
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
	std::cout << "gmcfshiftpendingc_: Tile address (sanity): <" << tileptr->address <<">\n";
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
	std::cout << "gmcfpushpendingc_: Tile address (sanity): <" << tileptr->address <<">\n";

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
	int ptype = P_DRESP;
//	int64_t* ivp_sysptr, int64_t* ivp_tileptr,
//	int* source, int* destination, int* packet_type, int* data_id, int* timestamp,
//	int64_t* data_sz, int64_t* data_ptr
	gmcfsendpacketc_(
			ivp_sysptr, ivp_tileptr,
			source, destination, &ptype, data_id,pre_post,time,
			sz1d, &fivp
			);
}

void gmcffloatarrayfromptrc_(int64_t* ptr,float* array1d) {
	int64_t ivp = *ptr;
	void* vp=(void*)ivp;
	array1d = (float*)vp;
}

void gmcfcheckfifoc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,int* packet_type, int* has_packets) {

	int64_t ivp = *ivp_tileptr;
	void* vp=(void*)ivp;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
	std::cout << "gmcfcheckfifoc_: Tile address (sanity): <" << tileptr->address <<">\n";
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
