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
void gmcfreadfromfifoc_(
		int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* source, int* destination, int* packet_type, int* timestamp, int* pre_post, int* data_id, int64_t* data_ptr,
		int *fifo_empty
) {
// So, we need to read from the FIFO. In fact, not as easy as it seems: the FIFO read operation is built into the Tile, and blocks.

	void* vp=(void*)ivp_tileptr;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
	tileptr->transceiver->rx_fifo.wait_for_packets();
	// Now we know there is a packet, so get it.
	SBA::Packet_t  p = tileptr->transceiver->rx_fifo.pop_front();
	SBA::Header_t ph= SBA::getHeader(p);
	 *source = (int)SBA::getTo(ph);
	 *destination = (int)SBA::getReturn_to(ph);
	 *packet_type = (int)SBA::getPacket_type(ph);
	 *timestamp = (int)getAck_to(ph);
	 *pre_post = (int)getCtrl(ph);
	 *data_id = (int)SBA::getReturn_as(ph);
	 *data_ptr = (int64_t)SBA::getPayload_Word(p);
	 *fifo_empty = 	tileptr->transceiver->rx_fifo.has_packets() ? 0 : 1;

}

void gmcfsendrequestpacketc_(
		int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* source, int* destination, int* packet_type, int* timestamp
		) {

	void* vp=(void*)ivp_tileptr;
	SBA::Tile* tileptr = (SBA::Tile*)vp;
	SBA::Header_t ph = SBA::mkHeader(*packet_type,0,0,0,*destination,*source,*timestamp,0);
	SBA::Packet_t request_packet = SBA::mkPacket_new(ph,0);
	tileptr->transceiver->tx_fifo.push_back(request_packet);
}
