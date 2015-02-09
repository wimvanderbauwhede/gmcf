#ifndef _GMCF_C_H_
#define _GMCF_C_H_

#define REQDATA 4
#define REQTIME 6
#define RESPDATA 5
#define RESPTIME 7

// The minimal API is as follows:
void gmcfreadfromfifoc_(
		int64_t* s, int64_t* t,
		int* source, int* destination, int* packet_type, int* timestamp, int* pre_post, int* data_id, int* data_sz, int64_t* data_ptr,
		int *fifo_empty);

void gmcfsendpacketc_(
		int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* source, int* destination, int* packet_type, int* data_id, int* pre_post, int* timestamp,
		int64_t* data_sz,int64_t* data_ptr
		);

void gmcfwaitforpacketsc_(
		int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* packet_type, int* sender, int* npackets
		);

void gmcfshiftpendingc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* packet_type,
		int* source, int* destination, int* timestamp, int* pre_post, int* data_id, int64_t* data_sz, int64_t* data_ptr,
		int *fifo_empty
		);

void gmcfpushpendingc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* packet_type,
		int* source, int* destination, int* timestamp, int* pre_post, int* data_id, int64_t* data_sz, int64_t* data_ptr
		);

void gmcfsendarrayc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* data_id, int* source, int* destination, int* pre_post, int* time, int64_t* sz1d,
		float* array
		);

void gmcffloatarrayfromptrc_(int64_t* ptr,float* array1d, int* sz);

void gmcfintegerarrayfromptrc_(int64_t* ptr,int* array1d, int* sz);

void gmcfcheckfifoc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,int* packet_type, int* has_packets);

void gmcfgettileidc_(int64_t* ivp_tileptr, int* tile_id);

#endif // _GMCF_C_H_
