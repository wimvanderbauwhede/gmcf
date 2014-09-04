#ifndef _GMCF_C_H_
#define _GMCF_C_H_



#define REQDATA 4
#define REQTIME 6
#define RESPDATA 5
#define RESPTIME 7

// The minimal API is as follows:
void gmcfreadfromfifoc_(
		int64_t* s, int64_t* t,
		int* source, int* destination, int* packet_type, int* timestamp, int* pre_post, int* data_id, int64_t* data_sz, int64_t* data_ptr,
		int *fifo_empty);

void gmcfsendpacketc_(
		int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* source, int* destination, int* packet_type, int* data_id, int* pre_post, int* timestamp,
		int64_t* data_sz,int64_t* data_ptr
		);

void gmcfwaitforpacketsc_(
		int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* packet_type, int* npackets
		);
void gmcfshiftpendingc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* packet_type,
		int* source, int* destination, int* timestamp, int* pre_post, int* data_id, int64_t* data_sz, int64_t* data_ptr,
		int *fifo_empty
		);

void gmcfsendarrayc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,
		int* source, int* destination, int* time, int64_t* sz1d,
		float* array
		);

void gmcffloatarrayfromptrc_(int64_t* ptr,float* array1d);

void gmcfcheckfifoc(int64_t* ivp_sysptr, int64_t* ivp_tileptr,int* packet_type, int* has_packets);

#endif // _GMCF_C_H_
