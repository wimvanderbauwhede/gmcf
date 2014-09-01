#ifndef _GMCF_C_H_
#define _GMCF_C_H_



#define REQDATA 4
#define REQTIME 6
#define RESPDATA 5
#define RESPTIME 7

// The minimal API is as follows:
void gmcfreadfromfifoc_(
		int64_t* s, int64_t* t,
		int* source, int* destination, int* packet_type, int* timestamp, int* pre_post, int* data_id, int64_t* data_ptr,
		int *fifo_empty);

#endif // _GMCF_C_H_
