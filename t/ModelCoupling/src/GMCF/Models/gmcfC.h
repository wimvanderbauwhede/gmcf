#ifndef _GMCF_C_H_
#define _GMCF_C_H_
typedef int64_t* System;
typedef int64_t* Tile;

// The minimal API is as follows:
void gmcfreadfromfifoc_(System s, Tile t, int* source, int* destination, int* packet_type, int* timestamp, int* pre_post, int* data_id, int64_t* data_ptr, int *fifo_empty);

#endif // _GMCF_C_H_
