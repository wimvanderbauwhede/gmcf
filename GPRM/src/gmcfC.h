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

void gmcfshiftpendingc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr, int* src_model_id,
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

void gmcfcheckfifoc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,int* packet_type,int* source, int* has_packets);

void gmcfcheckfifosc_(int64_t* ivp_sysptr, int64_t* ivp_tileptr,int* packet_type, int* has_packets);

void gmcfgettileidc_(int64_t* ivp_tileptr, int* tile_id);

void gmcfaddtosetc_(int64_t* ivp_tileptr, int* set_id, int* model_id);
void gmcfremovefromsetc_(int64_t* ivp_tileptr, int* set_id, int* model_id);
void gmcfsetisemptyc_(int64_t* ivp_tileptr, int* set_id, int* is_empty);
void gmcfsetcontainsc_(int64_t* ivp_tileptr, int* set_id, int* model_id, int* contains); // returns #entries for model_id
void gmcfsetsizec_(int64_t* ivp_tileptr, int* set_id, int* set_size);

void gmcfgetpthreadidc_(int64_t*id);

void gmcfwriteregc_(int64_t* ivp_sysptr, int* model_id, int* regno, int64_t* word);
void gmcfreadregc_(int64_t* ivp_sysptr, int* model_id, int* regno, int64_t* word);

void gmcflockregc_(int64_t* ivp_sysptr, int* model_id);
void gmcfunlockregc_(int64_t* ivp_sysptr, int* model_id);

void gmcfwaitforregsc_(int64_t* ivp_sysptr,int64_t* ivp_tileptr,  int* model_id);

#endif // _GMCF_C_H_
