
void boundp1c_ (__global float *p,int im,int jm,int km) ;
void boundp2c_ (__global float *p,int im,int jm,int km) ;

// ====================================================== KERNEL ===========================================================

void press_sor_kernel (
#ifdef COMBINED_KERNEL
__local float* sor_chunks,
__local float* tmp_chunks,
#endif

		__global float4* uvw,
        __global float* p,
//#ifdef P_SCRATCH
        __global float* p_scratch,
//#endif
        __global float *rhs,
		__global float *cn1,__global float *cn2l,__global float *cn2s,__global float *cn3l,__global float *cn3s,__global float *cn4l,__global float *cn4s,
		__global float *chunks_num,
		__global float *chunks_denom,
		__global float *val_ptr,
		__global unsigned int *nrd_ptr,
		  const unsigned int im,
		  const unsigned int jm,
		  const unsigned int km
        );
