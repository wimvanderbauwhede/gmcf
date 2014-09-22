float calc_reltmp0(
        __global float2* p_scratch,
        __global float* rhs,
        float rhsav,
        const __global float *cn1,
        const __global float *cn2l,const __global float *cn2s,
        const __global float *cn3l,
        const __global float *cn3s,
        const __global float *cn4l,
        const __global float *cn4s,
        unsigned int i, unsigned int j, unsigned int k,
        const unsigned int ip,
        const unsigned int jp,
        const unsigned int kp
        ) ;
float calc_reltmp1(
        __global float2* p_scratch,
        __global float* rhs,
        float rhsav,
        const __global float *cn1,
        const __global float *cn2l,const __global float *cn2s,
        const __global float *cn3l,
        const __global float *cn3s,
        const __global float *cn4l,
        const __global float *cn4s,
        unsigned int i, unsigned int j, unsigned int k,
        const unsigned int ip,
        const unsigned int jp,
        const unsigned int kp
        ) ;
float calc_reltmp_mp_rb(
		__global float2* p_scratch,
        __global float* rhs,
        float rhsav,
        const __global float *cn1,const __global float *cn2l,const __global float *cn2s,const __global float *cn3l,const __global float *cn3s,const __global float *cn4l,const __global float *cn4s,
        unsigned int i, unsigned int j, unsigned int k,
        unsigned int j_lhs,unsigned int k_lhs,
        unsigned int nrd,
        const unsigned int ip,
        const unsigned int jp,
        const unsigned int kp
        ) ;
float calc_reltmp_mp_db(
		__global float2* p_scratch,
        __global float* rhs,
        float rhsav,
        const __global float *cn1,const __global float *cn2l,const __global float *cn2s,const __global float *cn3l,const __global float *cn3s,const __global float *cn4l,const __global float *cn4s,
        unsigned int i, unsigned int j, unsigned int k,
        unsigned int j_lhs,unsigned int k_lhs,
        unsigned int nrd,
        const unsigned int ip,
        const unsigned int jp,
        const unsigned int kp
        ) ;
// ====================================================== KERNEL ===========================================================
void press_sor_kernel (
#ifdef COMBINED_KERNEL
        __local float* sor_chunks,
//        __local float* tmp_chunks,
#endif
        __global float4* uvw,
//        __global float* p,
        __global float2* p_scratch,
        __global float *rhs,
        const __global float *cn1,const __global float *cn2l,const __global float *cn2s,const __global float *cn3l,const __global float *cn3s,const __global float *cn4l,const __global float *cn4s,
        __global float *chunks_num,
        __global float *chunks_denom,
        __global float *val_ptr,
        __global unsigned int *nrd_ptr,
        const unsigned int im,
        const unsigned int jm,
        const unsigned int km
        );


