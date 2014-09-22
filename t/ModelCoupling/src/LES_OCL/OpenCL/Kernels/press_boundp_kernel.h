
void boundp12c_(__global float2 *p2,const unsigned int im,const unsigned int jm,const unsigned int km);
void boundp_new (__global float2 *p2,const unsigned int im,const unsigned int jm,const unsigned int km,unsigned int idx_g,unsigned int idx_l);
// ====================================================== KERNEL ===========================================================
void press_boundp_kernel (
		__global float2* p2,
        const unsigned int im,
        const unsigned int jm,
        const unsigned int km
        );
