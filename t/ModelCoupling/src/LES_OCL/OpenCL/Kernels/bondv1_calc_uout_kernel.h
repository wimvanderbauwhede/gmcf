void bondv1_calc_uout_kernel (
#ifdef COMBINED_KERNEL
		__local float* aaat,
		__local float* bbbt,
#endif
    __global float4 * uvw,
    __global float * uout_ptr,
    __global float * aaa_chunks,
    __global float * bbb_chunks,
    const unsigned int im,
    const unsigned int jm,
    const unsigned int km
);
