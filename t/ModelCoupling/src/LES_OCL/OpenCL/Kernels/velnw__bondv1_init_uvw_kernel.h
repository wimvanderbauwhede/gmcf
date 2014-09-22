void velnw__bondv1_init_uvw_kernel (
    __global float2 * p,
    __global float4 * uvw,
    __global float4 * fgh, 
    __global float * dxs, 
    __global float * dys, 
    __global float * dzs, 
    __global float * dzn,
#ifndef EXTERNAL_WIND_PROFILE
    __global float * z2,
#else
    __global float * wind_profile,
#endif
    __global unsigned int* n_ptr,
    const unsigned int im, 
    const unsigned int jm, 
    const unsigned int km,
    const float dt
        ) ;
