
void les_calc_visc__adam_kernel (
		__global float4 *fgh,
		__global float4 *fgh_old,
		__global float *dx1,__global float *dy1,__global float *dzn,
		__global float16 *diu,
		__global float *sm,
		const unsigned int im,
		const unsigned int jm,
		const unsigned int km
);
