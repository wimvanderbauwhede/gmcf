/*
 * shared_macros.h
 * These are macros shared by all kernels, not by kernels ann host
 * Macros shared by kernels and host are passed in on the kernel build command line
 *  Created on: 15 Jul 2014
 *      Author: wim
 */

#ifndef SHARED_MACROS_H_
#define SHARED_MACROS_H_

#define __CL_ENABLE_EXCEPTIONS

#ifdef __CDT_PARSER__
#include "OpenCLEclipseCompat.hpp"
#endif

// GPU kernel is the default, can be set on scons command line with kernel=...
#define GPU_KERNEL 1
#define CPU_KERNEL 2
#define ORIG_KERNEL 3


#ifndef EXT_DEFS
#define ICAL 0
#define LOOP_ORDER 1
#define NTH 1
#define KERNEL CPU_KERNEL
#endif

#define N_ELTS_TH 256

// Physical property set
#define RO 1.1763
#define VN 1.583E-5
// IBM parameter set (Feedback force by Goldstein)
#define ALPHA -10.0
#define BETA -1.0

#ifndef COMBINED_KERNEL
#define __SUBKERNEL __kernel
#else
#define __SUBKERNEL
#endif
inline float SQR(float x) {return x*x; } 
inline float sqr(float x) {return x*x; } 

#endif /* SHARED_MACROS_H_ */
