#include "shared_macros.h"
#include "../calc_array_index.h"
#include "../calc_loop_iters.h"
#define NEW_BOUNDP

#ifndef COMBINED_KERNEL
void boundp12c_(__global float2 *p2,const unsigned int im,const unsigned int jm,const unsigned int km);
void boundp_new (__global float2 *p2,const unsigned int im,const unsigned int jm,const unsigned int km,unsigned int idx_g,unsigned int idx_l);
#endif
// ====================================================== KERNEL ===========================================================
__SUBKERNEL void press_boundp_kernel (
		__global float2* p,
        const unsigned int im,
        const unsigned int jm,
        const unsigned int km
        ) {

	const int ip = im;
	const int jp = jm;
	const int kp = km;

#if KERNEL == GPU_KERNEL

	unsigned int idx_g = get_group_id(0);
	unsigned int idx_l = get_local_id(0);

	boundp_new(p, im, jm, km, idx_g, idx_l);

#else
#ifndef NEW_BOUNDP
	if(get_global_id(0)==0) {
/*
So currently this is single-threaded, but I guess this is actually no big deal.
Still, suppose we want to parallelise it, then we need to parallelise the
nested loops of course.

What we should do I guess is simply take the same approach as for bondv1
*/
#if KERNEL == ORIG_KERNEL
		boundp12c_(p,im,jm,km);
#else

		unsigned int max_ij = (im > jm) ? im : jm ;
		unsigned int max_ijk = (km > max_ij) ? km : max_ij ;
		unsigned int max_idx = max_ijk+2;
		for (unsigned int idx_g = 0; idx_g<max_idx;idx_g++) {
			for (unsigned int idx_l = 0; idx_l<max_idx;idx_l++) {
				boundp_new(p,im,jm,km,idx_g,idx_l);
			}
		}
#endif
	}
#else // NEW_BOUNDP
    // New approach: global range = (jm+2)*(km+2) + (km+2)*(im+2)+(jm+2)*(im+2)
    unsigned int gl_id = get_global_id(0);

    if (gl_id < (jm+2)*(km+2) ) {
    	unsigned int k = gl_id / (jm+2) ;
    	unsigned int j = gl_id % (jm+2) ;
      //p(   0,j,k) = p(1 ,j,k)
      p[FTNREF3D0(0, j, k, ip + 3, jp + 3)] = p[FTNREF3D0(1, j, k, ip + 3, jp + 3)];  // SEGFAULTS
//      //p(im+1,j,k) = p(im,j,k)
      p[FTNREF3D0(im+1, j, k, ip + 3, jp + 3)] = p[FTNREF3D0(im, j, k, ip + 3, jp + 3)];  // SEGFAULTS
    } else if (gl_id <(jm+2)*(km+2) + (km+2)*(im+2)) {
  	  unsigned int k = (gl_id -(jm+2)*(km+2)) / (im+2) ;
  	  unsigned int i = (gl_id -(jm+2)*(km+2)) % (im+2);
      //p(i,   0,k) = p(i,jm,k)
      p[FTNREF3D0(i,0, k, ip + 3, jp + 3)] = p[FTNREF3D0(i, jm, k, ip + 3, jp + 3)];
//      //p(i,jm+1,k) = p(i, 1,k)
      p[FTNREF3D0(i,jm+1, k, ip + 3, jp + 3)] = p[FTNREF3D0(i, 1, k, ip + 3, jp + 3)];

    } else if (gl_id < (jm+2)*(km+2) + (km+2)*(im+2) + (jm+2)*(im+2)) {
  	  unsigned int j = (gl_id -(jm+2)*(km+2)-(km+2)*(im+2)) / (im+2) ;
  	  unsigned int i = (gl_id -(jm+2)*(km+2)-(km+2)*(im+2)) % (im+2) ;
      //p(i,j,   0) = p(i,j,1)
      p[FTNREF3D0(i, j, 0, ip + 3, jp + 3)] = p[FTNREF3D0(i, j, 1, ip + 3, jp + 3)]; // SEGFAULTS
      //p(i,j,km+1) = p(i,j,km)
      p[FTNREF3D0(i, j, km+1, ip + 3, jp + 3)] = p[FTNREF3D0(i, j, km, ip + 3, jp + 3)];
    }


#endif
#endif
} // END of press_boundp_kernel()
// ============================================================== ==============================================================
/*
void boundpc_(__global float *p, const int im, const int jm, const int km,
		int i, int j, int k) {

	const int ip = im;
	const int jp = jm;
	const int kp = km;

//! --computational boundary(neumann condition) vertical
	if (k == 0) { // rather than doing this for all k.
		// p[k=0]=p[k=1] so extend one down
		p[FTNREF3D0(i, j, 0, ip + 3, jp + 3)] = p[FTNREF3D0(i, j, 1, ip + 3,
				jp + 3)];
		// p[k=km+1]=p[k=km] so extend one up
		p[FTNREF3D0(i, j, km + 1, ip + 3, jp + 3)] = p[FTNREF3D0(i, j, km,
				ip + 3, jp + 3)];
	}
//! --computational boundary(neumann condition) horizontal
	if (i == 0) {
		// p[i=0]=p[i=1] so extend one left/west
		p[FTNREF3D0(0, j, k, ip + 3, jp + 3)] = p[FTNREF3D0(1, j, k, ip + 3,
				jp + 3)];
		// p[i=im+1]=p[i=im] so extend one right/east
		p[FTNREF3D0(im + 1, j, k, ip + 3, jp + 3)] = p[FTNREF3D0(im, j, k,
				ip + 3, jp + 3)];
	}
	if (j == 0) {
		// p[j=0]=p[j=1] so extend one south
		p[FTNREF3D0(i, 0, k, ip + 3, jp + 3)] = p[FTNREF3D0(i, jm, k, ip + 3,
				jp + 3)];
		// p[j=jm+1]=p[j=jm] so extend one north
		p[FTNREF3D0(i, jm + 1, k, ip + 3, jp + 3)] = p[FTNREF3D0(i, 1, k,
				ip + 3, jp + 3)];
	}

}


void boundp2c_ (__global float *p,const unsigned int im,const unsigned int jm,const unsigned int km) {

  const unsigned int ip = im;
  const unsigned int jp = jm;
  const unsigned int kp = km;

  int j;
  int i;
//!
//! --computational boundary(neumann condition)
  for (unsigned int j=0;j<=jm + 1;j++) {
    for (unsigned int i=0;i<=im + 1;i++) {
      p[FTNREF3D0(i,j,0,ip+3,jp+3)] = p[FTNREF3D0(i,j,1,ip+3,jp+3)];
      p[FTNREF3D0(i,j,km + 1,ip+3,jp+3)] = p[FTNREF3D0(i,j,km,ip+3,jp+3)];
    }
  }
//!

  return;
}

void boundp1c_ (__global float *p,const unsigned int im,const unsigned int jm,const unsigned int km) {

	const unsigned int ip = im;
	const unsigned int jp = jm;
	const unsigned int kp = km;

//!
//! --computational boundary(neumann condition)
  for (unsigned int k=0;k<=km + 1;k++) {
    for (unsigned int j=0;j<=jm + 1;j++) {
      p[FTNREF3D0(0,j,k,ip+3,jp+3)] = p[FTNREF3D0(1,j,k,ip+3,jp+3)];
      p[FTNREF3D0(im + 1,j,k,ip+3,jp+3)] = p[FTNREF3D0(im,j,k,ip+3,jp+3)];
    }
  }
  for (unsigned int k=0;k<=km + 1;k++) {
    for (unsigned int i=0;i<=im + 1;i++) {
      p[FTNREF3D0(i,0,k,ip+3,jp+3)] = p[FTNREF3D0(i,jm,k,ip+3,jp+3)];
      p[FTNREF3D0(i,jm + 1,k,ip+3,jp+3)] = p[FTNREF3D0(i,1,k,ip+3,jp+3)];
    }
  }

  return;
}
*/


/*
 So, what happens here is that, in this particular order,
- first for all j and k, which I consider the "side planes (W & E) of the cube, the values are extended i=1 -> i=0 and i=im->i=im+1
- then for all i and k, which I consider the "front and back planes (S & N) of the cube, the values are extended j=1 -> j=0 and j=jm->j=jm+1
- then for all i and j, which I consider the "top and bottom planes of the cube, the values are extended k=1 -> k=0 and k=km->k=km+1


p[FTNREF3D0(0,jm+1,k,ip+3,jp+3)]=p[FTNREF3D0(1,1,k,ip+3,jp+3)];
p[FTNREF3D0(1,jm+1,k,ip+3,jp+3)]=p[FTNREF3D0(1,1,k,ip+3,jp+3)];

p[FTNREF3D0(im,jm+1,ip+3,jp+3)]=p[FTNREF3D0(im,1,k,ip+3,jp+3)];
p[FTNREF3D0(im+1,jm+1,ip+3,jp+3)]=p[FTNREF3D0(im,1,k,ip+3,jp+3)];

p[FTNREF3D0(0,0,k,ip+3,jp+3)]=p[FTNREF3D0(1,jm,k,ip+3,jp+3)];
p[FTNREF3D0(1,0,k,ip+3,jp+3)]=p[FTNREF3D0(1,jm,k,ip+3,jp+3)];

p[FTNREF3D0(im,0,k,ip+3,jp+3)]=p[FTNREF3D0(im,jm,k,ip+3,jp+3)];
p[FTNREF3D0(im+1,0,k,ip+3,jp+3)]=p[FTNREF3D0(im,jm,k,ip+3,jp+3)];




 * */

void boundp12c_ (__global float2 *p,const unsigned int im,const unsigned int jm,const unsigned int km) {

	const unsigned int ip = im;
	const unsigned int jp = jm;
	const unsigned int kp = km;

//!
//! --computational boundary(neumann condition)
	// 0,0,0 .. 0, jm+1, km+1
	// im+1,0,0 .. im+1, jm+1, km+1
	for (unsigned int k = 0; k <= km + 1; k++) {
		for (unsigned int j = 0; j <= jm + 1; j++) {
			for (unsigned int i = 0; i <= im + 1; i++) { //

				if (i == 0) {
					p[FTNREF3D0(0, j, k, ip + 3, jp + 3)] = p[FTNREF3D0(1, j, k,ip + 3, jp + 3)];
					p[FTNREF3D0(im + 1, j, k, ip + 3, jp + 3)] = p[FTNREF3D0(im,j, k, ip + 3, jp + 3)];
				}
			}
		}
	}
//	// 0,0,0 .. im+1, 0, km+1
//	// 0, jm+1,0 .. im+1, jm+1, km+1
	for (unsigned int k = 0; k <= km + 1; k++) {
		for (unsigned int j = 0; j <= jm + 1; j++) { //
			for (unsigned int i = 0; i <= im + 1; i++) {
				if (j == 0) {
					p[FTNREF3D0(i, 0, k, ip + 3, jp + 3)] = p[FTNREF3D0(i, jm, k, ip + 3, jp + 3)];
					p[FTNREF3D0(i, jm + 1, k, ip + 3, jp + 3)] = p[FTNREF3D0(i,1, k, ip + 3, jp + 3)];
				}
			}
		}
		}

//!
//! --computational boundary(neumann condition)
	// 0,0,0 .. im+1,km+1,0
	// 0,0,km+1 .. im+1,jm+1,km+1
	for (unsigned int k = 0; k <= km + 1; k++) { //
		for (unsigned int j = 0; j <= jm + 1; j++) {
			for (unsigned int i = 0; i <= im + 1; i++) {
				if (k == 0) {
					p[FTNREF3D0(i, j, 0, ip + 3, jp + 3)] = p[FTNREF3D0(i, j, 1,ip + 3, jp + 3)];
					p[FTNREF3D0(i, j, km + 1, ip + 3, jp + 3)] = p[FTNREF3D0(i,j, km, ip + 3, jp + 3)];
				}
			}
		}
	}
/*
	for (unsigned int k = 0; k <= km + 1; k++) { //
		p[FTNREF3D0(0,jm+1,k,ip+3,jp+3)]=p[FTNREF3D0(1,1,k,ip+3,jp+3)];
		p[FTNREF3D0(1,jm+1,k,ip+3,jp+3)]=p[FTNREF3D0(1,1,k,ip+3,jp+3)];

		p[FTNREF3D0(im,jm+1,k,ip+3,jp+3)]=p[FTNREF3D0(im,1,k,ip+3,jp+3)];
		p[FTNREF3D0(im+1,jm+1,k,ip+3,jp+3)]=p[FTNREF3D0(im,1,k,ip+3,jp+3)];

		p[FTNREF3D0(0,0,k,ip+3,jp+3)]=p[FTNREF3D0(1,jm,k,ip+3,jp+3)];
		p[FTNREF3D0(1,0,k,ip+3,jp+3)]=p[FTNREF3D0(1,jm,k,ip+3,jp+3)];

		p[FTNREF3D0(im,0,k,ip+3,jp+3)]=p[FTNREF3D0(im,jm,k,ip+3,jp+3)];
		p[FTNREF3D0(im+1,0,k,ip+3,jp+3)]=p[FTNREF3D0(im,jm,k,ip+3,jp+3)];
	}


	p[FTNREF3D0(0,jm+1,0,ip+3,jp+3)]=p[FTNREF3D0(0,jm+1,1,ip+3,jp+3)];
	p[FTNREF3D0(1,jm+1,0,ip+3,jp+3)]=p[FTNREF3D0(1,jm+1,1,ip+3,jp+3)];

	p[FTNREF3D0(im,jm+1,0,ip+3,jp+3)]=p[FTNREF3D0(im,jm+1,1,ip+3,jp+3)];
	p[FTNREF3D0(im+1,jm+1,0,ip+3,jp+3)]=p[FTNREF3D0(im+1,jm+1,1,ip+3,jp+3)];

	p[FTNREF3D0(0,0,0,ip+3,jp+3)]=p[FTNREF3D0(0,0,1,ip+3,jp+3)];
	p[FTNREF3D0(1,0,0,ip+3,jp+3)]=p[FTNREF3D0(1,0,1,ip+3,jp+3)];

	p[FTNREF3D0(im,0,0,ip+3,jp+3)]=p[FTNREF3D0(im,0,1,ip+3,jp+3)];
	p[FTNREF3D0(im+1,0,0,ip+3,jp+3)]=p[FTNREF3D0(im+1,0,1,ip+3,jp+3)];


	p[FTNREF3D0(0,jm+1,km+1,ip+3,jp+3)]=p[FTNREF3D0(0,jm+1,km,ip+3,jp+3)];
	p[FTNREF3D0(1,jm+1,km+1,ip+3,jp+3)]=p[FTNREF3D0(1,jm+1,km,ip+3,jp+3)];

	p[FTNREF3D0(im,jm+1,km+1,ip+3,jp+3)]=p[FTNREF3D0(im,jm+1,km,ip+3,jp+3)];
	p[FTNREF3D0(im+1,jm+1,km+1,ip+3,jp+3)]=p[FTNREF3D0(im+1,jm+1,km,ip+3,jp+3)];

	p[FTNREF3D0(0,0,km+1,ip+3,jp+3)]=p[FTNREF3D0(0,0,km,ip+3,jp+3)];
	p[FTNREF3D0(1,0,km+1,ip+3,jp+3)]=p[FTNREF3D0(1,0,km,ip+3,jp+3)];

	p[FTNREF3D0(im,0,km+1,ip+3,jp+3)]=p[FTNREF3D0(im,0,km,ip+3,jp+3)];
	p[FTNREF3D0(im+1,0,km+1,ip+3,jp+3)]=p[FTNREF3D0(im+1,0,km,ip+3,jp+3)];
*/
//!

/*
 Based on a simple test, the differences between merged and not merged are in coords:

 i/j/k
 ----
 i,0,0 : 1 .. im: p[i,jm,1]; 0: p[1,jm,1]; im+1: p[im,im,1]
 i,jm+1,0 : 1 .. im: p[i,1,1]; 0: p[1,1,1]; im+1: p[im,1,1]

 0,j,0 : 1 .. jm : p[1,j,]; jm+1: p[1,1,1]
 im+1,j,0 : 1 .. jm : p[im,j,1]; jm+1 : p[im,1,1]

 0,0,k : 1 .. km : p[1,jm,k]; km+1 : p[1,jm,km]
 im+1,0,k : 1 .. km : p[im,jm,k]; km+1 : p[im,jm,km]

 So we'd need 3 very short loops, I guess it is easier to do these on the host.

 */
}
void boundp_new(__global float2 *p,const unsigned int im,const unsigned int jm,const unsigned int km,unsigned int idx_g,unsigned int idx_l) {
	const unsigned int ip = im;
	const unsigned int jp = jm;
	const unsigned int kp = km;
				unsigned int i,j,k;
			// Now, we consider the planes, ribs and corners by setting the bounds on the ranges

			// 1. Planes
			// 1.1 E/W planes, i.e. i = 0/1 im/im+1 for all j and k

			j = idx_g;
			k = idx_l;

			if (j>0 && j<=jm && k>0 && k<=km) {
				p[FTNREF3D0(0,j,k,ip+3,jp+3)] = p[FTNREF3D0(1,j,k,ip+3,jp+3)];
				p[FTNREF3D0(im+1,j,k,ip+3,jp+3)] = p[FTNREF3D0(im,j,k,ip+3,jp+3)];
			}
			// 1.2 N/S planes, i.e. j = 0/1 jm/jm+1 for all i and k

			i = idx_g;
			k = idx_l;
			if (i>0 && i<=im && k>0 && k<=km) {
				p[FTNREF3D0(i,0,k,ip+3,jp+3)] = p[FTNREF3D0(i,jm,k,ip+3,jp+3)];
				p[FTNREF3D0(i,jm+1,k,ip+3,jp+3)] = p[FTNREF3D0(i,1,k,ip+3,jp+3)];
			}
			// 1.3 T/B planes, i.e. k = 0/1 km/km+1 for all i and j
			//FIXME! this somehow results in WRONG values in the inner cube!
			i = idx_g;
			j = idx_l;
			if (i>0 && i<=im && j>0 && j<=jm) {
				p[FTNREF3D0(i,j,0,ip+3,jp+3)] = p[FTNREF3D0(i,j,1,ip+3,jp+3)];
				p[FTNREF3D0(i,j,km+1,ip+3,jp+3)] = p[FTNREF3D0(i,j,km,ip+3,jp+3)];
			}
			// 2. Ribs
			// 2.1 Ribs for E/W planes, so we have 8 of them

			// Vertical ribs on E/W planes
			k = idx_l;
			if (k > 0 && k <= km) {
				// Vertical ribs on W plane : i=0, j=0 ; i=0, j=jm+1
				p[FTNREF3D0(0,0,k,ip+3,jp+3)] = p[FTNREF3D0(1,jm,k,ip+3,jp+3)];
				p[FTNREF3D0(0,jm+1,k,ip+3,jp+3)] = p[FTNREF3D0(1,1,k,ip+3,jp+3)];
				// Vertical ribs on E plane : i=jm+1, j=0 ; i=jm+1, j=jm+1
				p[FTNREF3D0(im+1,0,k,ip+3,jp+3)] = p[FTNREF3D0(im,jm,k,ip+3,jp+3)];
				p[FTNREF3D0(im+1,jm+1,k,ip+3,jp+3)] = p[FTNREF3D0(im,1,k,ip+3,jp+3)];
			}
			// Horizontal ribs on E/W plane

			j = idx_l;


			if (j>0 && j<=jm ) {
				// Horizontal ribs on W plane: k=0, i=0; k=km+1; i=0
				p[FTNREF3D0(0,j,0,ip+3,jp+3)] = p[FTNREF3D0(1,j,1,ip+3,jp+3)];
				p[FTNREF3D0(0,j,km+1,ip+3,jp+3)] = p[FTNREF3D0(1,j,km,ip+3,jp+3)];
				// Horizontal ribs on E plane: k=0, i=im+1; k=km+1; i=im+1
				p[FTNREF3D0(im+1,j,0,ip+3,jp+3)] = p[FTNREF3D0(im,j,1,ip+3,jp+3)];
				p[FTNREF3D0(im+1,j,km+1,ip+3,jp+3)] = p[FTNREF3D0(im,j,km,ip+3,jp+3)];

			}
			// 2.2 Ribs for N/S planes, so we have 4 of them
			i = idx_l;
			if (i>0 && i<=im ) {
				// Horizontal ribs on S plane: k=0, j=0; k=km+1; j=0
				p[FTNREF3D0(i,0,0,ip+3,jp+3)] = p[FTNREF3D0(i,jm,1,ip+3,jp+3)];
				p[FTNREF3D0(i,0,km+1,ip+3,jp+3)] = p[FTNREF3D0(i,jm,km,ip+3,jp+3)];
				// Horizontal ribs on N plane: k=0, j=jm+1; k=km+1; j=jm+1
				p[FTNREF3D0(i,jm+1,0,ip+3,jp+3)] = p[FTNREF3D0(i,1,1,ip+3,jp+3)];
				p[FTNREF3D0(i,jm+1,km+1,ip+3,jp+3)] = p[FTNREF3D0(i,1,km,ip+3,jp+3)];
			}

			// 2.3 The ribs for the T/B planes are already covered by the above.

			// 3. Corners
			// bottom-left, N
			p[FTNREF3D0(0,0,0,ip+3,jp+3)] = p[FTNREF3D0(1,jm,1,ip+3,jp+3)];
			// bottom-right, N
		    p[FTNREF3D0(im+1,0,0,ip+3,jp+3)] = p[FTNREF3D0(im,jm,1,ip+3,jp+3)];
			// top-left, N
			p[FTNREF3D0(0,0,km+1,ip+3,jp+3)] = p[FTNREF3D0(1,jm,km,ip+3,jp+3)];
			// top-right, N
		    p[FTNREF3D0(im+1,0,km+1,ip+3,jp+3)] = p[FTNREF3D0(im,jm,km,ip+3,jp+3)];

			// bottom-left, S
			p[FTNREF3D0(0,jm+1,0,ip+3,jp+3)] = p[FTNREF3D0(1,1,1,ip+3,jp+3)];
			// bottom-right, S
		    p[FTNREF3D0(im+1,jm+1,0,ip+3,jp+3)] = p[FTNREF3D0(im,1,1,ip+3,jp+3)];
			// top-left, S
			p[FTNREF3D0(0,jm+1,km+1,ip+3,jp+3)] = p[FTNREF3D0(1,1,km,ip+3,jp+3)];
			// top-right, S
		    p[FTNREF3D0(im+1,jm+1,km+1,ip+3,jp+3)] = p[FTNREF3D0(im,1,km,ip+3,jp+3)];

}
