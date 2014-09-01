/*
   This is a function-based GMCF API intended for use in FORTRAN programs

   The approach is to cast the pointer to the SBA::System object to a 64-bit integer and return it.
   This word gets passed around in the FORTRAN code (as INTEGER*8).
   Every call to the API takes this word as its first argument, casts it back to the object and so on.

   So we create some low-level casting functions first
*/
#ifndef _GMCF_F_H_
#define _GMCF_F_H_

typedef int64_t* Tile;
typedef int64_t* System;

template<typename TPtr> TPtr fromWord(int64_t ivp) {
	int64_t* ip=(int64_t*)ivp;
	void* vp=(void*)ip;
	TPtr tp = (TPtr)vp;
	return tp;
}

template<typename TPtr> int64_t toWord(TPtr tp) {
	void* vp = reinterpret_cast<void*>(tp);
	int64_t ivp = (int64_t)vp;
	return ivp;
}

extern "C" {
#include "gmcfC.h"
}
#endif // _GMCF_F_H_
