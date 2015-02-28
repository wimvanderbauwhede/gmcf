#include "../../../launcher.h" //multiple definition of `chip_array' if places in Schedule.h
#include "Schedule.h"
#ifndef DARWIN
#define _GNU_SOURCE
#include <sched.h> // thread mapping
#endif
#define RESET_COLOR "\e[m"
#define MAKE_PURPLE "\e[35m"
#define MAKE_GREEN "\e[32m"
#define MAKE_RED "\e[31m"

int appid = 0;

 int thread_mapping(int address) {
	cpu_set_t  mask;
	CPU_ZERO(&mask);
	int target_core = address;
/*
	if(address==0) target_core=239;//address;
	else if (address==1) target_core=0;
	else if (address<122) target_core=((address-2)*2+1)%240;// for the SULUG with 4 cores in total!
        else target_core=((address-2)*2+2)%240;
*/

//	if((address%240)<120) target_core=((address*2)%NUM_CPU);// for the SULUG with 4 cores in total!
//	else target_core=((address*2+1)%NUM_CPU);
	
/*	
	else if (address<62) target_core=((address-2)*4+1)%240;// for the SULUG with 4 cores in total!
        else if(address<122) target_core=((address-2)*4+2)%240;
        else if(address<182) target_core=((address-2)*4+3)%240;
        else target_core=((address-2)*4+4)%240;
*/

	CPU_SET(target_core, &mask);
	if (sched_setaffinity(0, sizeof(mask), &mask) !=0)
		perror("sched_setaffinity");
	return target_core;
 }


