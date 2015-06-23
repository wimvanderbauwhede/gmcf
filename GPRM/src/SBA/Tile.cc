// Tile.rb
//
// :title: Service-based SoC project - SBA Tile class
//
//
// *
// *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *
//
// $Id: Tile.rb 2532 2009-04-22 16:15:08Z socgroup $


#include "System.h"
#if 0
#ifndef DARWIN
#define _GNU_SOURCE
#include <sched.h>
#include <pthread.h>
#endif
#endif
#include "Tile.h"
#include "Schedule.h"
// The SBA Tile is the interface between the Service Manager and the Network.
// It transfers data from the Network tx_fifo to a local rx_fifo
// and from the local tx_fifo to the Network rx_fifo


    //-- ----------------------------------------------------------------------------
    //
    // Main methods
    //

using namespace std;
using namespace SBA;


void Tile::run() {
#if USE_THREADS==1
	transceiver->rx_fifo.wait_for_packets();
#endif    
#ifdef VERBOSE
                		cout << "Tile::run() start for tile "<< address <<","<<service<<"\n";
#endif // VERBOSE

	status = true;
	while(status==true) {
		 service_manager.run();
		 if (service_manager.core_status==CS_busy) {
		   service_core.run();
		 }
		 transceiver->run();
    	 status = service_manager.status || (transceiver->tx_fifo.length()>0) || transceiver->rx_fifo.has_packets();
	}
#ifdef VERBOSE
                		cout << "Tile::run() done\n";
#endif // VERBOSE

}

#if USE_THREADS==1
    void *SBA::run_tile_loop(void* voidp) {

    	SBA::Tile* tilep = (SBA::Tile*)voidp;
        while (1) {
        	tilep->run();
        }
        pthread_exit((void *) 0);
    }
    void Tile::run_th () {
#ifdef VERBOSE
    cout << "Starting Tile " << service << "\n";
#endif // VERBOSE
//    	thread_mapping(address);
#ifndef DARWIN
        cpu_set_t cpuset;
        CPU_ZERO(&cpuset);
        CPU_SET(service - 1, &cpuset);
#endif
        pthread_attr_init(&attr);
#ifndef DARWIN
        pthread_attr_setaffinity_np(&attr, sizeof(cpu_set_t), &cpuset);
#endif
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        pthread_create(&tid, &attr, SBA::run_tile_loop, (void*)this);
        //cout << "Thread ID: " << pthread_self() << endl;
#if 0
// WV: Thread pinning, untested and unused, using Ashkan's approach instead
#ifndef DARWIN
//		cpu_set_t cpuset;
//		CPU_ZERO(&cpuset);
//		CPU_SET(service, &cpuset);
//		int st = pthread_setaffinity_np(tid, sizeof(cpu_set_t), &cpuset);
//		if (st != 0) {
//		  std::cerr <<"Could not set affinity on thread " << tid << "\n";
//		}
#endif
#endif
        //printf("Thread ID:   %ld  Created on the Tile Adress: %d \n", tid, address);
    }
#endif // USE_THREADS==1

