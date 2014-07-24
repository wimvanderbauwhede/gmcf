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
#include "Tile.h"
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
	transceiver->rx_fifo.wait_for_packets(address);
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
    	 //transceiver.run();
		 transceiver->run();
    	 //status= service_manager.status || (transceiver.tx_fifo.length()>0) || transceiver.rx_fifo.has_packets();
    	 status= service_manager.status || (transceiver->tx_fifo.length()>0) || transceiver->rx_fifo.has_packets();
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
        pthread_attr_init(&attr);
        pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED);
        pthread_create(&tid, &attr, SBA::run_tile_loop, (void*)this);
        //cout << "Thread ID: " << pthread_self() << endl;
        //printf("Thread ID:   %ld  Created on the Tile Adress: %d \n", tid, address);
    }
#endif // USE_THREADS==1

