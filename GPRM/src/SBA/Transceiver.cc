// Transceiver.rb
//
// :title: Gannet Service-based SoC project - Transmitter/Receiver class
//
//
// *
// *  (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *
//
////==============================================================================
////
//// Gannet Service-based SoC project - Tansmitter/Receiver class
////
////==============================================================================
//


#include "System.h"
#include "Transceiver.h"

using namespace std;
using namespace SBA;

 void Transceiver::run()  {

        transmit_packets();
#ifdef VERBOSE
                		cout << "Transceiver::run() done\n";
#endif // VERBOSE

    } //  of run()



#if USE_THREADS==1
 void Transceiver::receive_packets()  {
    System& sba_system=*((System*)sba_system_ptr);
#ifdef VERBOSE
        cout << "Node " <<service<< " receiving"<<endl;
#endif // VERBOSE
    } // of receive_packets
#endif // USE_THREADS


 void Transceiver::transmit_packets()  {
    System& sba_system=*((System*)sba_system_ptr);
#ifdef VERBOSE
         cout << service <<" TRX:transmit_packets(): FIFO length: "<< tx_fifo.length() << endl;
#endif // VERBOSE
        if (tx_fifo.status()==1 ){
            while (tx_fifo.status()==1){
                Packet_t packet= tx_fifo.front();tx_fifo.pop_front();
                Service service_id= getTo(getHeader(packet));
				ServiceAddress dest=service_id;
#ifdef VERBOSE
                cout << "PACKET:\n" << ppPacket(packet) << "\n";
                cout << "service id: " <<service_id<< "; dest addr: " <<dest<< ""<<endl;// Ashkan_debug
#endif // VERBOSE

                if (dest==sba_system.gw_address ){
#ifdef VERBOSE
                		cout << "Packet for gateway\n";
#endif // VERBOSE
                	sba_system.gw_instance.transceiver.rx_fifo.push_back(packet);
#ifdef VERBOSE
                		cout << "Packet delivered to gateway\n";
#endif // VERBOSE

                } else {
/*
    It is possible that the service_id is higher than the highest node id.
    In that case we remap using modulo. This requires that node ids are contiguous!    
*/
#ifdef VERBOSE
                	cout <<"WARNING: ad-hoc dest computation: "<<((service_id-1) % NSERVICES)+1<< "\n";
#endif
                    ServiceAddress dest = ((service_id-1) % NSERVICES)+1;
                         //sba_system.nodes[dest]->transceiver.rx_fifo.push_back(packet);
                	// dest is generally not the same as the index in the nodes array.!
                    sba_system.nodes[dest]->transceiver->rx_fifo.push_back(packet);
                }
            }
        }
    } // of transmit_packets

 void Transceiver::multicast()  {
     System& sba_system=*((System*)sba_system_ptr);
#ifdef VERBOSE
          cout << service <<" TRX:multicast(): FIFO length: "<< tx_fifo.length() << endl;
          cout << "this is tx_fifo.status():" << tx_fifo.status() << endl;//Ashkan_debug
#endif // VERBOSE
         if (tx_fifo.status()==1 ){
             while (tx_fifo.status()==1){
                 Packet_t packet= tx_fifo.front();tx_fifo.pop_front();
                 Service service_id= getTo(getHeader(packet));
 				ServiceAddress dest=service_id;
#ifdef VERBOSE
                 cout << "HEADER:"<<endl;
                 cout << (int)getTo(getHeader(packet))<<endl;

                 cout << "PACKET:\n" << ppPacket(packet) << "\n";
                 //cout << "service id: " <<service_id<< "; dest addr: " <<dest<< ""<<endl; // Ashkan_debug
                 //cout << "" <<(int)getType(getHeader(packet))<< " " <<getReturn_as(getHeader(packet))<< " to " <<sba_system.service_by_address(dest)<< " (addr:" <<dest<< ")\n"; // Ashkan: obsolete. Remove it
#endif // VERBOSE
 
 #if DISTR==0
                 if (dest==sba_system.gw_address ) {
                     sba_system.gw_instance.transceiver.rx_fifo.push_back(packet);
                 }
                 else {
					   //cout << "NSERVICES" << NSERVICES << endl;
                     for (unsigned int node_id=1;node_id<=NSERVICES;node_id++) {
					//for (map<ServiceAddress,Tile*>::iterator iter_=sba_system.nodes.begin();iter_!=sba_system.nodes.end();iter_++) {
					  // ServiceAddress node_id= iter_->first;
					   sba_system.nodes[node_id]->transceiver->rx_fifo.push_back(packet);
#ifdef VERBOSE
                 	   cout << "TRX::multicast(): packet sent to System.node: " << node_id << endl;
#endif
					   }
                 }
 #else // DISTR
     format="C";

         format="Q";

     uint payload_length=getLength_p(packet);
                 packet_buf=packet.pack(format*(3+payload_length));
                  uint32_t host=calcDestIP(dest); // assumes base_ip
                 tx_socket.send(packet_buf, 0, host, GWPORT+dest);
 #endif // DISTR
             }
         }
     } // of transmit_packets

