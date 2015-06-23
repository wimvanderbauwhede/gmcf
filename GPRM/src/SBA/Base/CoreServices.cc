// Base::CoreServices.cc
//   
// :title: Gannet Service-based SoC project - Service Core Library 
//    
//
// *
// *  (c) 2004-2010 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
// *  
//
#define  NO_SERVICES

#include <fstream>
#include <sstream>
#include <unordered_map>
#include "SBA/System.h" 
#include "SBA/Tile.h" 
#include "Base/CoreServices.h"
#include "SystemConfiguration.h"
#ifndef NO_SERVICES
#include "Base/Let.h"
#include "Base/Apply.h"
#endif

 using namespace std;

 using namespace SBA; 

 void Base::CoreServices::ls_BEGIN() {
 #ifdef VERBOSE
          cout << "PAR (BEGIN) CORE: "<<nargs()<<" args\n";
 #endif // VERBOSE
         Word res;
         uint n_args=nargs();
         res=arg(n_args-1);
 #ifdef VERBOSE
          cout << service<< " CORE: Passing on result "<<res<<"\n";
 #endif // VERBOSE
         result(res);
  } // of BEGIN


#ifndef NO_SERVICES    

 double Base::CoreServices::word2dbl(Word result) {
        u_int64_t* result_p=&result;
        void* tmpd=(void*)result_p;
        double* tmp_dbl_p = (double*) tmpd;
        double dbl_result=*tmp_dbl_p;
        return dbl_result;
    }

 Word Base::CoreServices::dbl2word(double dbl) {
        double* dbl_p=&dbl;
        void* v_p=(void*)dbl_p;
        Word* w_p=(Word*)v_p;
        Word w= *w_p;
        return w;        
    }

 Word_List Base::CoreServices::string2symbol(string str) {
        uint  npad=NBYTES - (str.size() % NBYTES);
        uint nwords=(str.size()+npad)/NBYTES;
         str.resize(NBYTES*nwords,0);
        Symbol_t sheader = mkSymbol(K_B,T_s,1,1,0,nwords,npad);
        Word_List sym;
        sym.push_back(sheader);
        for(uint i=0;i<nwords;i++) {        
            Word strword=0;
            for (uint j=0;j<NBYTES;j++) {
                strword+=(Word)str[NBYTES*i+j]<<8*(NBYTES-j-1);
            }
            cout << strword <<"\n";
            sym.push_back(strword);
        }        

    
        return sym;
    }
 string Base::CoreServices::extsym2str(Word sym) {
	 Word filename_ptr=getExtValue(sym);
	 void* vp = (void*)filename_ptr;
	 // the format is nwords|npadbytes|content
	 Word* wp=(Word*)vp;
	 uint nwords = wp[0];
	 uint npadbytes = wp[1];
//	 unsigned char* bytes =(unsigned char*)wp;
	 string filename = "";
//	 for (uint idx=16;idx<16+nwords*8-npadbytes;idx++) {
//		 filename+=bytes[idx];
//	 }
//	 std::cout << "extsym2str(): nwords="<<nwords<<", npadbytes="<<npadbytes<<"\n";
	 for (uint idx=0; idx<nwords; idx++) {
		 Word w = wp[idx+2];
//		 std::cout << std::hex <<w <<"\n";
		 for (uint jdx=0;jdx<8;jdx++) {
			 Word byte = (w>>(8*(7-jdx))) & 0xFF;
			 unsigned char ch=(unsigned char)byte;
//			 std::cout << "("<<byte<<") "<<ch <<"\n";
			 if (idx==nwords-1 && jdx == (8-npadbytes)) {
//				 std::cout << "\n" ;
				 break;
			 }
			 filename+=ch;
		 }
	 }
	 return filename;
 }
 string Base::CoreServices::sym2str(Word_List sym) {
         Word header=sym.front();sym.pop_front();
        uint nwords=getSubtask(header);
        uint padding=getName(header);

        string str;
        str.reserve(nwords*NBYTES-padding);
        for(uint i=0;i<nwords;i++) {    
            uint npad=(i==nwords-1)?padding:0;
            Word strword=sym.front();sym.pop_front();
            for (uint j=0;j<NBYTES-npad;j++) {
                char byte=(char)((strword>>8*(NBYTES-j-1))&255);
                str+=byte;
            }
        }       
        

    
        return str;
    }
    

 string Base::CoreServices::wl2str(Word_List wl) {
        uint nwords=wl.size();
        uint padding=0;
        string str;
        str.reserve(nwords*NBYTES-padding);
        for(uint i=0;i<nwords;i++) {    
            uint npad=(i==nwords-1)?padding:0;
            Word strword=wl.front();wl.pop_front();
            for (uint j=0;j<NBYTES-npad;j++) {
                char byte=(char)((strword>>8*(NBYTES-j-1))&255);
                str+=byte;
            }
        }       
       
    
        return str;
    }
    

 Int Base::CoreServices::sym2int(Word_List sym) {
        return getInt(sym);
    }
    

 Word Base::CoreServices::sym2uint(Word_List sym) {
        return getUInt(sym);
    }
    

 bool Base::CoreServices::sym2bool(Word_List sym) {
        return getUInt(sym)==1;
    }
                    

 float Base::CoreServices::sym2flt(Word_List sym) {
         Word result=sym.front();sym.pop_front();
        double flt_result=0; std::cerr << "ALU CORE: Float not implemented ("<<result<<")\n"; exit(0);
        return flt_result;
    }

 Int Base::CoreServices::div(Int m,Int n) {
        Int q=0;
        Int         sm=1;
        if (m<0){
            sm=-1;
        }        
        Int sn=1;
        if (n<0){
            sn=-1;
        }        
        Int           um=m*sm;
        Int     un=n*sn;
        Int modmn= um % un;
        q=(um-modmn)/un;
        if (2*modmn>un){
            q+=1;
        }  
        return q*sn*sm;
    }
    
    

 void Base::CoreServices::ls_ALU() {
	 /*
        // Set up context
	CoreServices* servicecore_ptr=this;
	CoreServices& *servicecore_ptr;
//System* sba_system_ptr=(System*)(servicecore_ptr->sba_system_ptr);
	 SBA::System& sba_system=*((SBA::System*)sba_system_ptr);
Tile& sba_tile=*(sba_system.nodes[servicecore_ptr->address]);
*/
#ifdef VERBOSE
        cout << "ALU CORE: processing subtask " <<current_subtask()<< ""<<endl;
#endif // VERBOSE
         Word_List result_list;
        Uint operation=method();

#ifdef VERBOSE
        cout << "ALU (" <<service<< ") CORE: " <<nargs()<< " addresses"<<endl;
#endif // VERBOSE
        Word res_symbol=arg(0);
        Int        int_result=getInt(res_symbol);
        Word wresult=int_result;
#ifdef VERBOSE
        cout << "ALU CORE: arg 1: Found int " <<wresult<< " (" <<T_i<< ") @ " <<0<< ""<<endl;
#endif // VERBOSE
        uint n_args=nargs();
        if (operation==M_CoreServices_ALU_not){
            wresult=1-wresult;
        } else {
            int             ii=0; 
            for(uint argn=0;argn<=n_args-1 ;argn++) {
                ii+=1;
                if (ii>1){
                Word tres_symbol=arg(argn);
                Int        int_tres=getInt(tres_symbol);
                 Word tres=int_tres;                    
#ifdef VERBOSE
                        cout << "ALU CORE: arg " <<ii<< ": Found int " <<tres<< " (" <<T_i<< ") @ " <<addr(argn)<< ""<<endl;
#endif // VERBOSE
                 switch (operation) {
                 case M_CoreServices_ALU_plus :
                 {
                     int_result+=int_tres;                    
                  break;
                 }
                 case M_CoreServices_ALU_minus :
                 {
                     int_result-=int_tres; 
                  break;
                 }
                 case M_CoreServices_ALU_times   :
                 {
                     int_result*=int_tres;
                  break;
                 }
                 case M_CoreServices_ALU_over :
                 {
                     int_result=div(int_result,int_tres);
                  break;
                 }
                 case M_CoreServices_ALU_lt :
                 {
                     int_result=(int_result<int_tres)?1:0;
                  break;
                 }
                 case M_CoreServices_ALU_gt :
                 {
                     int_result=(int_result>int_tres)?1:0;
                  break;
                 }
                 case M_CoreServices_ALU_eq :
                 {
                     int_result=(int_result==int_tres)?1:0;
                     break;}
                 default:
                    cerr << "Unknown ALU CORE service: " <<operation<< "";
                    exit(1);
                       exit(0);
                } ;
            }
            }
        }
         wresult=(Uint)int_result;                
#ifdef VERBOSE
        cout << "ALU CORE RESULT: (uint64) " <<wresult<< ""<<endl;
        cout << "ALU (" <<service<< ") CORE (" <<current_subtask()<< "):  result: " <<wresult<< ""<<endl;
#endif // VERBOSE
        Word one=1;
        if (wresult>((one<< FB_Value)-1)){ // too big to fit in a Word
            res_symbol=setExt(res_symbol,1);
            res_symbol=setNSymbols(res_symbol,1);
			Word* wp = new Word; // FIXME: malloc instead!
			void* vp=(void*)wp;
			Word wvp=(Word)vp;
			res_symbol=setValue(res_symbol,wvp);
//             result_list.push_back(res_symbol);result_list.push_back(wresult);
        } else {
            res_symbol=setExt(res_symbol,0);
            res_symbol=setValue(res_symbol,wresult);
//             result_list.push_back(res_symbol);
        }            
        result(res_symbol);
    } // of ALU

#include "ls_Math.cc"

#include "ls_SEQ.cc"

#include "ls_CTRL.cc"

#include "ls_APPLY.cc"

 void Base::CoreServices::ls_IF() {
	 uint  argidx=0;
	 uint operation =method();
        if (operation==M_CoreServices_IF_return or operation==M_CoreServices_IF_returntc){
            argidx=0;
        } else {
            unsigned int condval= arg(0) & 0x1;
            argidx= 2-condval;
        }
// If it's quoted and K_R, it will be a request, dispatch it;
// otherwise, return it, it will be data, just return it.
	if (is_quoted_ref(argidx)) {
			dispatch_reference(argidx);
			if (operation==M_CoreServices_IF_returntc
				||operation==M_CoreServices_IF_iftc
				) {
				// we ignore the dispatch and clean up
				core_status=CS_done;
			}
	} else {
			result(arg(argidx));
	}
} // of ls_IF

#include "ls_LET.cc"
#include "ls_IO.cc"
#include "ls_REG.cc"
//// This is the subroutine to run a certain task in a given thread
//void Base::CoreServices::ls_RUN() {
//}
#endif // NO_SERVICES

void Base::CoreServices::none() {
    // Set up context
     Result res; res.push_back((Word)0); result(res);
}        


// This one is generated by the build system in the inherited class
void Base::CoreServices::select_wrapper(unsigned int code) {
	switch (code) {
		default:
			none();
	};
}
