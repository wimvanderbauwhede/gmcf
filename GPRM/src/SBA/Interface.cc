// Interface.rb
//
// :title: Gannet Service-based SoC project - Interface class
//
// (c) 2004-2009 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>

#include <queue>
#include "System.h"
#include "Interface.h"
#include "Timings.h"

using namespace std;
using namespace SBA;

 Bytecode Interface::read_bytecode(string tdc_file) {
         FILE * fd=fopen(tdc_file.c_str(),"r");
         Bytecode bytewords;
        Word byteword=0;
        uint hwb=0;
         Word byte=0;
         while((Int)byte!=EOF) {
         byte=(Word)fgetc(fd);
            byteword+=(byte<<(8*(NBYTES-1-hwb)));
            hwb=hwb+1;
            if (hwb==NBYTES){
                hwb=0;
                bytewords.push_back(byteword);
                byteword=0;
            }
         }
        return bytewords;
    }

