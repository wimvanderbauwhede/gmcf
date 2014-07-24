/*
 *  (c) 2004-2013 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 */
#include "gannet.h"

void gannetvm(std::string tdc_file, const std::vector< void*>& args, unsigned int ncycles=500) {
/*
    SBA::StringPair sp;
	sp.taskfile=tdc_file;
    SBA::TaskDescList tds;
	tds.push_back(sp);

    SBA::Runtime gannet(tds);
*/    
    SBA::Runtime gannet(tdc_file,ncycles);
    //gannet.setNCycles(ncycles);
	gannet.run(args);
}

void gannetvm(std::string tdc_file, unsigned int ncycles=500) {
/*
    SBA::StringPair sp;
	sp.taskfile=tdc_file;
    SBA::TaskDescList tds;
	tds.push_back(sp);

    SBA::Runtime gannet(tds);
*/    
    SBA::Runtime gannet(tdc_file,ncycles);
    //gannet.setNCycles(ncycles);
	gannet.run();
}
