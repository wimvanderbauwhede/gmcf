/*
 *  (c) 2004-2013 Wim Vanderbauwhede <wim@dcs.gla.ac.uk>
 */
#include "gmcf.h"

void gmcfCoupler(std::string tdc_file, unsigned int ncycles=500) {
    SBA::Runtime coupler(tdc_file,ncycles);
    coupler.run();
}
