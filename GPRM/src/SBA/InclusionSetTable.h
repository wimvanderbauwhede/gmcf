#ifndef _SBA_INCLUSION_SET_TABLE_
#define _SBA_INCLUSION_SET_TABLE_

#include "InclusionSet.h"

namespace SBA {
class InclusionSetTable {
    private:
        std::unordered_map<unsigned int,InclusionSet*> _set_tbl;
	public:
	void add(unsigned int,unsigned int,unsigned int);
	void remove(unsigned int,unsigned int);
	unsigned int size(unsigned int);
	const std::vector<unsigned int>& elts(unsigned int);
	unsigned int count(unsigned int,unsigned int);
	unsigned int takefirst(unsigned int);
    // WV: not sure about this, but since I allocated the InclusionSets with new() I need to de-allocate them somewhere.
    ~InclusionSetTable() {
        for (auto iter=_set_tbl.begin();iter!=_set_tbl.end();iter++) {
                _set_tbl.erase(iter); // I think at this point the pointer to InclusionSet still needs to be freed
                delete iter->second; 
        }
    }
};

}
#endif

