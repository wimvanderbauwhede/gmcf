#ifndef _SBA_INCLUSION_SET_
#define _SBA_INCLUSION_SET_

#include <unordered_map>
namespace SBA {
class InclusionSet {
    private:
        std::unordered_map<unsigned int,unsigned int> _set;
        unsigned int _size;
	public:
    InclusionSet() : _size(0) {}
	void add(unsigned int);
	void remove(unsigned int);
	unsigned int size();
	unsigned int count(unsigned int);
};

}

#endif
