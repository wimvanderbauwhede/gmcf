#include "InclusionSet.h"
using namespace SBA;
void InclusionSet::add(unsigned int elt) {
    if (_set.count(elt)>0) {
        _set[elt]++;
    } else {
        _set[elt]=1;
    }
    _size++;
}
void InclusionSet::add(unsigned int elt, unsigned int value_to_add) {
    if (_set.count(elt)>0) {
        _set[elt] = _set[elt] + value_to_add;
    } else {
        _set[elt]= value_to_add;
    }
    _size++;
}
void InclusionSet::remove(unsigned int elt) {
    if (_set.count(elt)>0) {
        _set[elt]--;
    }
    // _set[elt]->count(elt) returns 1 even if _set[elt] is 0
    // hence the check for a 0 value rather than a 0 count
    if (_set[elt]==0) {
        _set.erase(elt);
    }
    _size--;
}
unsigned int InclusionSet::size() {
    return _size;
}
unsigned int InclusionSet::count(unsigned int elt) {
    return _set.count(elt);
}

unsigned int InclusionSet::takefirst() {
    return _set.begin()->first;
}

const std::vector<unsigned int>& InclusionSet::elts() {
	std::vector<unsigned int> elts;
	for (auto _iter : _set) {
		elts.push_back( _iter.first );
	}
	return elts;
}

