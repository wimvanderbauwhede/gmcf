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
void InclusionSet::remove(unsigned int elt) {
  if (_set.count(elt)>0) {
        _set[elt]--;
    }
    if (_set.count(elt)==0) {
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

const std::vector<unsigned int>& InclusionSet::elts() {
	std::vector<unsigned int> elts;
	for (auto _iter : _set) {
		elts.push_back( _iter.first );
	}
	return elts;
}
