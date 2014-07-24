#ifndef __CAST_POINTERS_H__
#define __CAST_POINTERS_H__
#include <cctype>
// e.g. SBA::System* sba_sysptr = fromWord<SBA::System*>(*ivp);
template<typename TPtr> TPtr fromWord(int64_t ivp) {
    int64_t* ip=(int64_t*)ivp;
    void* vp=(void*)ip;
    TPtr tp = (TPtr)vp;
    return tp;
}
// e.g. *sba_sys_ivp=toWord<SBA::System*>(sba_sysptr);
template<typename TPtr> int64_t toWord(TPtr tp) {
    void* vp = reinterpret_cast<void*>(tp);
    int64_t ivp = (int64_t)vp;
    return ivp;
}

#endif // __CAST_POINTERS_H__
