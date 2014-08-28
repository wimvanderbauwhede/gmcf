#include "SBA/Types.h"

class GMCF {
    public:
        int run_model1(const SBA::System* sba_sysptr, const SBA::Tile* sba_tileptr);
        int run_model2(const SBA::System* sba_sysptr, const SBA::Tile* sba_tileptr);
};
