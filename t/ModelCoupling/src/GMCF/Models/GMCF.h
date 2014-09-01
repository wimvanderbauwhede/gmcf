#include "SBA/Types.h"
#include "SBA/System.h"
#include "SBA/Tile.h"

class GMCF {
    public:
		int64_t run_model1(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t);
		int64_t run_model2(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t);
};
