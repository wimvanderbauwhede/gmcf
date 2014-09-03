
/* Clearly, this is entirely generic and should be generated and put in gensrc
The generator should be trivial as well:
for my $i (1..$n_models) {
print "\t\t".'int64_t run_model'.$i.'(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t);'."\n";
}

 */
#include "SBA/Types.h"
#include "SBA/System.h"
#include "SBA/Tile.h"

class GMCF {
    public:
		int64_t run_model1(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t);
		int64_t run_model2(SBA::System* sba_sysptr, SBA::Tile* sba_tileptr, uint64_t);
};
