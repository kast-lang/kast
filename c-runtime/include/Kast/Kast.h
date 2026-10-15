#include "./__common__.h"
#include "./array_list.h"
#include "./box.h"
#include "./claim.h"
#include "./cli.h"
#include "./closure.h"
#include "./conv.h"
#include "./dbg.h"
#include "./fmt.h"
#include "./io.h"
#include "./mem.h"
#include "./random.h"
#include "./ref_counted.h"
#include "./string.h"
#include "./sys.h"
#include "./tcp.h"
#include "./thread.h"
#include "./type_info.h"
#include "./unwind.h"
#ifdef KAST_ALLOCATION_STATS
#include "allocation_stats.h"
#endif
#ifdef USE_BACKTRACE
#include "./backtrace.h"
#endif

void Kast_init(int argc, const char* argv[]);
void Kast_quit(void);

noreturn void Kast_match_non_exhaustive(void);
noreturn void default_panic_handler(const StringView s);
