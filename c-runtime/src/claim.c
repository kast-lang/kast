#include <Kast/claim.h>

Kast_Claimed Kast_Claimed_init(const char* name) {
    return (Kast_Claimed) {
        .name = name,
        .state = Kast_Claimed_State_Owned,
#ifdef KAST_DEBUG
        .trace = NULL,
#endif
    };
}

void Kast_Claimed_access(Kast_Claimed* claimed) {
    switch (claimed->state) {
        case Kast_Claimed_State_Moved:
#ifdef KAST_DEBUG
            if (claimed->trace != NULL) {
                fprintf(
                    stderr,
                    "%s was previously claimed here:\n",
                    claimed->name
                );
                Kast_Backtrace_print(claimed->trace);
            }
#endif
            exit_with_error("Trying to access a moved %s", claimed->name);
            break;
        case Kast_Claimed_State_Owned:
            break;
        default:
            exit_with_error("looks like someone forgot a case in switch...");
    }
}

void Kast_mark_as_claimed(Kast_Claimed* claimed) {
    switch (claimed->state) {
        case Kast_Claimed_State_Moved:
#ifdef KAST_DEBUG
            if (claimed->trace != NULL) {
                fprintf(
                    stderr,
                    "%s was previously claimed here:\n",
                    claimed->name
                );
                Kast_Backtrace_print(claimed->trace);
            }
#endif
            exit_with_error("Trying to claim a moved %s", claimed->name);
            break;
        case Kast_Claimed_State_Owned:
            break;
        default:
            exit_with_error("looks like someone forgot a case in switch...");
    }
    claimed->state = Kast_Claimed_State_Moved;
#ifdef KAST_DEBUG
    claimed->trace = Kast_Backtrace_get_boxed();
#endif
}

Kast_Claimed_State Kast_Claimed_drop(Kast_Claimed claimed) {
#ifdef KAST_DEBUG
    if (claimed.trace != NULL) {
        Kast_Backtrace_drop_boxed(claimed.trace);
        return Kast_Claimed_State_Moved;
    } else {
        return Kast_Claimed_State_Owned;
    }
#else
    return claimed.state;
#endif
}
