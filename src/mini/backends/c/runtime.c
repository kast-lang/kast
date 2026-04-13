typedef struct RawUnwindToken
{
    int id;
} RawUnwindToken;

const RawUnwindToken NO_UNWIND = {.id = 0};

RawUnwindToken unwinding = NO_UNWIND;

int next_raw_unwind_token_id;