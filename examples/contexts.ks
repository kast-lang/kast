const LoggerT = newtype {
    .log :: String -> (),
};
const Logger = @context LoggerT;
const log = s with Logger => (
    (@current Logger).log(s);
);

const create_logger = (name :: String) -> LoggerT => {
    .log = s => print(name + ": " + s),
};

const init_logger = (name :: String) -> () with Logger => (
    # with return Logger = create_logger(name);
);

init_logger("Test");
log("test logger");

with Logger = create_logger("GLOBAL");
(
    with Logger = create_logger("LOCAL");
    log("inside scope");
);
log("outside scope");
