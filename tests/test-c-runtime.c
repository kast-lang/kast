#include <Kast.h>

int main(int argc, const char** argv) {
    Kast_init(argc, argv);
    Kast_Formatter fmt = Kast_Formatter_new(Kast_stdout_writer);
    String s = String_from_C_StringView("Hello, world!");
    Kast_dbg_write(&s, &String_TypeInfo, &fmt);
    Kast_write_c(stdout, "\n");
    String_drop(s);
    Kast_Formatter_drop(fmt);
    Kast_quit();
    return 0;
}
