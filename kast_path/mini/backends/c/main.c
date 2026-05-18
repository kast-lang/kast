#undef main
int main(int argc, char *argv[])
{
    Context ctx;
    ctx.program_args.data = argv;
    ctx.program_args.length = argc;
    init_consts(&ctx);
    minikast_main.f(minikast_main.captured, &ctx);
    return 0;
}