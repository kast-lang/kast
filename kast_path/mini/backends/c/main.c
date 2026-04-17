#undef main
int main()
{
    Context ctx;
    init_consts(&ctx);
    minikast_main(&ctx);
    return 0;
}