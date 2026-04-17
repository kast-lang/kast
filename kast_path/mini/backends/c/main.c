#undef main
int main()
{
    Context ctx;
    init_consts(&ctx);
    minikast_main.f(minikast_main.captured, &ctx);
    return 0;
}