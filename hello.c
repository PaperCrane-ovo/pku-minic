int a;

int set_a(int val) { a = val; return a; }


int main()
{
    a = 2;
    set_a(1);
    putint(a);

    return 0;
}