int a;

int main() {
	{int a = 0;}
	a = 1;
	{int a = 2;}
	a = 3;
	{int a = 4;}
	putint(a);
	return 0;
}