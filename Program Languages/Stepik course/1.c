#include "stdio.h"

void print_newline()
{
    puts("\n");
}

void f(int a, int b)
{
    printf("%d", a + b);
}

int avg3(int a, int b, int c)
{
    return (a + b + c) / 3;
}

int is_sorted3(int a, int b, int c)
{
    if (a < b && b < c)
        return 1;
    if (a > b && b > c)
        return -1;
    else
        return 0;
}

int max2(int a, int b)
{
    return a > b ? a : b;
}

int max3(int a, int b, int c)
{
    return max2(a, max2(b, c));
}

void fizzbuzz(int a)
{
    if (a > 0)
    {
        if (a % 3 == 0)
            printf("fizz");
        if (a % 5 == 0)
            printf("buzz");
    }
    else
        printf("no");
}

int main(void)
{
    f(5, 12);
    printf("%d", avg3(1, 2, 3));
    // printf("%d", 17283 + (5 * 6 * 7 * 8));
    return 0;
}
