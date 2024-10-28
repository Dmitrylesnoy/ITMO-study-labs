import math as m

for n in range(1, 10000):
    a = (4 * n - 17) / (17 * n + 13)
    d = abs(a - (4 / 17))
    print(a, d, n)
    if d < 0.1:
        print(n)
        break
