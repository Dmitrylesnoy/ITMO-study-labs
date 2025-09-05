inp = input("Put a target number: ").lower()
pow1 = input("Put a 1st pow: ").lower()
pow2 = input("Put a 2nd pow: ").lower()

alph = "0123456789abcdefghijklmnopqrstuvwxyz"

def Int2Dec(inp, pow):
    inp = inp[::-1]
    calc = 0
    for i in range(len(inp)):
        calc += alph.index(inp[i]) * (pow**i)
    return calc

def Float2Dec(inp, pow):
    calc = 0
    for i in range(len(inp)):
        calc += alph.index(inp[i]) * (pow ** (-1 * (i + 1)))
    return calc

def Fib2Dec(inp):
    calc = 0
    inp = str(inp)
    fib = [1, 2]
    for i in range(len(inp)):
        calc += int(inp[-(i + 1)]) * fib[i]
        fib.append(fib[-1] + fib[-2])
    return calc

def Fac2Dec(inp):
    calc = 0
    inp = str(inp)
    fac = 1
    for i in range(len(inp)):
        calc += int(inp[-(i + 1)]) * fac
        fac *= i + 2
    return calc

def Dec2Int(inp, pow):
    if inp == 0:
        return "0"
    calc = []
    while inp != 0:
        calc = [alph[inp % pow]] + calc
        inp //= pow
    calc = "".join(str(_) for _ in calc)
    return calc

def Dec2Float(inp, pow, acc):
    calc = ""
    for i in range(acc + 1):
        calc += alph[int(inp * pow)]
        inp = (inp * pow) % 1
        if inp == 0.0:
            break
    return "." + calc

def Dec2Fib(inp):
    calc = ""
    inp = int(inp)
    fiblist = [1, 1]
    while fiblist[-1] < inp:
        fiblist.append(fiblist[-1] + fiblist[-2])
    fiblist = fiblist[1:-1]
    while inp != 0:
        if inp >= fiblist[-1]:
            calc += "1"
            inp -= fiblist[-1]
        else:
            calc += "0"
        fiblist.pop()
    return calc

def Dec2Fac(inp):
    calc = ""
    inp = int(inp)
    faclist = [1]
    i = 1
    while faclist[-1] < inp:
        i += 1
        faclist.append(faclist[-1] * i)
    faclist = faclist[:-1]
    while inp != 0:
        if inp >= faclist[-1]:
            c = inp // faclist[-1]
            calc += alph[c]
            inp -= c * faclist[-1]
        else:
            calc += "0"
        faclist.pop()
    return calc


part1, part2 = inp, -1
if "." in inp:
    part1, part2 = inp.split(".")
elif "," in inp:
    part1, part2 = inp.split(",")


if pow1 == "fib":
    part1_d = Fib2Dec(part1)
elif pow1 == "fac":
    part1_d = Fac2Dec(part1)
else:
    pow1 = int(pow1)
    part1_d = Int2Dec(part1, pow1)
    if part2 != -1:
        part2_d = Float2Dec(part2, pow1)
    else:
        part2_d = -1

part1_f, part2_f = "0", "0"
if pow2 == "fib":
    part1_f = Dec2Fib(part1_d)
elif pow2 == "fac":
    part1_f = Dec2Fac(part1_d)
else:
    pow2 = int(pow2)
    part1_f = Dec2Int(part1_d, pow2)
    if part2 != -1:
        part2_f = Dec2Float(part2_d, pow2, 10)
    else:
        part2_f = -1

print(part1_f + (part2_f if part2_f != -1 else ""))
