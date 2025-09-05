import re

inp = input()
# inp = "gIT . Git ... hub ! ; git a ; hub . hub hub. hub    git hub ! git hub & hub git git git hub hub git"
# inp = "Довольно распространённая ошибка ошибка ошибка – это лишний повтор повтор слова слова слова. Смешно, не не не не не правда ли? Не нужно портить хор хоровод."
words = re.split(r"(\W+)", inp)
print(words)
words1 = [words[0]]

i = 1
l = len(words)
while i < len(words) - 1:
    words1.append(words[i])

    if words[i-1] != words[i+1]:
        # words1.append(words[i])
        # i += 1
        words1.append(words[i+1])
        # i +6= 2
    # else:
        # words1.append(words[i])
    i += 2
print(words1)
# i = 1
# words1 = [words[0],words[1]]
# for i in range(1, l):
#     if re.match("\w", words[i]) is not None:
#         for w in words1[::-1]:
#             if len(w)!=1:
#                 if words[i] == w:
#                    continue
#     else:
#         words1.append(words[i])
#     pass
# words1.append(words[-2])
# words1.append(words[-1])

while "" in words1:
    words1.remove("")
i = 0
while i < len(words1) - 1:
    if (
        re.match("\W", words1[i]) is not None
        and re.match("\W", words1[i + 1]) is not None
    ):
        words1.pop(i)
    else:
        i += 1

print(inp)
print("".join(words1))
