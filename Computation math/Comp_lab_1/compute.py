import numpy as np


def transform2diagonal(A, B):
    n = len(A)
    A_new = A.copy()
    B_new = B.copy()
    for i in range(n):
        max_e = i + np.argmax(np.abs(A_new[i:, i]))  # max element from diagonale in row
        if max_e != i:
            A_new[[max_e, i]] = A_new[[i, max_e]]
            B_new[[max_e, i]] = B_new[[i, max_e]]
                
    # TODO: проверка отсутствия преобладания на диагонали
    
    return A_new, B_new


def calc_gauss_seidel(A, X, B, m=10, e=1e-2):
    """Решение СЛАУ Методом Гаусса-Зейделя

    Args:
        A (float): матрица коэффициентов
        X (float): матрица решений
        B (float): матрица свободных членов
        m (int, optional): макс. колическо итераций приближений. Defaults to 10.
        e (float, optional): допустимая погрешность. Defaults to 1e-2.
    """
    
    k=1
    while 1:
        delta = 0
        for i in range(n):
            s=0
            for j in range(i):
                s += A[i, j] * X[j]   
            for j in range(i+1, n):
                s += A[i, j] * X[j]
                
            new_x = (B[i] - s) / A[i, i]
            
            d = abs(new_x - X[i])
            if d > delta:
                delta = d
            X[i] = new_x

        print(f"Итерация {k}: {X.copy()}")

        if delta < e:
            print(f"Решение найдено за {k} итераций:")
            for i in range(n):
                print(f"x{i+1} = {X[i]:.6f}")
            break
        else:
            if k < m:
                k = k + 1
            else:
                print("Итерации расходятся")
                exit(0)

# TODO:  генерации случайной матрицы с заданной размерностью.
# TODO: проверка ввода пользователя
'''
• Точность задается с клавиатуры/файла, 
• Проверка диагонального преобладания (в случае, если диагональное 
преобладание в исходной  матрице отсутствует, сделать перестановку 
строк/столбцов до тех пор, пока преобладание не будет достигнуто). В 
случае невозможности достижения диагонального преобладания - вы
водить соответствующее сообщение. 
• Вывод нормы матрицы (любой, на Ваш выбор), 
• Вывод вектора неизвестных: 𝑥1, 𝑥2, …,𝑥𝑛, 
• Вывод вектора  погрешностей: |𝑥𝑖(𝑘) − 𝑥𝑖(𝑘−1)|'''

n = int(input("Введите размерность матрицы:"))
# [[2,2,10],
# [10,1,1],
# [2,10,1]]
A = np.zeros((n, n), float)
print("Построчно введите матрицу коэффициентов A")
for i in range(n):
    A[i] = list(map(float, input(f"A[{i+1}]: ").split()))
    
# [14,12,13]
print("Введите матрицу свободных членов B")
B = np.array(list(map(float, input("B: ").split())), float)

X = np.zeros(n, float)
m = 10
e = 0.01

A, B = transform2diagonal(A, B)
calc_gauss_seidel(A, X, B, m, e)