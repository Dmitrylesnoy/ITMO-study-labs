import numpy as np
import json


def validate_int_input(prompt, min_value=None, max_value=None):
    """Валидация целочисленного ввода"""
    while True:
        user_input = input(prompt).strip().replace(",", ".")
        if not user_input:
            print("Ошибка: ввод не может быть пустым")
            continue
        if not user_input.isdigit():
            print("Ошибка: введите целое число")
            continue
        value = int(user_input)
        if min_value is not None and value < min_value:
            print(f"Ошибка: значение должно быть >= {min_value}")
            continue
        if max_value is not None and value > max_value:
            print(f"Ошибка: значение должно быть <= {max_value}")
            continue
        return value


def validate_vector(prompt, expected_cols):
    """Валидация ввода строки матрицы"""
    while True:
        user_input = input(prompt).strip()
        if not user_input:
            print("Ошибка: ввод не может быть пустым")
            continue
        values = user_input.split()
        if len(values) != expected_cols:
            print(f"Ошибка: ожидается {expected_cols} значений, получено {len(values)}")
            continue
        try:
            row = [float(val.replace(",",".")) for val in values]
            return row
        except ValueError:
            print("Ошибка: все элементы должны быть числами")


def is_number(s):
    """Проверяет, можно ли преобразовать строку в число (int или float)"""
    try:
        float(s)
        return True
    except ValueError:
        return False
    

def parse_json_matrix(filename):
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            data = json.load(f)
        
        if not all(k in data for k in ['A', 'B', 'X']):
            print("Ошибка: JSON должен содержать поля 'A', 'B', 'X'")
            return None, None, None
        
        A = np.array(data['A'], dtype=float)
        B = np.array(data['B'], dtype=float)
        X = np.array(data['X'], dtype=float)
        
        n = len(A)
        if A.shape != (n, n) or len(B) != n or len(X) != n:
            print("Ошибка: несоответствие размерностей матриц")
            return None, None, None
            
        return A, B, X
        
    except FileNotFoundError:
        print(f"Ошибка: файл '{filename}' не найден")
    except json.JSONDecodeError as e:
        print(f"Ошибка: некорректный JSON - {e}")
    except Exception as e:
        print(f"Ошибка: {e}")
    
    return None, None, None


def transform2diagonal(A, B):
    """Преобразование матрицы в диагональную"""
    n = len(A)
    A_new = A.copy()
    B_new = B.copy()
    diagonal_count = 0
    for i in range(n):
        max_e = i + np.argmax(np.abs(A_new[i:, i]))  # max element from diagonale in row
        if max_e != i:
            A_new[[max_e, i]] = A_new[[i, max_e]]
            B_new[max_e], B_new[i] = B_new[i], B_new[max_e]
            diagonal_count += 1
        
    for i in range(len(A)):
        if abs(A[i,i]) <= np.sum(np.abs(A[i])) - abs(A[i,i]):
            print("⚠️ Внимание: отсутствует диагональное преобладание")
            break    
    
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

        print(f"Итерация {k}:")
        for i in range(n):
            print(f"    x{i+1} = {X.copy()[i]:.10f}")

        if delta < e:
            print(f"Решение найдено за {k} итераций:")
            print("Вектор неизвестных:")
            for i in range(n):
                print(f"x{i+1} = {X[i]:.10f}")
            break
        else:
            if k < m:
                k = k + 1
            else:
                print("Итерации расходятся")
                break

'''
• Проверка диагонального преобладания (в случае, если диагональное 
преобладание в исходной  матрице отсутствует, сделать перестановку 
строк/столбцов до тех пор, пока преобладание не будет достигнуто). В 
случае невозможности достижения диагонального преобладания - вы
водить соответствующее сообщение. 
• Вывод нормы матрицы (любой, на Ваш выбор), 
• Вывод вектора  погрешностей: |𝑥𝑖(𝑘) − 𝑥𝑖(𝑘−1)|'''
while 1:
    n = validate_int_input("Введите размерность матрицы:", min_value=1)

    print("Выберите способ ввода матрицы: ")
    print("1 - Ввести вручную")
    print("2 - Сгенерировать случайную матрицу")
    print("3 - прочитать данные из файла")
    print("4 - выход")
    
    choice = validate_int_input("Ваш выбор: ", 1, 4)

    m = 10
    e = 0.01

    if choice == 1:
        # [[2,2,10],
        # [10,1,1],
        # [2,10,1]]
        A = np.zeros((n, n), float)
        print("Построчно введите матрицу коэффициентов A")
        for i in range(n):
            A[i] = validate_vector(f"A[{i+1}]: ", n)
            
        # [14,12,13]
        print("Введите матрицу свободных членов B")
        B = validate_vector("B: ", n)
        print("Введите матрицу неизвестных X")
        X = validate_vector("X: ", n)
        print("Введите точность")
        e = validate_vector("e: ", 1)
        
        
    elif choice == 2:
        A = np.random.rand(n, n) * 10 - 5
        B = np.random.rand(n) * 20 - 10
        X = np.random.rand(n) * 20 - 10
        print("Сгенерированная матрица A:")
        print(A)
        print("Сгенерированный вектор B:")
        print(B)
        print("Сгенерированный вектор X:")
        print(X)
        
    elif choice == 3:
        A, B, X = parse_json_matrix(input("Введите имя файла: "))
        print("Сгенерированная матрица A:")
        print(A)
        print("Сгенерированный вектор B:")
        print(B)
        print("Сгенерированный вектор X:")
        print(X)
    elif choice == 4:
        exit()       
    else:
        print("Неверный выбор. Выход.")
        exit(1)


    A, B = transform2diagonal(A, B)
    calc_gauss_seidel(A, X, B, m, e)