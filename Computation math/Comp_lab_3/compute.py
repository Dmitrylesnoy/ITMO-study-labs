import numpy as np
import json


def input2num(prompt, is_int=False, min_value=None, max_value=None):
    """Валидация численного ввода"""
    while True:
        user_input = input(prompt).strip().replace(",", ".")
        if not user_input:
            print("Ошибка: ввод не может быть пустым")
            continue
        if not user_input.isdigit() and is_int:
            print("Ошибка: введите целое число")
            continue
        value = float(user_input)
        if min_value is not None and value < min_value:
            print(f"Ошибка: значение должно быть >= {min_value}")
            continue
        if max_value is not None and value > max_value:
            print(f"Ошибка: значение должно быть <= {max_value}")
            continue
        return int(value) if is_int else value


def function_1(x) -> float:
    """
    Непрерывная функция, парабола
    f(x) = x² + 2x + 1
    """
    return x**2 + 2 * x + 1


def function_2(x) -> float:
    """
    Разрыв 1-го рода в точке x = 1
    f(x) = { x², если x < 1
           { x + 1, если x ≥ 1
    """
    if x < 1:
        return x**2
    else:
        return x + 1


def function_3(x) -> float:
    """
    Разрыв 2-го рода в точке x = 2
    f(x) = 1/(x - 2) + x
    """
    if abs(x - 2) < 1e-10:
        return None
    return 1 / (x - 2) + x


def function_4(x) -> float:
    """
    Устранимый разрыв в точке x = 0
    f(x) = sin(x)/x при x ≠ 0, f(0) = 1
    """
    if abs(x) < 1e-10:
        return 1.0
    return np.sin(x) / x


def function_5(x) -> float:
    """
    Интеграл функции не решается в Wolfram
    f(x) = sin(e^(cos(sqrt(x)))) / x
    """
    try:
        return np.sin(np.exp(np.cos(np.sqrt(abs(x))))) / (x + 1e-9)
    except:
        return None


def left_rectangles(fun, a, b, n):
    h = (b - a) / n
    result = sum(fun(a + i * h) for i in range(n))
    return result * h


def right_rectangles(fun, a, b, n):
    h = (b - a) / n
    result = sum(fun(a + (i + 1) * h) for i in range(n))
    return result * h


def center_rectangles(fun, a, b, n):
    h = (b - a) / n
    result = sum(fun(a + h / 2 + i * h) for i in range(n))
    return result * h


def trapezoid(fun, a, b, n):
    h = (b - a) / n
    result = (fun(a) + fun(b)) / 2
    for i in range(1, n):
        result += fun(a + h * i)
    return result * h


def simpson(fun, a, b, n):
    if n % 2 != 0: n += 1                
    h = (b-a) / n
    result = fun(a) + fun(b)
    for i in range(1, n):
        y_i = fun(a+h*i)
        if i%2==0:
            result += 2 * y_i          
        else:
            result += 4 * y_i

    return result * h / 3


def calucale_integral(fun, method, k, a, b, e, max_iter=25):
    """
    Вычисление интеграла с заданной точностью
    fun - интегрируемая функция
    method - метод интегрирования (функция)
    k - порядок точности метода
    a, b - пределы интегрирования
    e - требуемая точность
    """

    # TODO: Обработка разрыва 2 рода
    # TODO: если выйти из области определния - не решается

    n = 4
    i = 0

    eps = 1e-11
    s_a, s_b = a + eps, b - eps
    if s_a >= s_b: return 0, n

    old_I = method(fun, s_a, s_b, n)
    while i < max_iter:
        n *= 2
        curr_I = method(fun, s_a, s_b, n)

        if np.isinf(curr_I) or np.isnan(curr_I):
            print("Ошибка: Интеграл расходится (бесконечное значение)")
            return None, n

        if abs(curr_I-old_I) / (2**k - 1) < e:
            return curr_I, n

        old_I = curr_I
        i += 1
        print(f"[{i}]: n={n}, integral={curr_I}")

    return old_I, n


def find_discontinuity(fun, a, b, steps=10000):
    """ Поиск точек разрыва """
    discont = []
    dx = (b - a) / steps
    threshold = 1e4

    prev_y = None
    for i in range(steps+1):
        x = a + i * dx
        try:
            y = fun(x)
            if y is None or np.isinf(y) or np.isnan(y) or abs(y) > threshold:
                discont.append(x)
            elif prev_y is not None and abs(prev_y - y) > threshold:
                discont.append(x)
        except (ZeroDivisionError, ValueError):
            l, r = x - dx, x
            for _ in range(30):
                m = (l + r) / 2
                try:
                    val = fun(m)
                    if val is None or abs(val) > 1e10: raise ValueError
                    l = m
                except:
                    r = m
            discont.append(r)
            prev_y = None

    unique = []
    if discont:
        discont.sort()
        unique.append(discont[0])
        for p in discont[1:]:
            if p - unique[-1] > (b-a) / 100:
                unique.append(p)
    return unique


def smart_integral(fun, method, k, a, b, e):
    print("Анализ интервала на наличие разрывов...")
    points = find_discontinuity(fun, a, b)

    if not points:
        print("Разрывов не обнаружено")
        return calucale_integral(fun, method, k, a, b, e)
    print(f"Обнаружен разрыв в точках: {[round(p, 4) for p in points]}")

    total_val, total_n = 0, 0
    points.sort()
    curr_start = a
    for c in points:
        d_left = c - curr_start
        d_right = b - c

        if d_left > 0 and d_right > 0:
            offset = min(d_left, d_right)
            print(f"Зануление симметричного участка вокруг x={round(c,4)}: [{round(c-offset, 4)}, {round(c+offset, 4)}]")
            if d_left > d_right:
                segment_b = c - offset
                if abs(segment_b - curr_start) > 1e-9:
                    val, n = calucale_integral(fun, method, k, curr_start, segment_b, e / 2)
                    total_val += val
                    total_n += n

            curr_start = c + offset
        elif d_left <= 0:
            curr_start = c + 1e-9
    if b - curr_start > 1e-9:
        val, n = calucale_integral(fun, method, k, curr_start, b, e / 2)
        total_val += val
        total_n += n

    return total_val, total_n


class Task:
    def __init__(self):
        self.methods = {
            "left_rectangles": (left_rectangles, 1),
            "right_rectangles": (right_rectangles, 1),
            "center_rectangles": (center_rectangles, 2),
            "trapezoid": (trapezoid, 2),
            "simpson": (simpson, 4),
        }
        self.method_func = None
        self.method_k = None
        self.fun = None
        self.a = None
        self.b = None
        self.e = None

    def solve(self):
        if None in (self.fun, self.method_func, self.method_k, self.a, self.b, self.e):
            missing = []
            if self.fun is None:
                missing.append("функция")
            if self.method_func is None:
                missing.append("метод")
            if self.method_k is None:
                missing.append("k")
            if self.a is None:
                missing.append("a")
            if self.b is None:
                missing.append("b")
            if self.e is None:
                missing.append("e")
            print(f"Ошибка: не установлены параметры: {', '.join(missing)}")
            return None

        print(f"Вычисление интеграла с параметрами:")
        print(f"  fun: {self.fun.__name__}")
        print(f"  method_func: {self.method_func.__name__}")
        print(f"  k: {self.method_k}")
        print(f"  a: {self.a}, b: {self.b}, e: {self.e}")

        return smart_integral(
            self.fun, self.method_func, self.method_k, self.a, self.b, self.e
        )


"""
    Реализовать в программе методы по выбору пользователя: 
    +? Метод прямоугольников (3 модификации: левые, правые, средние) 
    +? Метод трапеций 
    +? Метод Симпсона 
    2. Методы должны быть оформлены в виде отдельной(ого) функции/класса. 
    3. Вычисление значений функции оформить в виде отдельной(ого) функ
    ции/класса. 
    4. Для оценки погрешности и завершения вычислительного процесса использо
    вать правило Рунге. 
    5. Предусмотреть вывод результатов: значение интеграла, число разбиения интер
    вала интегрирования для достижения требуемой точности. 


    Отсутствие обработки точек разрыва в лабораторной №3 (в случае отсутствия максимальный балл за выполнение - 70%, за защиту - 80%)

    Возможность решения интеграла который имеет разрыв 2 рода и НЕ решается в вольфрам альфа. 
    (Т.е. в случае если левая и правая части интеграла по модулю симметричны относительно точки разрыва и имеют разный знак, 
    например функция 1/x с точкой разрыва 0, то мы должны посчитать его исходя из принципа что мы можем выкинуть точку разрыва. 
    Например a = -1, b = 2 промежуток -1;1 симметричен относительно 0 с разными знаками  его интеграл равен 0, 
    получается нам надо посчитать только интеграл от 1 до 2, который останется после зануления участка -1; 1) 
    (в случае отсутствия максимальный балл за выполнение - 70%, за защиту - 80%)
"""


a, b = 0.0, 0.0
n = 4
e = 1.0
user_task = Task()

functions = [
    (
        function_1,
        """Непрерывная функция, парабола 
                        f(x) = x² + 2x + 1""",
    ),
    (
        function_2,
        """Разрыв 1-го рода в точке x = 1 
                        f(x) = { x², если x < 1 
                               { x + 1, если x ≥ 1 """,
    ),
    (
        function_3,
        """Разрыв 2-го рода в точке x = 2
                        f(x) = 1/(x - 2) + x""",
    ),
    (
        function_4,
        """Устранимый разрыв в точке x = 0 
                        f(x) = sin(x)/x при x ≠ 0, f(0) = 1""",
    ),
    (
        function_5,
        """Интеграл функции не решается в Wolfram
                        f(x) = sin(e^(cos(sqrt(x)))) / x""",
    ),
]

cfun = None

while 1:
    print()
    print("Выберите опцию: ")
    print("0 - выход")
    print("1 - задать пределы")
    print("2 - задать точность")
    print("3 - выбрать функцию")
    print("4 - выбрать метод")
    print("5 - вычислить интеграл")

    choice = input2num("Ваш выбор: ", True, 0, 5)
    print()

    match choice:
        case 1:
            user_task.a = input2num("Введите левый предел интегррирования a: ")
            user_task.b = input2num("Введите правый предел интегррирования b: ")
        case 2:
            user_task.e = input2num("Введите точность вычислений: ")
        case 3:
            for i in range(len(functions)):
                print(f"{i}: {functions[i][1]}")

            user_task.fun = functions[input2num("Введите номер функции: ", True, 0, 4)][
                0
            ]
        case 4:
            methods_names = list(user_task.methods.keys())
            for i, name in enumerate(methods_names):
                print(f"{i}: {name}")
            choice = input2num(
                "Введите номер метода: ", True, 0, len(methods_names) - 1
            )
            user_task.method_func, user_task.method_k = user_task.methods[
                methods_names[choice]
            ]
        case 5:
            integral, solve_n = user_task.solve()
            if integral is not None:
                print(f"Найденный приближенный интеграл: {integral}")
                print(f"Число разбиений (n): {solve_n}")

        case 0:
            exit()
