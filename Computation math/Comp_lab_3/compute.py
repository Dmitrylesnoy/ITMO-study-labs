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
    return x**2 + 2*x + 1

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
    if abs(x - 2) < 1e-10:  # проверка на точку разрыва
        return float('inf')
    return 1/(x - 2) + x

def function_4(x) -> float:
    """
    Устранимый разрыв в точке x = 0
    f(x) = sin(x)/x при x ≠ 0, f(0) = 1
    """
    if abs(x) < 1e-10:  # в близи к нуля
        return 1.0
    return np.sin(x)/x
    

def left_rectangles(fun, a, b, n):
    result = 0
    h = (b-a) / n
    for i in range(n):
        result += h * fun(a+h*i)    
    return result

def right_rectangles(fun, a, b, n):
    result = 0
    h = (b-a) / n
    for i in range(n): 
        result += h * fun(a+h*(i+1))
    return result

def center_rectangles(fun, a, b, n):
    result = 0
    h = (b-a) / n
    for i in range(n):
        result += h * fun(a+h/2 + h*i)
    return result

def trapezoid(fun, a, b, n):  # If n<3 ?
    if n < 1:
        return 0
    
    result = 0
    h = (b-a) / n
    for i in range(1, n):
        result += fun(a+h*i)        
    result += (fun(a) + fun(b))/2
    
    return result * h

def simpson(fun, a, b, n):
    if n % 2 != 0:
        n = n if n % 2 == 0 else n + 1
                
    result = 0
    h = (b-a) / n
    for i in range(n+1):
        y_i = fun(a+h*i)
        if i%2==0:
            result += 2 * y_i          
        else:
            result += 4 * y_i
            
    return result* h/3


def runge_rule(I1, I0, k, e):
    R = abs(I1 - I0) / (2**k - 1)
    return R < e

def calucale_integral(fun, method, k, a, b, e, logs=False, max_iter=100_000):
    """
    Вычисление интеграла с заданной точностью
    fun - интегрируемая функция
    method - метод интегрирования (функция)
    k - порядок точности метода
    a, b - пределы интегрирования
    e - требуемая точность
    """
    
    n = 4
    i = 0
    integrals = []
    
    while i < max_iter:
        try:
            current_integral = method(fun, a, b, n)
            integrals.append(current_integral)
        except Exception as ex:
            print(f"Ошибка при вызове метода: {ex}")
            print(f"method={method}, fun={fun}, a={a}, b={b}, n={n}")
            raise
        
        if logs:
            print(f"[{i}]: n={n}, integral={current_integral}")
        
        if i >= 1:
            if runge_rule(current_integral, integrals[-2], k, e):
                if logs:
                    print(f"Достигнута точность на итерации {i}, n={n}")
                break
        
        n *= 2
        i += 1
    
    return integrals[-1]

class Task:
    def __init__(self):
        self.methods = {"left_rectangles": (left_rectangles, 2),
                        "right_rectangles": (right_rectangles, 2),
                        "center_rectangles": (center_rectangles, 2),
                        "trapezoid": (trapezoid, 3),
                        "simpson": (simpson, 4)}
        self.method_func = None  # Переименуем для ясности
        self.method_k = None
        self.fun = None
        self.a = None
        self.b = None
        self.e = None
        
    def set_limits(self, a, b):
        self.a = a
        self.b = b
        
    def set_e(self, e):
        self.e = e
        
    def set_fun(self, fun):
        self.fun = fun
        
    def set_method(self, method_name):
        self.method_func, self.method_k = self.methods[method_name]
        
    def get_methods_names(self):
        return list(self.methods.keys())
    
    def solve(self, logs=False, max_iter=100_000):
        if None in (self.fun, self.method_func, self.method_k, self.a, self.b, self.e):
            missing = []
            if self.fun is None: missing.append("функция")
            if self.method_func is None: missing.append("метод")
            if self.method_k is None: missing.append("k")
            if self.a is None: missing.append("a")
            if self.b is None: missing.append("b")
            if self.e is None: missing.append("e")
            print(f"Ошибка: не установлены параметры: {', '.join(missing)}")
            return None
            
        print(f"Вычисление интеграла с параметрами:")
        print(f"  fun: {self.fun.__name__}")
        print(f"  method_func: {self.method_func.__name__}")
        print(f"  k: {self.method_k}")
        print(f"  a: {self.a}, b: {self.b}, e: {self.e}")
        
        return calucale_integral(
            self.fun, 
            self.method_func,
            self.method_k, 
            self.a, 
            self.b, 
            self.e, 
            logs, 
            max_iter
        )
    
'''
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
'''


a, b = 0.0, 0.0
n = 4
e = 1.0
user_task = Task()

functions = [(function_1, """Непрерывная функция, парабола 
                        f(x) = x² + 2x + 1"""),
             (function_2, """Разрыв 1-го рода в точке x = 1 
                        f(x) = { x², если x < 1 
                               { x + 1, если x ≥ 1 """),
             (function_3, """Разрыв 2-го рода в точке x = 2
                        f(x) = 1/(x - 2) + x"""), 
             (function_4, """Устранимый разрыв в точке x = 0 
                        f(x) = sin(x)/x при x ≠ 0, f(0) = 1""")]

cfun = None

while 1:
    print()
    print("Выберите опцию: ")
    print("0 - выход")
    print("1 - задать пределы")
    print("2 - задать точность")
    print("3 - выбрать функцию")
    print("4 - выбрать метод")
    print("5 - посмотреть введённые параметры")
    print("6 - вычислить интеграл")
    
    choice = input2num("Ваш выбор: ", True, 0, 6)
    print()
    

    match choice:
        case 1:
            a = input2num("Введите левый предел интегррирования a: ")
            b = input2num("Введите правый предел интегррирования b: ")
            user_task.set_limits(a,b)
        case 2:
            user_task.set_e(input2num("Введите точность вычислений: "))
        case 3:
            for i in range(len(functions)):
                print(f"{i}: {functions[i][1]}")

            user_task.set_fun(functions[input2num("Введите номер функции: ", True, 0, 3)][0] )
        case 4:
            methods_names = user_task.get_methods_names()
            for i, name in enumerate(methods_names):
                print(f"{i}: {name}")
            choice = input2num("Введите номер метода: ", True, 0, len(methods_names)-1)
            user_task.set_method(methods_names[choice])
        case 5:
            print("выбранные парамеры:")

        case 6:
            integral = user_task.solve(logs=True)
            if integral is not None:
                print(f"Найденный приближенный интеграл: {integral}")
                
        case 0:
            exit()
    
    
    