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

def runge_rule(e):
    
    return True

def rectangles(fun, a, b, n, type="center"):
    result = 0
    h = (b-a) / n
    for i in range(n):
        if type=="center":
            result += h * fun(a+h/2 + h*i)
        if type=="left":
            result += h * fun(a+h*i)    
        if type=="right":
            result += h * fun(a+h*(i+1))
            
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
    
    
'''
    Реализовать в программе методы по выбору пользователя: 
     Метод прямоугольников (3 модификации: левые, правые, средние) 
     Метод трапеций 
     Метод Симпсона 
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

functions = [function_1,function_2, function_3, function_4]
cfun = None

while 1:
    print()
    print("Выберите опцию: ")
    print("0 - выход")
    print("1 - задать пределы")
    print("2 - задать точность")
    print("3 - выбрать функцию")
    print("4 - вычислить интеграл")
    
    choice = input2num("Ваш выбор: ", True, 1, 4)
    print()

    match choice:
        case 1:
            a = input2num("Введите левый предел интегррирования a: ")
            b = input2num("Введите правый предел интегррирования b: ")
        case 2:
            e = input2num("Введите точность вычислений: ")
        case 3:
            
            print('''1. Непрерывная функция, парабола 
                        f(x) = x² + 2x + 1''')
            print('''2.Разрыв 1-го рода в точке x = 1
                        f(x) = { x², если x < 1 
                               { x + 1, если x ≥ 1 ''')
            print('''3. Разрыв 2-го рода в точке x = 2
                        f(x) = 1/(x - 2) + x''')
            print('''4. Устранимый разрыв в точке x = 0
                        f(x) = sin(x)/x при x ≠ 0, f(0) = 1''')

            cfun = functions[input2num("Введите номер функции: ", True, 1, 4)-1] 
        
        
        
        case 0:
            exit()
    
    
    