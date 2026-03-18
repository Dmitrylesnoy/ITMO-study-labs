
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
