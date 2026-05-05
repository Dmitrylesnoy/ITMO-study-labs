def stack_based_calculator(input):
    """Stack-based calculator supporting +, -, *, / operations.

    Uses Reverse Polish Notation (RPN). Examples:
    - "1 1 +" -> 2
    - "1 2 3 4 + * /" -> 0 (integer division, floor)
    - "1 2 + 3 *" -> 9

    - Separator: spaces
    - End of input -- new line.
    - Division by zero returns -1.
    - Overflow returns 0xCCCCCCCC.
    - Invalid expressions return -1.

    Python example args:
        input (str): The input string containing RPN expression.

    Returns:
        tuple: A tuple containing the result as a list and the remaining input.
    """
    line, rest = read_line(input, 0x40)
    if line is None:
        return [overflow_error_value], rest

    if not line.strip():
        return [-1], rest

    try:
        tokens = line.strip().split()
        stack = []

        for token in tokens:
            if token in ["+", "-", "*", "/"]:
                if len(stack) < 2:
                    return [-1], rest  # Not enough operands

                b = stack.pop()
                a = stack.pop()

                if token == "+":
                    result = a + b
                elif token == "-":
                    result = a - b
                elif token == "*":
                    result = a * b
                elif token == "/":
                    if b == 0:
                        return [-1], rest  # Division by zero
                    result = a // b  # Integer division
                else:
                    return [-1], rest

                if result < -2147483648 or result > 2147483647:
                    return [overflow_error_value], rest

                stack.append(result)
            else:
                num = int(token)
                if num < -2147483648 or num > 2147483647:
                    return [overflow_error_value], rest
                stack.append(num)

            print(stack)
        if len(stack) != 1:
            return [-1], rest

        return [stack[0]], rest

    except Exception:
        return [-1], rest


assert stack_based_calculator('1 1 +\n') == ([2], '')
assert stack_based_calculator('1 2 + 3 *\n') == ([9], '')
assert stack_based_calculator('10 3 /\n') == ([3], '')