import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import numpy as np
import json
import math as m
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

# --- МАТЕМАТИЧЕСКИЙ БЛОК  ---
class MathCore:
    # функции 
    @staticmethod
    def func_linear(x, a, b):
        return a*x+b
    @staticmethod
    def func_poly2(x, a, b, c):
        return a*x**2+b*x+c
    @staticmethod
    def func_poly2(x, a, b, c, d):
        return a*x**3+b*x**2+c*x+d
    @staticmethod
    def func_exp(x, a, b):
        return a*m.exp(b*x)
    @staticmethod
    def func_exp_lin(x, a, b):
        return m.ln(a)+b*x
    @staticmethod
    def func_log(x, a, b):
        return a*m.ln(x)+b
    @staticmethod
    def func_pow(x, a, b):
        return a*x**b
    @staticmethod
    def func_pow_lin(x, a, b):
        return m.ln(a)+b*m.ln(x)
    
    # Решение СЛАУ
    @staticmethod
    def transform2diagonal(A, B):
        """Преобразование матрицы в диагональную"""
        n = len(A)
        A_new = A.copy()
        B_new = B.copy()
        diagonal_count = 0
        for i in range(n):
            max_e = i + np.argmax(np.abs(A_new[i:, i]))
            if max_e != i:
                A_new[[max_e, i]] = A_new[[i, max_e]]
                B_new[max_e], B_new[i] = B_new[i], B_new[max_e]
                diagonal_count += 1
            
        for i in range(len(A)):
            if abs(A[i,i]) <= np.sum(np.abs(A[i])) - abs(A[i,i]):
                messagebox.showwarning("Внимание: отсутствует диагональное преобладание")
                break
        
        return A_new, B_new

    @staticmethod
    def calc_gauss_seidel(A_inp, X, B_inp, m=100, e=1e-8):
        """Решение СЛАУ Методом Гаусса-Зейделя

        Args:
            A (float): матрица коэффициентов
            X (float): матрица решений
            B (float): матрица свободных членов
            m (int, optional): макс. колическо итераций приближений. Defaults to 10.
            e (float, optional): допустимая погрешность. Defaults to 1e-2.
        """
        A, B = self.transform2diagonal(A_inp, B_inp)
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

    
    # Методы решения уравнений



# --- ГРАФИЧЕСКИЙ МЕНЕДЖЕР ---
class GraphicsManager:
    def __init__(self, parent_frame):
        self.fig, self.ax = plt.subplots(figsize=(8, 6), dpi=100)
        self.canvas = FigureCanvasTkAgg(self.fig, master=parent_frame)
        self.canvas.get_tk_widget().pack(fill='both', expand=True)

    def clear(self):
        self.ax.clear()

    def setup_plot(self, title, xlabel, ylabel, grid=True, legend=True, axes_lines=True):
        self.ax.set_title(title)
        self.ax.set_xlabel(xlabel)
        self.ax.set_ylabel(ylabel)
        if grid:
            self.ax.grid(True, alpha=0.3)
        if axes_lines:
            self.ax.axhline(0, color='k', linewidth=0.5, linestyle='-')
            self.ax.axvline(0, color='k', linewidth=0.5, linestyle='-')
        if legend:
            self.ax.legend()

    def draw_function(self, func, func_name, x_range, color='b-', linewidth=2):
        x = np.linspace(x_range[0], x_range[1], 400)
        y = func(x)
        self.ax.plot(x, y, color, linewidth=linewidth, label=f'f(x) = {func_name}')


    def mark_point(self, x, y, color='ro', markersize=8, label=None):
        self.ax.plot(x, y, color, markersize=markersize, label=label)

    def refresh(self):
        self.canvas.draw()

    def get_axes(self):
        return self.ax


# --- ОСНОВНОЕ ПРИЛОЖЕНИЕ ---
class NumericalMethodsApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Численные методы решения нелинейных уравнений и систем")
        self.geometry("1300x800")
        
        self.functions = {
            "Линейная функция": {"f": MathCore.f1, "der": MathCore.f1_der, "phi": MathCore.f1_phi},
            "Полином 2 степени": {"f": MathCore.f2, "der": MathCore.f2_der, "phi": MathCore.f2_phi},
            "Полином 3 степени": {"f": MathCore.f3, "der": MathCore.f3_der, "phi": MathCore.f3_phi},
            "Логарифмическая функция": {"f": MathCore.f4, "der": MathCore.f4_der, "phi": MathCore.f4_phi},
            "Экспоненциальная функция": {"f": MathCore.f5, "der": MathCore.f5_der, "phi": MathCore.f5_phi}

        }
        
        self.systems = {
            "sin(x) + 2y = 2, x + cos(y) = 1": {"F": MathCore.sys1, "J": MathCore.sys1_jacobian},
            "x² + y² = 4, xy = 1": {"F": MathCore.sys2, "J": MathCore.sys2_jacobian}
        }
        
        self.equation_methods = {
            "Метод половинного деления": "bisection",
            "Метод секущих": "secant",
            "Метод простой итерации": "simple_iter"
        }
        
        self.history = []
        self.create_widgets()
    
    def create_widgets(self):
        left_frame = ttk.Frame(self, padding="10")
        left_frame.pack(side=tk.LEFT, fill=tk.BOTH, expand=False)
        
        # выбор типа задачи
        ttk.Label(left_frame, text="Тип задачи:", font=('Arial', 10, 'bold')).pack(anchor='w', pady=(0,5))
        self.task_var = tk.StringVar(value="equation")
        ttk.Radiobutton(left_frame, text="Уравнение", variable=self.task_var, 
                        value="equation", command=self.sync_ui).pack(anchor='w')
        ttk.Radiobutton(left_frame, text="Система", variable=self.task_var, 
                        value="system", command=self.sync_ui).pack(anchor='w')
        
        # Выбор функции/системы
        ttk.Label(left_frame, text="\nВыберите функцию/систему:", font=('Arial', 10, 'bold')).pack(anchor='w')
        self.func_combo = ttk.Combobox(left_frame, state="readonly", width=45)
        self.func_combo.pack(fill='x', pady=5)
        self.func_combo.bind('<<ComboboxSelected>>', lambda e: self.update_plot())
        
        # Выбор метода решения
        ttk.Label(left_frame, text="\nМетод решения:", font=('Arial', 10, 'bold')).pack(anchor='w')
        self.method_combo = ttk.Combobox(left_frame, state="readonly", width=45)
        self.method_combo.pack(fill='x', pady=5)
        
        # Входные данные
        params_frame = ttk.LabelFrame(left_frame, text="Входные данные", padding="10")
        params_frame.pack(fill='x', pady=10)
        
        ttk.Label(params_frame, text="a / x_0:").grid(row=0, column=0, sticky='w', pady=2)
        self.entry_a = ttk.Entry(params_frame)
        self.entry_a.grid(row=0, column=1, sticky='ew', pady=2, padx=(5,0))
        
        ttk.Label(params_frame, text="b / x_1 / y_0:").grid(row=1, column=0, sticky='w', pady=2)
        self.entry_b = ttk.Entry(params_frame)
        self.entry_b.grid(row=1, column=1, sticky='ew', pady=2, padx=(5,0))
        
        ttk.Label(params_frame, text="Точность e:").grid(row=2, column=0, sticky='w', pady=2)
        self.entry_eps = ttk.Entry(params_frame)
        self.entry_eps.insert(0, "0.001")
        self.entry_eps.grid(row=2, column=1, sticky='ew', pady=2, padx=(5,0))
        
        params_frame.columnconfigure(1, weight=1)
        
        # Кнопки
        btn_frame = ttk.Frame(left_frame)
        btn_frame.pack(fill='x', pady=10)
        
        ttk.Button(btn_frame, text="Загрузить из JSON", command=self.load_json).pack(fill='x', pady=2)
        ttk.Button(btn_frame, text="РЕШИТЬ", command=self.solve, style='Accent.TButton').pack(fill='x', pady=2)
        self.save_btn = ttk.Button(btn_frame, text="Сохранить результаты", command=self.save_results, state='disabled')
        self.save_btn.pack(fill='x', pady=2)
        
        # Таблица решения
        history_frame = ttk.LabelFrame(left_frame, text="История итераций", padding="5")
        history_frame.pack(fill='both', expand=True, pady=10)
        
        columns = ('iter', 'x', 'y', 'f(x)', 'error')
        self.tree = ttk.Treeview(history_frame, columns=columns, show='headings', height=12)
        
        self.tree.heading('iter', text='Итер')
        self.tree.heading('x', text='x')
        self.tree.heading('y', text='y')
        self.tree.heading('f(x)', text='f(x)')
        self.tree.heading('error', text='Погрешность')
        
        self.tree.column('iter', width=50)
        self.tree.column('x', width=90)
        self.tree.column('y', width=90)
        self.tree.column('f(x)', width=90)
        self.tree.column('error', width=90)
        
        scrollbar = ttk.Scrollbar(history_frame, orient=tk.VERTICAL, command=self.tree.yview)
        self.tree.configure(yscrollcommand=scrollbar.set)
        
        self.tree.pack(side=tk.LEFT, fill=tk.BOTH, expand=True)
        scrollbar.pack(side=tk.RIGHT, fill=tk.Y)
        
        # график
        right_frame = ttk.Frame(self)
        right_frame.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True)
        self.plot_frame = ttk.Frame(right_frame)
        self.plot_frame.pack(fill=tk.BOTH, expand=True)
        
        self.graphics = GraphicsManager(self.plot_frame)
        
        self.sync_ui()
    
    def sync_ui(self):
        for item in self.tree.get_children():
            self.tree.delete(item)
        
        if self.task_var.get() == "equation":
            self.func_combo['values'] = tuple(self.equations.keys())
            self.method_combo['values'] = tuple(self.equation_methods.keys())
            self.entry_b.config(state='normal')
        else:
            self.func_combo['values'] = tuple(self.systems.keys())
            self.method_combo['values'] = ("Метод Ньютона",)
            self.entry_b.config(state='normal')
        
        if self.func_combo['values']:
            self.func_combo.current(0)
        if self.method_combo['values']:
            self.method_combo.current(0)
        
        self.update_plot()
    
    def _get_plot_range(self):
        try:
            a = float(self.entry_a.get())
            b = float(self.entry_b.get())
            margin = 3 * (b - a)
            if margin == 0:
                margin = 1.0
            x_min = a - margin
            x_max = b + margin
        except ValueError:
            x_min, x_max = -5, 5
        return x_min, x_max
    
    def update_plot(self):
        self.graphics.clear()
        
        if self.task_var.get() == "equation":
            func_name = self.func_combo.get()
            if func_name in self.equations:
                f = self.equations[func_name]["f"]
                x_min, x_max = self._get_plot_range()
                self.graphics.draw_equation(f, func_name, (x_min, x_max))
                self.graphics.setup_plot(
                    title=f'График функции {func_name}',
                    xlabel='x',
                    ylabel='f(x)'
                )
        else:
            sys_name = self.func_combo.get()
            if sys_name in self.systems:
                F = self.systems[sys_name]["F"]
                try:
                    x0 = float(self.entry_a.get())
                    y0 = float(self.entry_b.get())
                    margin = 20.0
                    x_range = (x0 - margin, x0 + margin)
                    y_range = (y0 - margin, y0 + margin)
                except ValueError:
                    x_range = (-3, 3)
                    y_range = (-3, 3)
                self.graphics.draw_system(F, x_range, y_range)
                self.graphics.setup_plot(
                    title=f'Система: {sys_name}',
                    xlabel='x',
                    ylabel='y',
                    legend=False
                )
        
        self.graphics.refresh()
    
    def solve(self):
        try:
            for item in self.tree.get_children():
                self.tree.delete(item)
            
            eps = float(self.entry_eps.get())
            
            if self.task_var.get() == "equation":
                self.solve_equation(eps)
            else:
                self.solve_system(eps)
            
            self.save_btn.config(state='normal')
        except Exception as e:
            messagebox.showerror("Ошибка", str(e))
    
    def solve_equation(self, eps):
        func_name = self.func_combo.get()
        method_name = self.method_combo.get()
        method_key = self.equation_methods[method_name]
        
        f_data = self.equations[func_name]
        f = f_data["f"]
        
        if method_key == "bisection":
            a = float(self.entry_a.get())
            b = float(self.entry_b.get())
            result, self.history = MathCore.solve_bisection(f, a, b, eps)
            for rec in self.history:
                self.tree.insert('', 'end', values=(
                    rec['iter'],
                    f"{rec['x']:.6f}",
                    "-",
                    f"{rec['f(x)']:.6f}",
                    f"{rec['error']:.6f}"
                ))
        
        elif method_key == "secant":
            x0 = float(self.entry_a.get())
            x1 = float(self.entry_b.get())
            result, self.history = MathCore.solve_secant(f, x0, x1, eps)
            for rec in self.history:
                self.tree.insert('', 'end', values=(
                    rec['iter'],
                    f"{rec['x_next']:.6f}",
                    "-",
                    f"{rec.get('f(x_curr)', 0):.6f}",
                    f"{rec['error']:.6f}"
                ))
        
        elif method_key == "simple_iter":
            x0 = float(self.entry_a.get())
            phi = f_data["phi"]
            result, self.history = MathCore.solve_simple_iteration(phi, x0, eps)
            for rec in self.history:
                self.tree.insert('', 'end', values=(
                    rec['iter'],
                    f"{rec['x']:.6f}",
                    "-",
                    f"{rec.get('phi(x)', 0):.6f}",
                    f"{rec['error']:.6f}"
                ))
        
        # Обновление графика с отмеченным корнем
        self.update_plot()
        self.graphics.mark_point(result, f(result), color='ro', markersize=8, label=f'Корень: x={result:.6f}')
        self.graphics.setup_plot(
            title=f'График функции {func_name}',
            xlabel='x',
            ylabel='f(x)'
        )
        self.graphics.refresh()
        
        messagebox.showinfo("Результат", f"Найден корень: x = {result:.6f}\n"
                            f"Количество итераций: {len(self.history)}")
    
    def load_json(self):
        filename = filedialog.askopenfilename(
            title="Выберите JSON файл",
            filetypes=[("JSON files", "*.json"), ("All files", "*.*")]
        )
        if filename:
            try:
                with open(filename, 'r', encoding='utf-8') as f:
                    data = json.load(f)
                
                self.entry_a.delete(0, 'end')
                self.entry_a.insert(0, str(data.get('a', '')))
                self.entry_b.delete(0, 'end')
                self.entry_b.insert(0, str(data.get('b', '')))
                self.entry_eps.delete(0, 'end')
                self.entry_eps.insert(0, str(data.get('eps', '0.001')))
                
                if task_type := data.get('task_type'):
                    self.task_var.set(task_type)
                    self.sync_ui()
                if 'function_index' in data:
                    func_idx = data['function_index']
                    values = self.func_combo['values']
                    if 0 <= func_idx < len(values):
                        self.func_combo.current(func_idx)
                if self.task_var.get() == "equation" and 'method_index' in data:
                    method_idx = data['method_index']
                    methods = self.method_combo['values']
                    if 0 <= method_idx < len(methods):
                        self.method_combo.current(method_idx)
                
                self.update_plot()
                messagebox.showinfo("Успех", "Данные загружены из файла")
            except Exception as e:
                messagebox.showerror("Ошибка", f"Не удалось загрузить файл: {e}")
    
    def save_results(self):
        filename = filedialog.asksaveasfilename(
            title="Сохранить результаты",
            defaultextension=".json",
            filetypes=[("JSON files", "*.json"), ("CSV files", "*.csv"), ("All files", "*.*")]
        )
        if filename:
            try:
                if filename.endswith('.csv'):
                    df = pd.DataFrame(self.history)
                    df.to_csv(filename, index=False, encoding='utf-8')
                else:
                    with open(filename, 'w', encoding='utf-8') as f:
                        json.dump(self.history, f, indent=4, ensure_ascii=False)
                messagebox.showinfo("Успех", "Результаты сохранены в файл")
            except Exception as e:
                messagebox.showerror("Ошибка", f"Не удалось сохранить файл: {e}")


app = NumericalMethodsApp()
app.mainloop()