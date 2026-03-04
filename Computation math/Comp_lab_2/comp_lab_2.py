import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import numpy as np
import json
import pandas as pd
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

# --- МАТЕМАТИЧЕСКИЙ БЛОК  ---
class MathCore:
    # функции и их производные 
    @staticmethod
    def f1(x): return 2*x**3 + 3.41*x**2 - 23.74*x + 2.95
    @staticmethod
    def f1_der(x): return 6*x**2 + 6.82*x - 23.74
    @staticmethod
    def f1_phi(x): return (23.74*x - 2*x**3 - 3.41*x**2) / 23.74 + x
    
    @staticmethod
    def f2(x): return -1.8*x**3 - 2.94*x**2 + 10.37*x + 5.38
    @staticmethod
    def f2_der(x): return -5.4*x**2 - 5.88*x +10.37
    @staticmethod
    def f2_phi(x): return (1.8*x**3 + 2.94*x**2 - 5.38) / 10.37
    
    @staticmethod
    def f3(x): return x**3-1.89*x**2-2*x+1.76
    @staticmethod
    def f3_der(x): return 3*x**2 - 3.78*x-2
    @staticmethod
    def f3_phi(x): return (x**3 - 2*x + 1.76) / 1.89
    
    @staticmethod
    def f4(x): return np.sin(x) - 0.1*x**2
    @staticmethod
    def f4_der(x): return np.cos(x) - 0.2*x
    @staticmethod
    def f4_phi(x): return np.sqrt(10 * np.sin(x)) if np.sin(x) >= 0 else -np.sqrt(-10 * np.sin(x))
    
    @staticmethod
    def f5(x): return np.exp(x) - 2*x - 3
    @staticmethod
    def f5_der(x): return np.exp(x) - 2
    @staticmethod
    def f5_phi(x): return (np.exp(x) - 3) / 2
    
    # системы и их преобразованный вид
    @staticmethod
    def sys1(x, y):
        return np.sin(x) + 2*y - 2, x + np.cos(y) - 1
    
    @staticmethod
    def sys1_jacobian(x, y):
        return np.array([[np.cos(x), 2], [1, -np.sin(y)]])
    
    @staticmethod
    def sys2(x, y):
        return x**2 + y**2 - 4, x*y - 1
    
    @staticmethod
    def sys2_jacobian(x, y):
        return np.array([[2*x, 2*y], [y, x]])
    
    # Методы решения уравнений
    @staticmethod
    def solve_bisection(f, a, b, eps, max_iter=100):
        history = []
        if f(a) * f(b) > 0:
            raise ValueError("На концах интервала функция должна иметь разные знаки")
        
        for i in range(max_iter):
            c = (a + b) / 2
            fc = f(c)
            err = (b - a) / 2
            
            history.append({
                "iter": i+1, 
                "a": a, 
                "b": b, 
                "x": c, 
                "f(x)": fc, 
                "error": err
            })
            
            if err < eps or abs(fc) < eps:
                break
                
            if f(a) * fc < 0:
                b = c
            else:
                a = c
                
        return c, history
    
    @staticmethod
    def solve_secant(f, x0, x1, eps, max_iter=100):
        history = []
        x_prev, x_curr = x0, x1
        
        for i in range(max_iter):
            f_prev = f(x_prev)
            f_curr = f(x_curr)
            
            if abs(f_curr - f_prev) < 1e-15:
                break
                
            x_next = x_curr - f_curr * (x_curr - x_prev) / (f_curr - f_prev)
            err = abs(x_next - x_curr)
            
            history.append({
                "iter": i+1,
                "x_prev": x_prev,
                "x_curr": x_curr,
                "f(x_prev)": f_prev,
                "f(x_curr)": f_curr,
                "x_next": x_next,
                "error": err
            })
            
            if err < eps or abs(f(x_next)) < eps:
                x_curr = x_next
                break
                
            x_prev, x_curr = x_curr, x_next
            
        return x_curr, history
    
    @staticmethod
    def solve_simple_iteration(phi, x0, eps, max_iter=100):
        history = []
        x_curr = x0
        
        for i in range(max_iter):
            x_next = phi(x_curr)
            err = abs(x_next - x_curr)
            
            history.append({
                "iter": i+1,
                "x": x_curr,
                "phi(x)": x_next,
                "error": err
            })
            
            if err < eps:
                x_curr = x_next
                break
                
            x_curr = x_next
            
        return x_curr, history
    
    # Метод решения систем
    @staticmethod
    def solve_newton_system(F, J, x0, y0, eps, max_iter=100):
        history = []
        x, y = x0, y0
        
        for i in range(max_iter):
            f1, f2 = F(x, y)
            jac = J(x, y)
            
            try:
                det = jac[0,0] * jac[1,1] - jac[0,1] * jac[1,0]
                dx = (jac[1,1] * f1 - jac[0,1] * f2) / det
                dy = (-jac[1,0] * f1 + jac[0,0] * f2) / det
            except:
                break
                
            x_new, y_new = x - dx, y - dy
            err = max(abs(x_new - x), abs(y_new - y))
            
            history.append({
                "iter": i+1,
                "x": x,
                "y": y,
                "f1(x,y)": f1,
                "f2(x,y)": f2,
                "error": err
            })
            
            if err < eps:
                x, y = x_new, y_new
                break
                
            x, y = x_new, y_new
            
        return (x, y), history


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

    def draw_equation(self, func, func_name, x_range, color='b-', linewidth=2):
        x = np.linspace(x_range[0], x_range[1], 400)
        y = func(x)
        self.ax.plot(x, y, color, linewidth=linewidth, label=f'f(x) = {func_name}')

    def draw_system(self, system_func, x_range, y_range):
        x = np.linspace(x_range[0], x_range[1], 100)
        y = np.linspace(y_range[0], y_range[1], 100)
        X, Y = np.meshgrid(x, y)
        Z1, Z2 = system_func(X, Y)

        contour1 = self.ax.contour(X, Y, Z1, levels=[0], colors='blue', linewidths=2)
        contour2 = self.ax.contour(X, Y, Z2, levels=[0], colors='red', linewidths=2)
        self.ax.clabel(contour1, inline=True, fontsize=10)
        self.ax.clabel(contour2, inline=True, fontsize=10)
        self.ax.axis('equal')

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
        
        self.equations = {
            "2x³ + 3.41x² - 23.74x + 2.95": {"f": MathCore.f1, "der": MathCore.f1_der, "phi": MathCore.f1_phi},
            "-1.8x³ - 2.94x² + 10.37x + 5.38": {"f": MathCore.f2, "der": MathCore.f2_der, "phi": MathCore.f2_phi},
            "x³ - 1.89x² - 2x + 1.76": {"f": MathCore.f3, "der": MathCore.f3_der, "phi": MathCore.f3_phi},
            "sin(x) - 0.1x²": {"f": MathCore.f4, "der": MathCore.f4_der, "phi": MathCore.f4_phi},
            "eˣ - 2x - 3": {"f": MathCore.f5, "der": MathCore.f5_der, "phi": MathCore.f5_phi}
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
    
    def solve_system(self, eps):
        sys_name = self.func_combo.get()
        sys_data = self.systems[sys_name]
        
        x0 = float(self.entry_a.get())
        y0 = float(self.entry_b.get())
        
        result, self.history = MathCore.solve_newton_system(
            sys_data["F"], sys_data["J"], x0, y0, eps
        )
        
        for rec in self.history:
            self.tree.insert('', 'end', values=(
                rec['iter'],
                f"{rec['x']:.6f}",
                f"{rec['y']:.6f}",
                f"{rec['f1(x,y)']:.6f}",
                f"{rec['error']:.6f}"
            ))
        
        # Обновление графика с отмеченным решением
        self.update_plot()
        self.graphics.mark_point(result[0], result[1], color='go', markersize=8,
                                 label=f'Решение: ({result[0]:.4f}, {result[1]:.4f})')
        self.graphics.setup_plot(
            title=f'Система: {sys_name}',
            xlabel='x',
            ylabel='y',
            legend=True
        )
        self.graphics.refresh()
        
        messagebox.showinfo("Результат", f"Найдено решение:\nx = {result[0]:.6f}\n"
                            f"y = {result[1]:.6f}\nКоличество итераций: {len(self.history)}")
    
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