import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import numpy as np
import json
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg

# --- МАТЕМАТИЧЕСКИЙ БЛОК ---
class MathCore:
    @staticmethod
    def f1(x): return 2*x**3 + 3.41*x**2 - 23.74*x + 2.95
    @staticmethod
    def f1_der(x): return 6*x**2 + 6.82*x - 23.74
    @staticmethod
    def f1_der2(x): return 12*x + 6.82
    
    @staticmethod
    def sys1_phi(x, y): 
        # возвращает новые x и y для метода простых итераций
        return 0.7 - np.cos(y - 1), 1 - 0.5 * np.sin(x)

    @staticmethod
    def solve_newton(f, f_der, f_der2, a, b, eps):
        history = []
        x0 = a if f(a) * f_der2(a) > 0 else b
        cx = x0
        for i in range(100):
            prev_x = cx
            fv = f(cx)
            cx = prev_x - fv / f_der(prev_x)
            err = abs(cx - prev_x)
            history.append({"iter": i+1, "x": prev_x, "f(x)": fv, "error": err})
            if err < eps: break
        return cx, history

    @staticmethod
    def solve_sys_phi(phi_func, x0, y0, eps):
        history = []
        cx, cy = x0, y0
        for i in range(100):
            nx, ny = phi_func(cx, cy)
            err = max(abs(nx - cx), abs(ny - cy))
            history.append({"iter": i+1, "x": cx, "y": cy, "err": err})
            if err < eps: break
            cx, cy = nx, ny
        return (cx, cy), history

# --- ГРАФИЧЕСКИЙ ИНТЕРФЕЙС ---
class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Numerical Methods GUI")
        self.geometry("1100x700")
        self.history = []
        
        # Контейнеры
        self.left_panel = tk.Frame(self, padx=15, pady=15, width=350)
        self.left_panel.pack(side=tk.LEFT, fill=tk.Y)
        
        self.right_panel = tk.Frame(self, bg="white")
        self.right_panel.pack(side=tk.RIGHT, expand=True, fill=tk.BOTH)

        # Сначала создаём график, чтобы он был доступен в update_plot
        self.init_plot()
        self.init_ui()

    def init_ui(self):
        # Выбор задачи
        tk.Label(self.left_panel, text="Тип задачи:", font=('Arial', 10, 'bold')).pack(anchor='w')
        self.task_var = tk.StringVar(value="eq")
        tk.Radiobutton(self.left_panel, text="Уравнение", variable=self.task_var, value="eq", command=self.sync_menu).pack(anchor='w')
        tk.Radiobutton(self.left_panel, text="Система", variable=self.task_var, value="sys", command=self.sync_menu).pack(anchor='w')

        self.obj_list = ttk.Combobox(self.left_panel, state="readonly", width=40)
        self.obj_list.pack(pady=10)
        self.obj_list.bind("<<ComboboxSelected>>", lambda e: self.update_plot())

        # Ввод данных
        tk.Label(self.left_panel, text="\nПараметры (a, b или x0, y0):").pack(anchor='w')
        self.entry_a = tk.Entry(self.left_panel)
        self.entry_a.pack(fill='x')
        self.entry_b = tk.Entry(self.left_panel)
        self.entry_b.pack(fill='x')
        tk.Label(self.left_panel, text="Точность (eps):").pack(anchor='w')
        self.entry_eps = tk.Entry(self.left_panel)
        self.entry_eps.insert(0, "0.01")
        self.entry_eps.pack(fill='x')

        # Кнопки
        tk.Button(self.left_panel, text="Загрузить JSON", command=self.load_json, bg="#e1e1e1").pack(fill='x', pady=5)
        tk.Button(self.left_panel, text="РЕШИТЬ", command=self.run_solve, bg="#4CAF50", fg="white", font=('Arial', 10, 'bold')).pack(fill='x', pady=5)
        self.save_btn = tk.Button(self.left_panel, text="Сохранить JSON", command=self.save_json, state='disabled')
        self.save_btn.pack(fill='x', pady=5)

        self.res_log = tk.Text(self.left_panel, height=12, font=('Consolas', 9))
        self.res_log.pack(fill='both', pady=10)
        
        self.sync_menu()

    def init_plot(self):
        self.fig, self.ax = plt.subplots(figsize=(5, 4), dpi=100)
        self.canvas = FigureCanvasTkAgg(self.fig, master=self.right_panel)
        self.canvas.get_tk_widget().pack(expand=True, fill='both')
    
    def sync_menu(self):
        if self.task_var.get() == "eq":
            self.obj_list['values'] = ("2x^3 + 3.41x^2 - 23.74x + 2.95", "sin(x) - 0.1x^2")
        else:
            self.obj_list['values'] = ("Система: sin(x)+2y=2...",)
        self.obj_list.current(0)
        self.update_plot()

    def update_plot(self):
        self.ax.clear()
        if self.task_var.get() == "eq":
            x = np.linspace(-5, 5, 200)
            idx = self.obj_list.current()
            if idx == 0:
                y = MathCore.f1(x)
            else:
                y = np.sin(x) - 0.1 * x**2
            self.ax.plot(x, y, label="f(x)")
            self.ax.axhline(0, color='black', lw=1)
        self.ax.grid(True)
        self.ax.legend()
        self.canvas.draw()

    def load_json(self):
        path = filedialog.askopenfilename(filetypes=[("JSON", "*.json")])
        if path:
            with open(path, 'r') as f:
                d = json.load(f)
                self.entry_a.delete(0, 'end')
                self.entry_a.insert(0, d.get('a', ''))
                self.entry_b.delete(0, 'end')
                self.entry_b.insert(0, d.get('b', ''))
                self.entry_eps.delete(0, 'end')
                self.entry_eps.insert(0, d.get('eps', '0.01'))

    def run_solve(self):
        try:
            a = float(self.entry_a.get())
            b = float(self.entry_b.get())
            eps = float(self.entry_eps.get())
            
            if self.task_var.get() == "eq":
                res, self.history = MathCore.solve_newton(MathCore.f1, MathCore.f1_der, MathCore.f1_der2, a, b, eps)
                msg = f"Корень: {res:.5f}\nИтераций: {len(self.history)}"
                self.ax.plot(res, MathCore.f1(res), 'ro', label="Root")
                self.ax.legend()
            else:
                res, self.history = MathCore.solve_sys_phi(MathCore.sys1_phi, a, b, eps)
                msg = f"X: {res[0]:.4f}\nY: {res[1]:.4f}\nИтераций: {len(self.history)}"
            
            self.res_log.delete(1.0, 'end')
            self.res_log.insert('end', msg)
            self.canvas.draw()
            self.save_btn.config(state='normal')
        except Exception as e:
            messagebox.showerror("Ошибка", str(e))

    def save_json(self):
        path = filedialog.asksaveasfilename(defaultextension=".json")
        if path:
            with open(path, 'w', encoding='utf-8') as f:
                json.dump(self.history, f, indent=4)
            messagebox.showinfo("Успех", "Результаты сохранены")



app = App()
app.mainloop()