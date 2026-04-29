import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from math import factorial, sin, cos, exp


# --- МАТЕМАТИЧЕСКИЙ БЛОК ---
class InterpolationCore:
    @staticmethod
    def get_finite_diffs(y):
        n = len(y)
        table = [np.array(y, dtype=float)]
        for j in range(1, n):
            prev = table[j - 1]
            # Конечные разности: Δy_i = y_{i+1} - y_i
            current = np.diff(prev)
            table.append(current)
        return table

    @staticmethod
    def lagrange(x_pts, y_pts, x_val):
        n = len(x_pts)
        res = 0
        for i in range(n):
            p = 1
            for j in range(n):
                if i != j:
                    p *= (x_val - x_pts[j]) / (x_pts[i] - x_pts[j])
            res += y_pts[i] * p
        return res

    @staticmethod
    def newton_forward(x_pts, y_pts, x_val, diffs):
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[0]) / h
        res = y_pts[0]
        term = 1
        for i in range(1, len(x_pts)):
            term *= t - i + 1
            res += (term * diffs[i][0]) / factorial(i)
        return res

    @staticmethod
    def gauss(x_pts, y_pts, x_val, diffs):
        n = len(x_pts)
        mid = n // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h
        res = y_pts[mid]
        term = 1
        # Первая формула Гаусса (для t > 0)
        for i in range(1, n):
            k = i // 2
            if i % 2 != 0:
                term *= t - k
                idx = mid - k
            else:
                term *= t + k
                idx = mid - k
            if 0 <= idx < len(diffs[i]):
                res += (term * diffs[i][idx]) / factorial(i)
        return res

    @staticmethod
    def stirling(x_pts, y_pts, x_val, diffs):
        n = len(x_pts)
        mid = n // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h
        res = y_pts[mid]
        t2 = t**2
        # Упрощенная реализация до 4-й разности
        try:
            d1 = (diffs[1][mid - 1] + diffs[1][mid]) / 2
            res += t * d1 + (t2 / 2) * diffs[2][mid - 1]
            d3 = (diffs[3][mid - 2] + diffs[3][mid - 1]) / 2
            res += (t * (t2 - 1) / 6) * d3 + (t2 * (t2 - 1) / 24) * diffs[4][mid - 2]
        except:
            pass
        return res

    @staticmethod
    def bessel(x_pts, y_pts, x_val, diffs):
        n = len(x_pts)
        mid = (n - 1) // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h
        # Базовая формула Бесселя
        res = (y_pts[mid] + y_pts[mid + 1]) / 2 + (t - 0.5) * diffs[1][mid]
        try:
            d2 = (diffs[2][mid - 1] + diffs[2][mid]) / 2
            res += (t * (t - 1) / 2) * d2
        except:
            pass
        return res


# --- ИНТЕРФЕЙС ---
class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Лабораторная №5. Интерполяция.")
        self.geometry("1100x800")
        self._setup_ui()

    def _setup_ui(self):
        sidebar = ttk.Frame(self, padding=10)
        sidebar.pack(side="left", fill="y")

        # Ввод данных
        ttk.Label(sidebar, text="1. Выбор ввода:", font="bold").pack(anchor="w")
        self.input_mode = tk.StringVar(value="manual")
        ttk.Radiobutton(
            sidebar, text="Ручной (X Y)", variable=self.input_mode, value="manual"
        ).pack(anchor="w")
        ttk.Radiobutton(
            sidebar, text="Функция", variable=self.input_mode, value="func"
        ).pack(anchor="w")

        self.txt = tk.Text(sidebar, height=8, width=25)
        self.txt.insert("1.0", "0.1 1.25\n0.2 2.38\n0.3 3.79\n0.4 5.44\n0.5 7.14")
        self.txt.pack(pady=5)

        self.func_box = ttk.Combobox(sidebar, values=["sin(x)", "exp(x)", "x^2"])
        self.func_box.set("sin(x)")
        self.func_box.pack(fill="x")

        self.range_entry = ttk.Entry(sidebar)
        self.range_entry.insert(0, "0 1 10")  # a b n
        self.range_entry.pack(fill="x", pady=2)

        ttk.Button(sidebar, text="Загрузить из файла", command=self.load_file).pack(
            fill="x"
        )

        ttk.Label(sidebar, text="2. Точка аргумента (X):").pack(
            anchor="w", pady=(10, 0)
        )
        self.target_x = ttk.Entry(sidebar)
        self.target_x.insert(0, "0.25")
        self.target_x.pack(fill="x")

        ttk.Button(sidebar, text="ВЫЧИСЛИТЬ", command=self.run).pack(fill="x", pady=10)

        self.res_txt = tk.Text(sidebar, height=12, width=30, state="disabled")
        self.res_txt.pack()

        # График
        self.fig, self.ax = plt.subplots(figsize=(5, 4))
        self.canvas = FigureCanvasTkAgg(self.fig, master=self)
        self.canvas.get_tk_widget().pack(side="right", fill="both", expand=True)

    def load_file(self):
        path = filedialog.askopenfilename()
        if path:
            with open(path, "r") as f:
                self.txt.delete("1.0", tk.END)
                self.txt.insert(tk.END, f.read())

    def show_diff_table(self, x, diffs):
        win = tk.Toplevel(self)
        win.title("Таблица конечных разностей")
        cols = ["x", "y"] + [f"Δ^{i}y" for i in range(1, len(diffs))]
        tree = ttk.Treeview(win, columns=cols, show="headings")
        for c in cols:
            tree.heading(c, text=c)
            tree.column(c, width=80)
        for i in range(len(x)):
            row = [f"{x[i]:.3f}"]
            for level in range(len(diffs)):
                if i < len(diffs[level]):
                    row.append(f"{diffs[level][i]:.4f}")
                else:
                    row.append("")
            tree.insert("", "end", values=row)
        tree.pack(fill="both", expand=True)

    def run(self):
        try:
            if self.input_mode.get() == "manual":
                lines = self.txt.get("1.0", tk.END).strip().split("\n")
                pts = [list(map(float, l.split())) for l in lines if l.strip()]
                x = np.array([p[0] for p in pts])
                y = np.array([p[1] for p in pts])
            else:
                a, b, n = map(float, self.range_entry.get().split())
                x = np.linspace(a, b, int(n))
                f_name = self.func_box.get()
                y = (
                    np.sin(x)
                    if "sin" in f_name
                    else (np.exp(x) if "exp" in f_name else x**2)
                )

            t_x = float(self.target_x.get())
            diffs = InterpolationCore.get_finite_diffs(y)

            # Расчет всеми методами
            v_lagr = InterpolationCore.lagrange(x, y, t_x)
            v_newt = InterpolationCore.newton_forward(x, y, t_x, diffs)
            v_gauss = InterpolationCore.gauss(x, y, t_x, diffs)
            v_stir = InterpolationCore.stirling(x, y, t_x, diffs)
            v_bess = InterpolationCore.bessel(x, y, t_x, diffs)

            self.res_txt.config(state="normal")
            self.res_txt.delete("1.0", tk.END)
            self.res_txt.insert(
                tk.END,
                f"X: {t_x}\nLagrange: {v_lagr:.5f}\nNewton: {v_newt:.5f}\nGauss: {v_gauss:.5f}\nStirling: {v_stir:.5f}\nBessel: {v_bess:.5f}",
            )
            self.res_txt.config(state="disabled")

            self.show_diff_table(x, diffs)
            self.plot(x, y, t_x)

        except Exception as e:
            messagebox.showerror("Ошибка", str(e))

    def plot(self, x, y, t_x):
        self.ax.clear()
        self.ax.scatter(x, y, color="black", label="Узлы")
        x_grid = np.linspace(min(x), max(x), 100)

        y_newt = [
            InterpolationCore.newton_forward(
                x, y, xi, InterpolationCore.get_finite_diffs(y)
            )
            for xi in x_grid
        ]
        y_gauss = [
            InterpolationCore.gauss(x, y, xi, InterpolationCore.get_finite_diffs(y))
            for xi in x_grid
        ]

        self.ax.plot(x_grid, y_newt, "b-", label="Ньютон")
        self.ax.plot(x_grid, y_gauss, "g--", label="Гаусс")
        self.ax.plot(
            t_x,
            InterpolationCore.lagrange(x, y, t_x),
            "ro",
            markersize=10,
            label="Точка",
        )

        self.ax.legend()
        self.ax.grid(True)
        self.canvas.draw()


App().mainloop()