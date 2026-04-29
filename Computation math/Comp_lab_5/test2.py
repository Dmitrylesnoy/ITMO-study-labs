import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from math import factorial, sin, cos, exp


class InterpolationMath:
    @staticmethod
    def get_finite_diffs(y):
        """Ручной расчет конечных разностей без np.diff"""
        n = len(y)
        table = [[] for _ in range(n)]
        table[0] = [float(val) for val in y]

        for j in range(1, n):
            for i in range(n - j):
                # Δ^j y_i = Δ^{j-1} y_{i+1} - Δ^{j-1} y_i
                diff = table[j - 1][i + 1] - table[j - 1][i]
                table[j].append(diff)
        return [np.array(col) for col in table if col]

    @staticmethod
    def get_divided_diffs(x, y):
        """Расчет таблицы разделенных разностей"""
        n = len(y)
        table = np.zeros((n, n))
        table[:, 0] = y
        for j in range(1, n):
            for i in range(n - j):
                table[i][j] = (table[i + 1][j - 1] - table[i][j - 1]) / (
                    x[i + j] - x[i]
                )
        return table

    @staticmethod
    def lagrange(x_pts, y_pts, x_val):
        n = len(x_pts)
        res = 0.0
        for i in range(n):
            p = 1.0
            for j in range(n):
                if i != j:
                    p *= (x_val - x_pts[j]) / (x_pts[i] - x_pts[j])
            res += float(y_pts[i]) * p
        return res

    @staticmethod
    def newton_divided_auto(x_pts, y_pts, x_val):
        """Ньютон на разделенных разностях с автовыбором (вперед/назад)"""
        coef = InterpolationMath.get_divided_diffs(x_pts, y_pts)
        n = len(x_pts)
        mid_idx = n // 2

        if x_val <= x_pts[mid_idx]:
            # Вперед (используем верхнюю строку)
            res = float(coef[0, 0])
            product = 1.0
            for i in range(1, n):
                product *= x_val - x_pts[i - 1]
                res += float(coef[0, i]) * product
            return res, "Ньютон (разд. вперед)"
        else:
            # Назад (используем нижнюю диагональ)
            res = float(coef[n - 1, 0])
            product = 1.0
            for i in range(1, n):
                product *= x_val - x_pts[n - i]
                res += float(coef[n - 1 - i, i]) * product
            return res, "Ньютон (разд. назад)"

    @staticmethod
    def newton_finite_auto(x_pts, y_pts, x_val, diffs):
        """Ньютон на конечных разностях с автовыбором (вперед/назад)"""
        n = len(x_pts)
        h = x_pts[1] - x_pts[0]
        if x_val <= x_pts[n // 2]:
            t = (x_val - x_pts[0]) / h
            res = float(y_pts[0])
            term = 1.0
            for i in range(1, n):
                term *= t - i + 1
                res += (term * float(diffs[i][0])) / factorial(i)
            return res, "Ньютон (кон. вперед)"
        else:
            t = (x_val - x_pts[n - 1]) / h
            res = float(y_pts[n - 1])
            term = 1.0
            for i in range(1, n):
                term *= t + i - 1
                res += (term * float(diffs[i][-1])) / factorial(i)
            return res, "Ньютон (кон. назад)"


class App(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("ИТМО Лаб 5: Интерполяция")
        self.geometry("1100x800")
        self._setup_ui()

    def _setup_ui(self):
        sidebar = ttk.Frame(self, padding=10)
        sidebar.pack(side="left", fill="y")

        ttk.Label(sidebar, text="1. Ввод данных:", font="Arial 10 bold").pack(
            anchor="w"
        )
        self.mode = tk.StringVar(value="manual")
        ttk.Radiobutton(
            sidebar, text="Клавиатура", variable=self.mode, value="manual"
        ).pack(anchor="w")
        ttk.Radiobutton(sidebar, text="Функция", variable=self.mode, value="func").pack(
            anchor="w"
        )

        self.txt = tk.Text(sidebar, height=8, width=30)
        self.txt.insert("1.0", "0.1 1.25\n0.2 2.38\n0.3 3.79\n0.4 5.44\n0.5 7.14")
        self.txt.pack(pady=5)

        self.func_box = ttk.Combobox(sidebar, values=["sin(x)", "exp(x)", "cos(x)"])
        self.func_box.set("sin(x)")
        self.func_box.pack(fill="x")
        self.range_in = ttk.Entry(sidebar)
        self.range_in.insert(0, "0 1 10")
        self.range_in.pack(fill="x", pady=2)

        ttk.Label(sidebar, text="2. Точка X:").pack(anchor="w", pady=(10, 0))
        self.target_x = ttk.Entry(sidebar)
        self.target_x.insert(0, "0.25")
        self.target_x.pack(fill="x")

        ttk.Button(sidebar, text="РАССЧИТАТЬ", command=self.run).pack(fill="x", pady=10)
        self.out = tk.Text(
            sidebar, height=15, width=35, state="disabled", font=("Courier New", 9)
        )
        self.out.pack()

        self.fig, self.ax = plt.subplots(figsize=(6, 5))
        self.canvas = FigureCanvasTkAgg(self.fig, master=self)
        self.canvas.get_tk_widget().pack(side="right", fill="both", expand=True)

    def run(self):
        try:
            if self.mode.get() == "manual":
                lines = self.txt.get("1.0", tk.END).strip().split("\n")
                pts = sorted([list(map(float, l.split())) for l in lines if l.strip()])
                x, y = np.array([p[0] for p in pts]), np.array([p[1] for p in pts])
            else:
                a, b, n = map(float, self.range_in.get().split())
                x = np.linspace(a, b, int(n))
                f_name = self.func_box.get()
                y = np.array(
                    [
                        (
                            sin(xi)
                            if "sin" in f_name
                            else (exp(xi) if "exp" in f_name else cos(xi))
                        )
                        for xi in x
                    ]
                )

            tx = float(self.target_x.get())
            f_diffs = InterpolationMath.get_finite_diffs(y)
            d_diffs = InterpolationMath.get_divided_diffs(x, y)

            v_lagr = InterpolationMath.lagrange(x, y, tx)
            v_new_d, n_d_name = InterpolationMath.newton_divided_auto(x, y, tx)
            v_new_f, n_f_name = InterpolationMath.newton_finite_auto(x, y, tx, f_diffs)

            res = f"X = {tx}\n" + "-" * 30 + "\n"
            res += f"Лагранж:        {v_lagr:.5f}\n"
            res += f"{n_d_name:<15}: {v_new_d:.5f}\n"
            res += f"{n_f_name:<15}: {v_new_f:.5f}\n"

            self.out.config(state="normal")
            self.out.delete("1.0", tk.END)
            self.out.insert("1.0", res)
            self.out.config(state="disabled")

            self.show_tables(x, f_diffs, d_diffs)
            self.plot(x, y, tx, f_diffs)
        except Exception as e:
            messagebox.showerror("Ошибка", f"Ошибка: {e}")

    def show_tables(self, x, f_table, d_table):
        win = tk.Toplevel(self)
        win.title("Таблицы разностей")
        tabs = ttk.Notebook(win)
        tabs.pack(fill="both", expand=True)

        # Конечные
        f_frame = ttk.Frame(tabs)
        tabs.add(f_frame, text="Конечные Δ")
        cols = ["X", "Y"] + [f"Δ^{i}" for i in range(1, len(f_table))]
        tree = ttk.Treeview(f_frame, columns=cols, show="headings")
        for c in cols:
            tree.heading(c, text=c)
            tree.column(c, width=80)
        for i in range(len(x)):
            row = [f"{x[i]:.2f}", f"{f_table[0][i]:.4f}"]
            for d in range(1, len(f_table)):
                row.append(f"{f_table[d][i]:.4f}" if i < len(f_table[d]) else "")
            tree.insert("", "end", values=row)
        tree.pack(fill="both", expand=True)

        # Разделенные
        d_frame = ttk.Frame(tabs)
        tabs.add(d_frame, text="Разделенные f[]")
        cols_d = ["X", "Y"] + [f"f{i}" for i in range(1, d_table.shape[1])]
        tree_d = ttk.Treeview(d_frame, columns=cols_d, show="headings")
        for c in cols_d:
            tree_d.heading(c, text=c)
            tree_d.column(c, width=80)
        for i in range(len(x)):
            row = [f"{x[i]:.2f}"]
            for j in range(d_table.shape[1]):
                row.append(f"{d_table[i][j]:.4f}" if i < len(x) - j else "")
            tree_d.insert("", "end", values=row)
        tree_d.pack(fill="both", expand=True)

    def plot(self, x, y, tx, diffs):
        self.ax.clear()
        self.ax.scatter(x, y, color="black", label="Узлы", zorder=5)
        xg = np.linspace(min(x), max(x), 100)
        # Отрисовка Ньютона как основной линии
        yg = [InterpolationMath.newton_finite_auto(x, y, val, diffs)[0] for val in xg]
        self.ax.plot(xg, yg, label="Многочлен Ньютона", color="blue")
        self.ax.scatter(
            [tx],
            [InterpolationMath.lagrange(x, y, tx)],
            color="red",
            s=80,
            label="Точка",
            zorder=6,
        )
        self.ax.legend()
        self.ax.grid(True)
        self.canvas.draw()


App().mainloop()
