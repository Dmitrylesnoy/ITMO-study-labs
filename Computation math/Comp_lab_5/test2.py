import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from math import factorial, sin, exp, cos


# --- МАТЕМАТИЧЕСКИЙ БЛОК ---
class InterpolationMath:
    @staticmethod
    def get_finite_diffs(y):
        n = len(y)
        table = [np.array(y, dtype=float)]
        for j in range(1, n):
            prev = table[-1]
            diff = prev[1:] - prev[:-1]
            table.append(diff)
        return table

    @staticmethod
    def get_divided_diffs(x, y):
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
        res = 0
        for i in range(n):
            p = 1
            for j in range(n):
                if i != j:
                    p *= (x_val - x_pts[j]) / (x_pts[i] - x_pts[j])
            res += y_pts[i] * p
        return res

    @staticmethod
    def newton_divided(x_pts, y_pts, x_val, table=None):
        if table is None:
            table = InterpolationMath.get_divided_diffs(x_pts, y_pts)
        n = len(x_pts)
        mid_idx = n // 2

        # Используем одну формулу для всей кривой, чтобы избежать скачков,
        # либо переключаем их. Здесь реализован выбор для точности в разных концах.
        if x_val <= x_pts[mid_idx]:
            # Первая формула (вперед)
            res = table[0, 0]
            prod = 1.0
            for i in range(1, n):
                prod *= x_val - x_pts[i - 1]
                res += table[0, i] * prod
            return res, "Ньютон (разд. вперед)"
        else:
            # Вторая формула (назад)
            res = table[n - 1, 0]
            prod = 1.0
            for i in range(1, n):
                prod *= x_val - x_pts[n - i]
                res += table[n - 1 - i, i] * prod
            return res, "Ньютон (разд. назад)"

    @staticmethod
    def newton_finite(x_pts, y_pts, x_val, diffs=None):
        if diffs is None:
            diffs = InterpolationMath.get_finite_diffs(y_pts)
        n = len(x_pts)
        h = x_pts[1] - x_pts[0]
        mid_idx = n // 2

        if x_val <= x_pts[mid_idx]:
            t = (x_val - x_pts[0]) / h
            res = float(y_pts[0])
            term = 1.0
            for i in range(1, n):
                term *= t - (i - 1)
                res += (term * diffs[i][0]) / factorial(i)
            return res, "Ньютон (кон. вперед)"
        else:
            t = (x_val - x_pts[n - 1]) / h
            res = float(y_pts[n - 1])
            term = 1.0
            for i in range(1, n):
                term *= t + (i - 1)
                res += (term * diffs[i][-1]) / factorial(i)
            return res, "Ньютон (кон. назад)"

    @staticmethod
    def stirling(x_pts, y_pts, x_val, diffs=None):
        if diffs is None:
            diffs = InterpolationMath.get_finite_diffs(y_pts)
        n = len(x_pts)
        mid = n // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h
        res = float(y_pts[mid])

        for k in range(1, (n + 1) // 2):
            # Нечетные порядки
            if 2 * k - 1 < n and mid - k >= 0:
                prod = t
                for i in range(1, k):
                    prod *= t**2 - i**2
                res += (
                    (prod / factorial(2 * k - 1))
                    * (diffs[2 * k - 1][mid - k] + diffs[2 * k - 1][mid - k + 1])
                    / 2
                )
            # Четные порядки
            if 2 * k < n and mid - k >= 0:
                prod = t**2
                for i in range(1, k):
                    prod *= t**2 - i**2
                res += (prod / factorial(2 * k)) * diffs[2 * k][mid - k]
        return res

    @staticmethod
    def bessel(x_pts, y_pts, x_val, diffs=None):
        if diffs is None:
            diffs = InterpolationMath.get_finite_diffs(y_pts)
        n = len(x_pts)
        mid = (n - 1) // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h

        res = (y_pts[mid] + y_pts[mid + 1]) / 2 + (t - 0.5) * diffs[1][mid]

        for k in range(2, (n + 1) // 2 + 1):
            # Четные порядки
            if 2 * k - 2 < n and mid - k + 1 >= 0:
                prod = t - 0.5
                for i in range(1, k):
                    prod *= (t + i - 1) * (t - i)
                # Упрощенная реализация Бесселя для демонстрации прохождения через узлы
                # В полной версии используются средние значения четных разностей
                idx = 2 * k - 2
                if idx < len(diffs) and mid - k + 1 < len(diffs[idx]):
                    p = 1.0
                    for i in range(k - 1):
                        p *= (t - i - 1) * (t + i)
                    res += (
                        (p / factorial(2 * k - 2))
                        * (
                            diffs[2 * k - 2][mid - k + 1]
                            + diffs[2 * k - 2][mid - k + 2]
                        )
                        / 2
                    )
        return res


# --- ИНТЕРФЕЙС ПРИЛОЖЕНИЯ ---
class InterpolationApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Лаб №5: Интерполяция (Исправлено)")
        self.geometry("1200x850")
        self._setup_ui()

    def _setup_ui(self):
        sidebar = ttk.Frame(self, padding=10)
        sidebar.pack(side="left", fill="y")

        ttk.Label(sidebar, text="Ввод данных:", font=("Arial", 10, "bold")).pack(
            anchor="w"
        )
        self.input_mode = tk.StringVar(value="manual")
        ttk.Radiobutton(
            sidebar, text="Клавиатура", variable=self.input_mode, value="manual"
        ).pack(anchor="w")
        ttk.Radiobutton(
            sidebar, text="Функция", variable=self.input_mode, value="func"
        ).pack(anchor="w")

        self.txt = tk.Text(sidebar, height=8, width=35)
        self.txt.insert("1.0", "0.1 1.25\n0.2 2.38\n0.3 3.79\n0.4 5.44\n0.5 7.14")
        self.txt.pack(pady=5)

        self.func_box = ttk.Combobox(sidebar, values=["sin(x)", "exp(x)", "x^4+cos(x)"])
        self.func_box.set("sin(x)")
        self.func_box.pack(fill="x")
        self.range_in = ttk.Entry(sidebar)
        self.range_in.insert(0, "0 1 5")
        self.range_in.pack(fill="x", pady=2)

        ttk.Button(sidebar, text="Загрузить из файла", command=self.load_file).pack(
            fill="x", pady=5
        )
        ttk.Label(sidebar, text="Точка X для расчета:").pack(anchor="w", pady=(10, 0))
        self.target_x = ttk.Entry(sidebar)
        self.target_x.insert(0, "0.25")
        self.target_x.pack(fill="x")

        ttk.Button(sidebar, text="РАССЧИТАТЬ", command=self.run).pack(fill="x", pady=10)

        self.out = tk.Text(
            sidebar, height=12, width=45, state="disabled", font=("Courier New", 9)
        )
        self.out.pack()

        content = ttk.Frame(self, padding=10)
        content.pack(side="right", fill="both", expand=True)
        self.fig, self.ax = plt.subplots(figsize=(7, 6))
        self.canvas = FigureCanvasTkAgg(self.fig, master=content)
        self.canvas.get_tk_widget().pack(fill="both", expand=True)

    def load_file(self):
        path = filedialog.askopenfilename()
        if path:
            with open(path, "r") as f:
                self.txt.delete("1.0", tk.END)
                self.txt.insert(tk.END, f.read())

    def run(self):
        try:
            if self.input_mode.get() == "manual":
                lines = self.txt.get("1.0", tk.END).strip().split("\n")
                pts = sorted(
                    [
                        list(map(float, l.replace(",", ".").split()))
                        for l in lines
                        if l.strip()
                    ]
                )
                x = np.array([p[0] for p in pts])
                y = np.array([p[1] for p in pts])
            else:
                a, b, n = map(float, self.range_in.get().replace(",", ".").split())
                x = np.linspace(a, b, int(n))
                f_name = self.func_box.get()
                y = np.array(
                    [
                        (
                            sin(xi)
                            if "sin" in f_name
                            else (exp(xi) if "exp" in f_name else xi**4 + cos(xi))
                        )
                        for xi in x
                    ]
                )

            tx = float(self.target_x.get())
            f_diffs = InterpolationMath.get_finite_diffs(y)
            d_diffs = InterpolationMath.get_divided_diffs(x, y)

            v_lagr = InterpolationMath.lagrange(x, y, tx)
            v_newt_fin, nf_name = InterpolationMath.newton_finite(x, y, tx, f_diffs)
            v_newt_div, nd_name = InterpolationMath.newton_divided(x, y, tx, d_diffs)
            v_stir = InterpolationMath.stirling(x, y, tx, f_diffs)
            v_bess = InterpolationMath.bessel(x, y, tx, f_diffs)

            res = f"Результаты в точке X = {tx}:\n"
            res += "-" * 40 + "\n"
            res += f"Лагранж:          {v_lagr:.6f}\n"
            res += f"{nf_name:<18}: {v_newt_fin:.6f}\n"
            res += f"{nd_name:<18}: {v_newt_div:.6f}\n"
            res += f"Стирлинг:         {v_stir:.6f}\n"
            res += f"Бессель:          {v_bess:.6f}\n"

            self.out.config(state="normal")
            self.out.delete("1.0", tk.END)
            self.out.insert("1.0", res)
            self.out.config(state="disabled")

            self.show_tables(x, f_diffs, d_diffs)
            self.plot(x, y, tx, f_diffs, d_diffs)
        except Exception as e:
            messagebox.showerror("Ошибка", f"Проверьте корректность данных.\n{str(e)}")

    def show_tables(self, x, f_table, d_table):
        win = tk.Toplevel(self)
        win.title("Таблицы разностей")
        tabs = ttk.Notebook(win)
        tabs.pack(fill="both", expand=True)

        # Таблица конечных разностей
        f_frame = ttk.Frame(tabs)
        tabs.add(f_frame, text="Конечные разности")
        cols = ["X", "Y"] + [f"Δ^{i}" for i in range(1, len(f_table))]
        tree = ttk.Treeview(f_frame, columns=cols, show="headings")
        for c in cols:
            tree.heading(c, text=c)
            tree.column(c, width=70, anchor="center")
        for i in range(len(x)):
            row = [f"{x[i]:.2f}", f"{f_table[0][i]:.4f}"]
            for d in range(1, len(f_table)):
                row.append(f"{f_table[d][i]:.4f}" if i < len(f_table[d]) else "")
            tree.insert("", "end", values=row)
        tree.pack(fill="both", expand=True)

        # Таблица разделенных разностей
        d_frame = ttk.Frame(tabs)
        tabs.add(d_frame, text="Разделенные разности")
        cols_d = ["X", "Y"] + [f"f{i}" for i in range(1, d_table.shape[1])]
        tree_d = ttk.Treeview(d_frame, columns=cols_d, show="headings")
        for c in cols_d:
            tree_d.heading(c, text=c)
            tree_d.column(c, width=80, anchor="center")
        for i in range(len(x)):
            row = [f"{x[i]:.2f}"]
            for j in range(d_table.shape[1]):
                row.append(f"{d_table[i][j]:.4f}" if i < len(x) - j else "")
            tree_d.insert("", "end", values=row)
        tree_d.pack(fill="both", expand=True)

    def plot(self, x, y, tx, f_diffs, d_diffs):
        self.ax.clear()
        x_grid = np.linspace(min(x), max(x), 200)

        # Предварительный расчет сеток для графиков
        y_lagr = [InterpolationMath.lagrange(x, y, xi) for xi in x_grid]
        y_newt_div = [
            InterpolationMath.newton_divided(x, y, xi, d_diffs)[0] for xi in x_grid
        ]
        y_newt_fin = [
            InterpolationMath.newton_finite(x, y, xi, f_diffs)[0] for xi in x_grid
        ]
        y_stir = [InterpolationMath.stirling(x, y, xi, f_diffs) for xi in x_grid]

        # Отрисовка
        self.ax.plot(
            x_grid, y_lagr, label="Лагранж", color="red", alpha=0.7, linewidth=2
        )
        self.ax.plot(
            x_grid, y_newt_div, label="Ньютон (разд.)", color="green", linestyle="--"
        )
        self.ax.plot(
            x_grid, y_newt_fin, label="Ньютон (кон.)", color="blue", linestyle=":"
        )
        self.ax.plot(x_grid, y_stir, label="Стирлинг", color="orange", alpha=0.6)

        # Узлы
        self.ax.scatter(x, y, color="black", s=40, label="Узлы", zorder=5)
        self.ax.scatter(
            [tx],
            [InterpolationMath.lagrange(x, y, tx)],
            color="magenta",
            s=80,
            marker="*",
            label="Цель",
            zorder=6,
        )

        self.ax.set_title("Сравнение интерполяционных методов")
        self.ax.legend(prop={"size": 8})
        self.ax.grid(True, alpha=0.3)
        self.canvas.draw()


InterpolationApp().mainloop()
