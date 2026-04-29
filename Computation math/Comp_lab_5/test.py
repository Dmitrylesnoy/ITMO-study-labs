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
            table.append(np.diff(table[j - 1]))
        return table

    @staticmethod
    def get_divided_diffs(x, y):
        n = len(y)
        coef = np.zeros([n, n])
        coef[:, 0] = y
        for j in range(1, n):
            for i in range(n - j):
                coef[i][j] = (coef[i + 1][j - 1] - coef[i][j - 1]) / (x[i + j] - x[i])
        return coef

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
    def newton_divided(x_pts, y_pts, x_val):
        coef = InterpolationMath.get_divided_diffs(x_pts, y_pts)
        n = len(x_pts)
        # 1-я формула (вперед)
        res = coef[0, 0]
        product = 1.0
        for i in range(1, n):
            product *= x_val - x_pts[i - 1]
            res += coef[0, i] * product
        return res

    @staticmethod
    def newton_finite_auto(x_pts, y_pts, x_val, diffs):
        n = len(x_pts)
        h = x_pts[1] - x_pts[0]
        if x_val <= x_pts[n // 2]:
            # 1-я формула Ньютона (вперед)
            t = (x_val - x_pts[0]) / h
            res = y_pts[0]
            term = 1
            for i in range(1, n):
                term *= t - i + 1
                res += (term * diffs[i][0]) / factorial(i)
            return res, "Ньютон 1 (вперед)"
        else:
            # 2-я формула Ньютона (назад)
            t = (x_val - x_pts[n - 1]) / h
            res = y_pts[n - 1]
            term = 1
            for i in range(1, n):
                term *= t + i - 1
                res += (term * diffs[i][-1]) / factorial(i)
            return res, "Ньютон 2 (назад)"

    @staticmethod
    def gauss_auto(x_pts, y_pts, x_val, diffs):
        n = len(x_pts)
        mid = n // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h
        res = y_pts[mid]
        term = 1
        method_name = "Гаусс 1" if t >= 0 else "Гаусс 2"
        for i in range(1, n):
            if t >= 0:  # 1-я формула
                k = i // 2
                term *= (t - k) if i % 2 != 0 else (t + k)
                idx = mid - k
            else:  # 2-я формула
                k = (i + 1) // 2
                term *= (t + k - 1) if i % 2 != 0 else (t - k)
                idx = mid - k
            if 0 <= idx < len(diffs[i]):
                res += (term * diffs[i][idx]) / factorial(i)
        return res, method_name

    @staticmethod
    def stirling(x_pts, y_pts, x_val, diffs):
        n = len(x_pts)
        mid = n // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h
        res = y_pts[mid]
        try:
            res += (
                t * (diffs[1][mid - 1] + diffs[1][mid]) / 2
                + (t**2 / 2) * diffs[2][mid - 1]
            )
        except:
            pass
        return res

    @staticmethod
    def bessel(x_pts, y_pts, x_val, diffs):
        n = len(x_pts)
        mid = (n - 1) // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h
        res = (y_pts[mid] + y_pts[mid + 1]) / 2 + (t - 0.5) * diffs[1][mid]
        try:
            res += (t * (t - 1) / 2) * (diffs[2][mid - 1] + diffs[2][mid]) / 2
        except:
            pass
        return res


# --- ИНТЕРФЕЙС ПРИЛОЖЕНИЯ ---
class InterpolationApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Лаб №5: Интерполяция")
        self.geometry("1100x800")
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

        self.txt = tk.Text(sidebar, height=8, width=30)
        self.txt.insert("1.0", "0.1 1.25\n0.2 2.38\n0.3 3.79\n0.4 5.44\n0.5 7.14")
        self.txt.pack(pady=5)

        self.func_box = ttk.Combobox(sidebar, values=["sin(x)", "exp(x)", "cos(x)"])
        self.func_box.set("sin(x)")
        self.func_box.pack(fill="x")
        self.range_in = ttk.Entry(sidebar)
        self.range_in.insert(0, "0 1 10")
        self.range_in.pack(fill="x", pady=2)

        ttk.Button(sidebar, text="Загрузить файл", command=self.load_file).pack(
            fill="x", pady=5
        )

        ttk.Label(sidebar, text="Точка X:").pack(anchor="w", pady=(10, 0))
        self.target_x = ttk.Entry(sidebar)
        self.target_x.insert(0, "0.25")
        self.target_x.pack(fill="x")

        ttk.Button(sidebar, text="РАССЧИТАТЬ", command=self.run).pack(fill="x", pady=10)

        self.out = tk.Text(
            sidebar, height=15, width=35, state="disabled", font=("Courier New", 9)
        )
        self.out.pack()

        content = ttk.Frame(self, padding=10)
        content.pack(side="right", fill="both", expand=True)
        self.fig, self.ax = plt.subplots(figsize=(6, 5))
        self.canvas = FigureCanvasTkAgg(self.fig, master=content)
        self.canvas.get_tk_widget().pack(fill="both", expand=True)

    def load_file(self):
        path = filedialog.askopenfilename()
        if path:
            with open(path, "r") as f:
                self.txt.delete("1.0", tk.END)
                self.txt.insert(tk.END, f.read())

    def show_tables_window(self, x, y):
        win = tk.Toplevel(self)
        win.title("Таблицы разностей")
        win.geometry("800x500")

        tabs = ttk.Notebook(win)
        tabs.pack(fill="both", expand=True)

        # 1. Вкладка конечных разностей
        f_diff_frame = ttk.Frame(tabs)
        tabs.add(f_diff_frame, text="Конечные разности (Δ)")
        self._build_treeview(f_diff_frame, x, InterpolationMath.get_finite_diffs(y), is_finite=True)

        # 2. Вкладка разделенных разностей
        d_diff_frame = ttk.Frame(tabs)
        tabs.add(d_diff_frame, text="Разделенные разности (f[...])")
        self._build_treeview(d_diff_frame, x, InterpolationMath.get_divided_diffs(x, y), is_finite=False)

    def _build_treeview(self, parent, x, table, is_finite):
        n = len(x)
        # Динамическое формирование заголовков
        if is_finite:
            cols = ["x", "y"] + [f"Δ^{i}y" for i in range(1, len(table))]
        else:
            cols = ["x", "y"] + [f"f[x0...x{i}]" for i in range(1, table.shape[1])]

        tree = ttk.Treeview(parent, columns=cols, show="headings")
        for c in cols:
            tree.heading(c, text=c)
            tree.column(c, width=100, anchor="center")

        for i in range(n):
            row = [f"{x[i]:.3f}"]
            if is_finite:
                # Для конечных разностей (список массивов разной длины)
                for level in range(len(table)):
                    if i < len(table[level]):
                        row.append(f"{table[level][i]:.4f}")
                    else:
                        row.append("-")
            else:
                # Для разделенных разностей (матрица n x n)
                for j in range(table.shape[1]):
                    if i < n - j:
                        row.append(f"{table[i][j]:.4f}")
                    else:
                        row.append("-")
            tree.insert("", "end", values=row)

        scroll = ttk.Scrollbar(parent, orient="vertical", command=tree.yview)
        tree.configure(yscroll=scroll.set)
        tree.pack(side="left", fill="both", expand=True)
        scroll.pack(side="right", fill="y")

    # def show_table(self, x, diffs):
    #     win = tk.Toplevel(self)
    #     win.title("Таблица конечных разностей")
    #     cols = ["X", "Y"] + [f"Δ^{i}" for i in range(1, len(diffs))]
    #     tree = ttk.Treeview(win, columns=cols, show="headings")
    #     for c in cols:
    #         tree.heading(c, text=c)
    #         tree.column(c, width=80, anchor="center")
    #     for i in range(len(x)):
    #         row = [f"{x[i]:.2f}", f"{diffs[0][i]:.4f}"]
    #         for d in range(1, len(diffs)):
    #             row.append(f"{diffs[d][i]:.4f}" if i < len(diffs[d]) else "")
    #         tree.insert("", "end", values=row)
    #     tree.pack(fill="both", expand=True)

    def run(self):
        try:
            if self.input_mode.get() == "manual":
                lines = self.txt.get("1.0", tk.END).strip().split("\n")
                pts = sorted([list(map(float, l.split())) for l in lines if l.strip()])
                x, y = np.array([p[0] for p in pts]), np.array([p[1] for p in pts])
            else:
                a, b, n = map(float, self.range_in.get().split())
                x = np.linspace(a, b, int(n))
                f_name = self.func_box.get()
                y = (
                    np.sin(x)
                    if "sin" in f_name
                    else (np.exp(x) if "exp" in f_name else np.cos(x))
                )

            tx = float(self.target_x.get())
            diffs = InterpolationMath.get_finite_diffs(y)

            v_newt_fin, n_name = InterpolationMath.newton_finite_auto(x, y, tx, diffs)
            v_newt_div = InterpolationMath.newton_divided(x, y, tx)
            v_gauss, g_name = InterpolationMath.gauss_auto(x, y, tx, diffs)
            v_stir = InterpolationMath.stirling(x, y, tx, diffs)
            v_bess = InterpolationMath.bessel(x, y, tx, diffs)
            v_lagr = InterpolationMath.lagrange(x, y, tx)

            res = f"X = {tx}\n" + "-" * 30 + "\n"
            res += f"Лагранж:        {v_lagr:.5f}\n"
            res += f"Ньютон (разд):  {v_newt_div:.5f}\n"
            res += f"{n_name:<15}: {v_newt_fin:.5f}\n"
            res += f"{g_name:<15}: {v_gauss:.5f}\n"
            res += f"Стирлинг:       {v_stir:.5f}\n"
            res += f"Бессель:        {v_bess:.5f}\n"

            self.out.config(state="normal")
            self.out.delete("1.0", tk.END)
            self.out.insert("1.0", res)
            self.out.config(state="disabled")

            self.show_tables_window(x, y)
            self.plot(x, y, tx, diffs)
        except Exception as e:
            messagebox.showerror("Ошибка", str(e))

    def plot(self, x, y, tx, diffs):
        self.ax.clear()

        self.ax.scatter(x, y, color="black", s=50, label="Узлы интерполяции", zorder=5)
        x_grid = np.linspace(min(x), max(x), 200)
        y_newton_grid = [
            InterpolationMath.newton_finite_auto(x, y, xi, diffs)[0] for xi in x_grid
        ]
        self.ax.plot(
            x_grid,
            y_newton_grid,
            label="Многочлен Ньютона",
            color="blue",
            linewidth=2,
        )

        y_gauss_grid = [InterpolationMath.gauss_auto(x, y, xi, diffs)[0] for xi in x_grid]
        self.ax.plot(
            x_grid,
            y_gauss_grid,
            "--",
            label="Многочлен Гаусса",
            color="orange",
            linewidth=2,
        )

        res_y, _ = InterpolationMath.gauss_auto(x, y, tx, diffs)
        self.ax.scatter(
            [tx],
            [res_y],
            color="red",
            s=100,
            edgecolors="white",
            label=f"Точка X={tx}",
            zorder=6,
        )

        self.ax.set_title("Сравнение интерполяции: Ньютон vs Гаусс")
        self.ax.set_xlabel("X")
        self.ax.set_ylabel("Y")
        self.ax.legend(loc="best")
        self.ax.grid(True, linestyle=":", alpha=0.6)
        self.canvas.draw()


InterpolationApp().mainloop()
