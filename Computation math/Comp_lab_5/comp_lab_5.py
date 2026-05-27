import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from math import factorial, sin, exp, cos


class InterpolationMath:
    @staticmethod
    def factorial(num):
        if num == 0 or num == 1:
            return 1
        res = 1
        for i in range(2, num + 1):
            res *= i
        return res

    @staticmethod
    def get_finite_diffs(y):
        n = len(y)
        table = [np.array(y, dtype=float)]
        for j in range(1, n):
            prev = table[j - 1]
            diff_list = []
            for i in range(len(prev) - 1):
                diff_list.append(prev[i + 1] - prev[i])
            table.append(np.array(diff_list, dtype=float))
        return table

    @staticmethod
    def get_divided_diffs(x, y):
        n = len(y)
        table = np.zeros((n, n), dtype=float)
        for i in range(n):
            table[i][0] = y[i]
        for j in range(1, n):
            for i in range(n - j):
                den = x[i + j] - x[i]
                if den != 0:
                    table[i][j] = (table[i + 1][j - 1] - table[i][j - 1]) / den
        return table

    @staticmethod
    def lagrange(x_pts, y_pts, x_val):
        n = len(x_pts)
        res = 0
        for i in range(n):
            p = 1
            for j in range(n):
                if i != j:
                    den = x_pts[i] - x_pts[j]
                    if den != 0:
                        p *= (x_val - x_pts[j]) / den
            res += y_pts[i] * p
        return res

    @staticmethod
    def newton_divided_auto(x_pts, y_pts, x_val):
        n = len(x_pts)

        coef = InterpolationMath.get_divided_diffs(x_pts, y_pts)
        mid_idx = (n - 1) // 2

        val_idx = n - 1
        for i in range(n):
            if x_pts[i] >= x_val:
                val_idx = i
                break

        if val_idx <= mid_idx:
            res = float(coef[0, 0])
            product = 1.0
            for i in range(1, n):
                product *= x_val - x_pts[i - 1]
                res += float(coef[0, i]) * product
            return res, "Ньютон (разд. вперед)"
        else:
            x_rev = []
            y_rev = []
            for i in range(n - 1, -1, -1):
                x_rev.append(x_pts[i])
                y_rev.append(y_pts[i])

            coef_rev = InterpolationMath.get_divided_diffs(x_rev, y_rev)
            res = float(coef_rev[0, 0])
            product = 1.0
            for i in range(1, n):
                product *= x_val - x_rev[i - 1]
                res += float(coef_rev[0, i]) * product
            return res, "Ньютон (разд. назад)"

    @staticmethod
    def newton_finite_auto(x_pts, y_pts, x_val):
        n = len(x_pts)
        h = x_pts[1] - x_pts[0]

        diffs = InterpolationMath.get_finite_diffs(y_pts)
        mid_idx = (n - 1) // 2

        val_idx = n - 1
        for i in range(n):
            if x_pts[i] >= x_val:
                val_idx = i
                break

        if val_idx <= mid_idx:
            t = (x_val - x_pts[0]) / h
            res = float(y_pts[0])
            term = 1.0
            for i in range(1, n):
                term *= t - (i - 1)
                res += (term * float(diffs[i][0])) / InterpolationMath.factorial(i)
            return res, "Ньютон (кон. вперед)"
        else:
            t = (x_val - x_pts[n - 1]) / h
            res = float(y_pts[n - 1])
            term = 1.0
            for i in range(1, n):
                term *= t + (i - 1)
                res += (term * float(diffs[i][-1])) / InterpolationMath.factorial(i)
            return res, "Ньютон (кон. назад)"

    @staticmethod
    def stirling(x_pts, y_pts, x_val):
        n = len(x_pts)
        if n < 3 or n % 2 == 0:
            return 0.0

        mid = (n - 1) // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h
        diffs = InterpolationMath.get_finite_diffs(y_pts)

        res = float(y_pts[mid])
        for k in range(1, (n + 1) // 2):
            odd_deg = 2 * k - 1
            idx_s = mid - k
            if idx_s >= 0 and idx_s + 1 < len(diffs[odd_deg]):
                prod = t
                for i in range(1, k):
                    prod *= t**2 - i**2
                term = (diffs[odd_deg][idx_s] + diffs[odd_deg][idx_s + 1]) / 2
                res += (prod / InterpolationMath.factorial(odd_deg)) * term

            even_deg = 2 * k
            idx_e = mid - k
            if idx_e >= 0 and idx_e < len(diffs[even_deg]):
                prod = t**2
                for i in range(1, k):
                    prod *= t**2 - i**2
                res += (prod / InterpolationMath.factorial(even_deg)) * diffs[even_deg][
                    idx_e
                ]
        return res

    @staticmethod
    def bessel(x_pts, y_pts, x_val):
        n = len(x_pts)
        if n < 4 or n % 2 != 0:
            return 0.0

        mid = (n - 1) // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h
        diffs = InterpolationMath.get_finite_diffs(y_pts)

        res = (y_pts[mid] + y_pts[mid + 1]) / 2 + (t - 0.5) * diffs[1][mid]
        for k in range(2, (n // 2) + 1):
            even_deg = 2 * k - 2
            idx_s = mid - k + 1
            if idx_s >= 0 and idx_s + 1 < len(diffs[even_deg]):
                prod = 1.0
                for i in range(k - 1):
                    prod *= (t - i - 1) * (t + i)
                term = (diffs[even_deg][idx_s] + diffs[even_deg][idx_s + 1]) / 2
                res += (prod / InterpolationMath.factorial(even_deg)) * term

            odd_deg = 2 * k - 1
            idx_o = mid - k + 1
            if idx_o >= 0 and idx_o < len(diffs[odd_deg]):
                prod = t - 0.5
                for i in range(1, k):
                    prod *= (t + i - 1) * (t - i)
                res += (prod / InterpolationMath.factorial(odd_deg)) * diffs[odd_deg][
                    idx_o
                ]
        return res


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

        self.txt = tk.Text(sidebar, height=15, width=40)
        self.txt.insert(
            "1.0",
            "0.1 1.25\n0.2 2.38\n0.3 3.79\n0.4 5.44\n0.5 7.14\n0.6 8.8\n0.7 10.46\n0.8 12.12\n0.9 13.78\n1.0 15.44\n1.1 17.1\n1.2 18.76",
        )
        self.txt.pack(pady=5)

        self.func_box = ttk.Combobox(sidebar, values=["sin(x)", "exp(x)", "x^4+cos(x)"])
        self.func_box.set("sin(x)")
        self.func_box.pack(fill="x")
        self.range_in = ttk.Entry(sidebar)
        self.range_in.insert(0, "0 1 12")
        self.range_in.pack(fill="x", pady=2)

        ttk.Label(sidebar, text="Графики:").pack(anchor="w", pady=(8, 0))
        self.show_lagrange = tk.BooleanVar(value=True)
        self.show_newt_div = tk.BooleanVar(value=True)
        self.show_newt_fin = tk.BooleanVar(value=True)
        self.show_stirling = tk.BooleanVar(value=True)
        self.show_bessel = tk.BooleanVar(value=True)
        self.show_function = tk.BooleanVar(value=False)

        ttk.Checkbutton(
            sidebar,
            text="Лагранж",
            variable=self.show_lagrange,
            command=self._on_plot_toggle,
        ).pack(anchor="w")
        ttk.Checkbutton(
            sidebar,
            text="Ньютон (разд.)",
            variable=self.show_newt_div,
            command=self._on_plot_toggle,
        ).pack(anchor="w")
        ttk.Checkbutton(
            sidebar,
            text="Ньютон (кон.)",
            variable=self.show_newt_fin,
            command=self._on_plot_toggle,
        ).pack(anchor="w")
        ttk.Checkbutton(
            sidebar,
            text="Стирлинг",
            variable=self.show_stirling,
            command=self._on_plot_toggle,
        ).pack(anchor="w")
        ttk.Checkbutton(
            sidebar,
            text="Бессель",
            variable=self.show_bessel,
            command=self._on_plot_toggle,
        ).pack(anchor="w")
        ttk.Checkbutton(
            sidebar,
            text="Исходная функция (если выбрана)",
            variable=self.show_function,
            command=self._on_plot_toggle,
        ).pack(anchor="w")

        ttk.Button(sidebar, text="Загрузить из файла", command=self.load_file).pack(
            fill="x", pady=5
        )
        ttk.Label(sidebar, text="Точка X:").pack(anchor="w", pady=(10, 0))
        self.target_x = ttk.Entry(sidebar)
        self.target_x.insert(0, "0.25")
        self.target_x.pack(fill="x")

        ttk.Button(sidebar, text="РАССЧИТАТЬ", command=self.run).pack(fill="x", pady=10)

        self.out = tk.Text(
            sidebar, height=15, width=45, state="disabled", font=("Courier New", 9)
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

    def _on_plot_toggle(self):
        if hasattr(self, "last_x") and self.last_x is not None:
            try:
                self.plot(self.last_x, self.last_y, self.last_tx)
            except Exception:
                pass

    def _build_treeview(self, parent, x, table, is_finite):
        n = len(x)
        if is_finite:
            cols = ["x", "y"] + [f"Δ^{i}y" for i in range(1, len(table))]
        else:
            cols = ["x", "y"] + [f"f[x{i-1},...]" for i in range(2, table.shape[1] + 1)]

        tree = ttk.Treeview(parent, columns=cols, show="headings")
        for c in cols:
            tree.heading(c, text=c)
            tree.column(c, width=100, anchor="center")

        for i in range(n):
            row = [f"{x[i]:.3f}"]
            if is_finite:
                for level in range(len(table)):
                    if i < len(table[level]):
                        row.append(f"{table[level][i]:.4f}")
                    else:
                        row.append("-")
            else:
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
                a, b, n_pts = map(float, self.range_in.get().replace(",", ".").split())
                n_int = int(n_pts)
                x_list = []
                y_list = []
                step = (b - a) / (n_int - 1) if n_int > 1 else 0
                f_name = self.func_box.get()
                for i in range(n_int):
                    xi = a + i * step
                    x_list.append(xi)
                    yi = (
                        sin(xi)
                        if "sin" in f_name
                        else (exp(xi) if "exp" in f_name else (xi**4 + cos(xi)))
                    )
                    y_list.append(yi)
                x, y = np.array(x_list), np.array(y_list)

            if len(x) < 2:
                raise Exception("Нужно как минимум 2 точки.")

            tx = float(self.target_x.get())
            f_diffs = InterpolationMath.get_finite_diffs(y)
            d_diffs = InterpolationMath.get_divided_diffs(x, y)

            v_lagr = InterpolationMath.lagrange(x, y, tx)
            v_newt_fin, nf_name = InterpolationMath.newton_finite_auto(x, y, tx)
            v_newt_div, nd_name = InterpolationMath.newton_divided_auto(x, y, tx)
            v_stir = InterpolationMath.stirling(x, y, tx)
            v_bess = InterpolationMath.bessel(x, y, tx)

            res = f"X = {tx}\n"
            res += f"Лагранж:        {v_lagr:.5f}\n"
            res += f"{nf_name:<18}: {v_newt_fin:.5f}\n"
            res += f"{nd_name:<18}: {v_newt_div:.5f}\n"
            st_text = f"{v_stir:.5f}" if v_stir != 0 else "-"
            be_text = f"{v_bess:.5f}" if v_bess != 0 else "-"
            res += f"Стирлинг (нечет N):   {st_text}\n"
            res += f"Бессель (чеь N):   {be_text}\n"

            self.out.config(state="normal")
            self.out.delete("1.0", tk.END)
            self.out.insert("1.0", res)
            self.out.config(state="disabled")

            # store last data for incremental plot updates from checkboxes
            self.last_x = x
            self.last_y = y
            self.last_tx = tx

            self.show_tables(x, y, d_diffs)
            self.plot(x, y, tx)
        except Exception as e:
            messagebox.showerror("Ошибка", str(e))

    def show_tables(self, x, y, d_table):
        win = tk.Toplevel(self)
        win.title("Таблицы разностей")
        tabs = ttk.Notebook(win)
        tabs.pack(fill="both", expand=True)
        f_frame = ttk.Frame(tabs)
        tabs.add(f_frame, text="Конечные разности")
        self._build_treeview(f_frame, x, InterpolationMath.get_finite_diffs(y), True)
        d_frame = ttk.Frame(tabs)
        tabs.add(d_frame, text="Разделенные разности")
        self._build_treeview(d_frame, x, d_table, False)

    def plot(self, x, y, tx):
        self.ax.clear()
        self.ax.scatter(x, y, color="black", s=50, label="Узлы интерполяции", zorder=5)

        x_min, x_max = x[0], x[0]
        for val in x:
            if val < x_min:
                x_min = val
            if val > x_max:
                x_max = val

        x_grid = []
        grid_steps = 200
        step_size = (x_max - x_min) / (grid_steps - 1)
        for i in range(grid_steps):
            x_grid.append(x_min + i * step_size)

        try:
            if self.show_lagrange.get():
                y_l = [InterpolationMath.lagrange(x, y, xi) for xi in x_grid]
                self.ax.plot(
                    x_grid,
                    y_l,
                    label="Лагранжа",
                    color="yellow",
                    linewidth=2.5,
                    alpha=0.8,
                )
        except:
            pass

        try:
            if self.show_newt_div.get():
                y_nd = [
                    InterpolationMath.newton_divided_auto(x, y, xi)[0] for xi in x_grid
                ]
                self.ax.plot(
                    x_grid, y_nd, label="Ньютона (разд.)", color="lime", linewidth=2.5
                )
        except:
            pass

        try:
            h_ref = x[1] - x[0]
            is_const = True
            for i in range(len(x) - 1):
                if abs((x[i + 1] - x[i]) - h_ref) > 1e-5:
                    is_const = False
                    break

            if is_const:
                if self.show_newt_fin.get():
                    y_nf = [
                        InterpolationMath.newton_finite_auto(x, y, xi)[0]
                        for xi in x_grid
                    ]
                    self.ax.plot(
                        x_grid,
                        y_nf,
                        label="Ньютона (кон.)",
                        color="blue",
                        linewidth=2.5,
                    )

                if self.show_stirling.get() and len(x) % 2 != 0:
                    y_st = [InterpolationMath.stirling(x, y, xi) for xi in x_grid]
                    self.ax.plot(
                        x_grid, y_st, label="Стирлинг", color="orange", linewidth=2
                    )

                if self.show_bessel.get() and len(x) % 2 == 0:
                    y_be = [InterpolationMath.bessel(x, y, xi) for xi in x_grid]
                    self.ax.plot(
                        x_grid, y_be, label="Бессель", color="cyan", linewidth=2
                    )
        except:
            pass

        res_y = InterpolationMath.lagrange(x, y, tx)
        self.ax.scatter(
            [tx],
            [res_y],
            color="red",
            s=150,
            edgecolors="white",
            marker="*",
            label=f"Точка X={tx}",
            zorder=6,
        )

        if self.input_mode.get() == "func" and self.show_function.get():
            f_n = self.func_box.get()
            y_func = [
                (
                    sin(xi)
                    if "sin" in f_n
                    else (exp(xi) if "exp" in f_n else (xi**4 + cos(xi)))
                )
                for xi in x_grid
            ]
            self.ax.plot(
                x_grid,
                y_func,
                label=f_n,
                color="darkviolet",
                linewidth=2,
                linestyle=":",
            )

        self.ax.set_title("Графики интерполяционных многочленов")
        self.ax.legend(loc="best", prop={"size": 8})
        self.ax.grid(True, linestyle=":", alpha=0.6)
        self.canvas.draw()


InterpolationApp().mainloop()
