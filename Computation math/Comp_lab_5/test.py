import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from math import factorial, sin, exp, cos


# --- МАТЕМАТИЧЕСКИЙ БЛОК ---
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
        # table is a list of arrays, each array is a column of differences
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
                if x[i + j] - x[i] != 0:
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
                    if x_pts[i] - x_pts[j] != 0:
                        p *= (x_val - x_pts[j]) / (x_pts[i] - x_pts[j])
            res += y_pts[i] * p
        return res

    @staticmethod
    def newton_divided_auto(x_pts, y_pts, x_val):
        coef = InterpolationMath.get_divided_diffs(x_pts, y_pts)
        n = len(x_pts)
        mid_idx = (n - 1) // 2

        # Check for small dataset
        if n < 2:
             if n == 1: return y_pts[0], "Newton (1 pt)"
             else: return 0.0, "Newton (No pts)"

        # Correct formula for backward difference to pass all points.
        # It's better to use one formula unless there is a specific need.
        # However, to maintain backward formula concept, we switch.
        
        # Determine split point. We use index, not value.
        split_point = mid_idx
        try:
            val_idx = next(i for i, v in enumerate(x_pts) if v >= x_val)
        except StopIteration:
            val_idx = n - 1 # Use forward if x is at the end.
        
        if val_idx <= split_point:
            # Forward formula. Pass points up to n.
            res = float(coef[0, 0])
            product = 1.0
            for i in range(1, n):
                product *= x_val - x_pts[i - 1]
                res += float(coef[0, i]) * product
            return res, "Ньютон (разд. вперед)"
        else:
            # Backward formula using divided differences. For correctness,
            # this often just reduces to forward formula on reverse points.
            # We'll use backward-like form but ensure it uses all points.
            # Simplified backward for now for correctness and to match split logic.
            # Using forward on reversed data is equivalent and robust.
            
            x_pts_rev = x_pts[::-1]
            y_pts_rev = y_pts[::-1]
            coef_rev = InterpolationMath.get_divided_diffs(x_pts_rev, y_pts_rev)
            
            res = float(coef_rev[0, 0])
            product = 1.0
            for i in range(1, n):
                product *= x_val - x_pts_rev[i - 1]
                res += float(coef_rev[0, i]) * product
            return res, "Ньютон (разд. назад)"

    @staticmethod
    def newton_finite_auto(x_pts, y_pts, x_val):
        n = len(x_pts)
        # Check for small dataset or no step
        if n < 2:
             if n == 1: return y_pts[0], "Newton (1 pt)"
             else: return 0.0, "Newton (No pts)"

        h = x_pts[1] - x_pts[0]
        if h == 0: return 0.0, "Newton (H=0)"
        
        diffs = InterpolationMath.get_finite_diffs(y_pts)
        mid_idx = (n - 1) // 2
        
        # Select formulas based on point.
        split_point = mid_idx
        try:
            val_idx = next(i for i, v in enumerate(x_pts) if v >= x_val)
        except StopIteration:
            val_idx = n - 1

        if val_idx <= split_point:
            # 1st formula (forward). Use points from 0 to n-1.
            t = (x_val - x_pts[0]) / h
            res = float(y_pts[0])
            term = 1.0
            for i in range(1, n):
                term *= (t - (i - 1))
                res += (term * float(diffs[i][0])) / factorial(i)
            return res, "Ньютон (кон. вперед)"
        else:
            # 2nd formula (backward). Use points from n-1 to 0.
            t = (x_val - x_pts[n - 1]) / h
            res = float(y_pts[n - 1])
            term = 1.0
            for i in range(1, n):
                # Correct backward term t(t+1)(t+2)...
                term *= (t + (i - 1))
                # Coefficient is diffs[i][0] of reverse y points which is the backward difference.
                # In forward table format it is diffs[i][last element]
                res += (term * float(diffs[i][-1])) / factorial(i)
            return res, "Ньютон (кон. назад)"

    @staticmethod
    def stirling(x_pts, y_pts, x_val):
        n = len(x_pts)
        if n < 3: return 0.0 # Standard stirling needs at least 3 points
        if n % 2 == 0: return 0.0 # Standard stirling needs odd number of points
        
        mid = (n - 1) // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h
        diffs = InterpolationMath.get_finite_diffs(y_pts)
        
        # Degree up to n-1
        res = float(y_pts[mid])
        for k in range(1, (n+1)//2):
            # Odd degree terms (2k-1)
            odd_deg = 2*k - 1
            idx_start = mid - k
            if idx_start >= 0 and idx_start + 1 < len(diffs[odd_deg]):
                prod = t
                for i in range(1, k):
                    prod *= (t**2 - i**2)
                term = (diffs[odd_deg][idx_start] + diffs[odd_deg][idx_start+1]) / 2
                res += (prod / factorial(odd_deg)) * term
                
            # Even degree terms (2k)
            even_deg = 2*k
            idx_even = mid - k
            if idx_even >= 0 and idx_even < len(diffs[even_deg]):
                prod = t**2
                for i in range(1, k):
                    prod *= (t**2 - i**2)
                res += (prod / factorial(even_deg)) * diffs[even_deg][idx_even]
        return res

    @staticmethod
    def bessel(x_pts, y_pts, x_val):
        n = len(x_pts)
        if n < 4: return 0.0 # Bessel needs at least 4 points
        if n % 2 != 0: return 0.0 # Standard bessel needs even number of points
        
        mid = (n - 1) // 2
        h = x_pts[1] - x_pts[0]
        t = (x_val - x_pts[mid]) / h
        diffs = InterpolationMath.get_finite_diffs(y_pts)
        
        # We start with the base two points which are degree 0. Then add higher terms.
        res = (y_pts[mid] + y_pts[mid + 1]) / 2 + (t - 0.5) * diffs[1][mid]
        for k in range(2, (n//2) + 1):
             # Even degree terms 2k-2. For 2-nd degree term, k=2, so deg=2
             even_deg = 2*k - 2
             idx_start = mid - k + 1
             if idx_start >= 0 and idx_start + 1 < len(diffs[even_deg]):
                p = 1
                for i in range(k-1): p *= (t-i-1)*(t+i)
                term = (diffs[even_deg][idx_start] + diffs[even_deg][idx_start+1]) / 2
                res += (p / factorial(even_deg)) * term
                
             # Odd degree terms 2k-1. For 3-rd degree term, k=2, so deg=3
             odd_deg = 2*k - 1
             idx_odd = mid - k + 1
             if idx_odd >= 0 and idx_odd < len(diffs[odd_deg]):
                 p = t - 0.5
                 for i in range(1, k): p *= (t+i-1)*(t-i)
                 res += (p / factorial(odd_deg)) * diffs[odd_deg][idx_odd]
        return res


# --- ИНТЕРФЕЙС ПРИЛОЖЕНИЯ ---
class InterpolationApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Лаб №5: Интерполяция (Исправлено)")
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

        # Keeping user's Text widget height and width
        self.txt = tk.Text(sidebar, height=15, width=40)
        self.txt.insert("1.0", "0.1 1.25\n0.2 2.38\n0.3 3.79\n0.4 5.44\n0.5 7.14\n0.6 8.8\n0.7 10.46\n0.8 12.12\n0.9 13.78\n1.0 15.44\n1.1 17.1\n1.2 18.76")
        self.txt.pack(pady=5)

        self.func_box = ttk.Combobox(
            sidebar, values=["sin(x)", "exp(x)", "x^4+cos(x)"]
        )
        self.func_box.set("sin(x)")
        self.func_box.pack(fill="x")
        self.range_in = ttk.Entry(sidebar)
        self.range_in.insert(0, "0 1 12")
        self.range_in.pack(fill="x", pady=2)

        ttk.Button(sidebar, text="Загрузить из файла", command=self.load_file).pack(
            fill="x", pady=5
        )

        ttk.Label(sidebar, text="Точка X:").pack(anchor="w", pady=(10, 0))
        self.target_x = ttk.Entry(sidebar)
        self.target_x.insert(0, "0.25")
        self.target_x.pack(fill="x")

        ttk.Button(sidebar, text="РАССЧИТАТЬ", command=self.run).pack(fill="x", pady=10)

        # Output text height is kept
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

    def _build_treeview(self, parent, x, table, is_finite):
        n = len(x)
        # Fix column names to reflect table size
        if is_finite:
            cols = ["x", "y"] + [f"Δ^{i}y" for i in range(1, len(table))]
        else:
             # cols = ["x", "y"] + [f"f[x0...x{i}]" for i in range(1, table.shape[1])]
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
                pts = sorted([list(map(float, l.replace(",",".").split())) for l in lines if l.strip()])
                x, y = np.array([p[0] for p in pts]), np.array([p[1] for p in pts])
            else:
                a, b, n_pts = map(float, self.range_in.get().replace(",",".").split())
                x = np.linspace(a, b, int(n_pts))
                f_name = self.func_box.get()
                y = np.array(
                    [
                        (
                            sin(xi)
                            if "sin" in f_name
                            else (exp(xi) if "exp" in f_name else (xi**4 + cos(xi)))
                        )
                        for xi in x
                    ]
                )

            if len(x) < 2: raise Exception("Для интерполяции нужно как минимум 2 точки.")
            
            tx = float(self.target_x.get())
            
            # Recalculate finite diffs for Stirling/Bessel to ensure they work.
            # get_finite_diffs now expects 'n' is at least '2'
            f_diffs = InterpolationMath.get_finite_diffs(y)
            # Dividing diffs for tree view.
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
            # Stirling and Bessel need check
            st_text = f"{v_stir:.5f}" if v_stir != 0 else "-"
            be_text = f"{v_bess:.5f}" if v_bess != 0 else "-"
            res += f"Стирлинг (odd N):   {st_text}\n"
            res += f"Бессель (even N):   {be_text}\n"

            self.out.config(state="normal")
            self.out.delete("1.0", tk.END)
            self.out.insert("1.0", res)
            self.out.config(state="disabled")

            self.show_tables(x, y, d_diffs)
            self.plot(x, y, tx)
        except Exception as e:
            messagebox.showerror("Ошибка", f"Проверьте корректность данных.\n{str(e)}")
            # For debugging indexing errors:
            import traceback
            traceback.print_exc()

    def show_tables(self, x, y, d_table):
        win = tk.Toplevel(self)
        win.title("Таблицы разностей")
        tabs = ttk.Notebook(win)
        tabs.pack(fill="both", expand=True)

        f_frame = ttk.Frame(tabs)
        tabs.add(f_frame, text="Конечные разности")
        f_table = InterpolationMath.get_finite_diffs(y)
        self._build_treeview(f_frame, x, f_table, is_finite=True)

        d_frame = ttk.Frame(tabs)
        tabs.add(d_frame, text="Разделенные разности")
        self._build_treeview(d_frame, x, d_table, is_finite=False)

    def plot(self, x, y, tx):
        self.ax.clear()

        using_function = (self.input_mode.get() == "func")

        # Nodes of interpolation
        self.ax.scatter(x, y, color="black", s=50, label="Узлы интерполяции", zorder=5)

        x_grid = np.linspace(min(x), max(x), 200)

        # --- LAGRANGE ---
        try:
            y_lagr_grid = [InterpolationMath.lagrange(x, y, xi) for xi in x_grid]
            self.ax.plot(x_grid, y_lagr_grid, label="Лагранжа", color="yellow", linewidth=2.5, alpha=0.8)
        except Exception as e: print(f"Lagrange plot error: {e}")

        # --- NEWTON DIVIDED ---
        try:
            # We must use all grid points to draw the whole curve. The 'auto' function switches partway.
            y_newt_div_grid = []
            for xi in x_grid:
                 val, _ = InterpolationMath.newton_divided_auto(x, y, xi)
                 y_newt_div_grid.append(val)
            self.ax.plot(x_grid, y_newt_div_grid, label="Ньютона (разд.)", color="lime", linewidth=2.5)
        except Exception as e: print(f"Newton Divided plot error: {e}")

        # --- NEWTON FINITE ---
        try:
            # Check for constant H to plot finite difference methods.
            h_vals = np.diff(x)
            if not np.allclose(h_vals, h_vals[0], atol=1e-5):
                # Don't plot finite methods if H is not constant
                print("Skipping finite methods in plot due to non-constant H.")
            else:
                y_newt_fin_grid = []
                for xi in x_grid:
                     val, _ = InterpolationMath.newton_finite_auto(x, y, xi)
                     y_newt_fin_grid.append(val)
                self.ax.plot(x_grid, y_newt_fin_grid, label="Ньютона (кон.)", color="blue", linewidth=2.5)
                
                # --- STIRLING ---
                # Check odd number of points
                if len(x) % 2 != 0:
                     y_stir_grid = [InterpolationMath.stirling(x, y, xi) for xi in x_grid]
                     self.ax.plot(x_grid, y_stir_grid, label="Стирлинга (odd)", color="orange", linewidth=2)
                
                # --- BESSEL ---
                # Check even number of points
                if len(x) % 2 == 0:
                     y_bess_grid = [InterpolationMath.bessel(x, y, xi) for xi in x_grid]
                     self.ax.plot(x_grid, y_bess_grid, label="Бесселя (even)", color="cyan", linewidth=2)

        except Exception as e: 
            print(f"Finite method plot error: {e}")
            import traceback
            traceback.print_exc()

        # Target point calculation and mark. Use Lagrange as standard representation.
        res_y = InterpolationMath.lagrange(x, y, tx)
        self.ax.scatter(
            [tx],
            [res_y],
            color="magenta",
            s=120,
            edgecolors="white",
            linewidth=1.5,
            marker="*",
            label=f"Точка X={tx}",
            zorder=6,
        )

        if using_function:
            f_name = self.func_box.get()
            y_func_grid = []
            for xi in x_grid:
                if "sin" in f_name:
                    y_func_grid.append(sin(xi))
                elif "exp" in f_name:
                    y_func_grid.append(exp(xi))
                else:
                    y_func_grid.append(xi**4+cos(xi))

            self.ax.plot(x_grid, y_func_grid, label=f_name,
                        color="darkviolet", linewidth=2, linestyle=":")

        self.ax.set_title("Графики интерполяционных многочленов")
        self.ax.set_xlabel("X")
        self.ax.set_ylabel("Y")
        # Keep legend position
        self.ax.legend(loc="best", prop={'size': 8})
        self.ax.grid(True, linestyle=":", alpha=0.6)
        self.canvas.draw()


InterpolationApp().mainloop()