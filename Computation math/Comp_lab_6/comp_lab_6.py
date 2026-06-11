import tkinter as tk
from tkinter import ttk, messagebox
import numpy as np
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg
from math import exp, sin, cos


class ODEMath:
    @staticmethod
    def get_equations():
        return [
            {
                "id": 1,
                "text": "y' = x + y  (y(0) = 1)",
                "f": lambda x, y: x + y,
                "exact": lambda x, x0, y0: 2.0 * exp(x) - x - 1.0,
                "default_inputs": ("0,0", "1,0", "2,0", "0,2", "0,001"),
            },
            {
                "id": 2,
                "text": "y' = x^2 - 2*y  (y(0) = 1)",
                "f": lambda x, y: x**2 - 2 * y,
                "exact": lambda x, x0, y0: 0.5 * x**2
                - 0.5 * x
                + 0.25
                + 0.75 * exp(-2 * x),
                "default_inputs": ("0,0", "1,0", "2,0", "0,2", "0,001"),
            },
            {
                "id": 3,
                "text": "y' = cos(x) - y  (y(0) = 1)",
                "f": lambda x, y: cos(x) - y,
                "exact": lambda x, x0, y0: 0.5 * sin(x) + 0.5 * cos(x) + 0.5 * exp(-x),
                "default_inputs": ("0,0", "1,0", "3,0", "0,2", "0,001"),
            },
        ]

    @staticmethod
    def euler_step(f, x, y, h):
        return y + h * f(x, y)

    @staticmethod
    def solve_euler(f, x0, y0, xn, h):
        steps = int(round((xn - x0) / h))
        x_pts = [x0]
        y_pts = [y0]
        for _ in range(steps):
            x_next = x_pts[-1] + h
            y_next = ODEMath.euler_step(f, x_pts[-1], y_pts[-1], h)
            x_pts.append(x_next)
            y_pts.append(y_next)
        return x_pts, y_pts

    @staticmethod
    def runge_kutta_4_step(f, x, y, h):
        k1 = h * f(x, y)
        k2 = h * f(x + h / 2, y + k1 / 2)
        k3 = h * f(x + h / 2, y + k2 / 2)
        k4 = h * f(x + h, y + k3)
        return y + (k1 + 2 * k2 + 2 * k3 + k4) / 6

    @staticmethod
    def solve_rk4(f, x0, y0, xn, h):
        steps = int(round((xn - x0) / h))
        x_pts = [x0]
        y_pts = [y0]
        for _ in range(steps):
            x_next = x_pts[-1] + h
            y_next = ODEMath.runge_kutta_4_step(f, x_pts[-1], y_pts[-1], h)
            x_pts.append(x_next)
            y_pts.append(y_next)
        return x_pts, y_pts

    @staticmethod
    def solve_adams(f, x0, y0, xn, h):
        steps = int(round((xn - x0) / h))

        x_pts = []
        for i in range(steps + 1):
            x_pts.append(x0 + i * h)
        y_pts = [y0]

        for i in range(min(3, steps)):
            y_pts.append(ODEMath.runge_kutta_4_step(f, x_pts[i], y_pts[i], h))

        for i in range(3, steps):
            f3 = f(x_pts[i], y_pts[i])
            f2 = f(x_pts[i - 1], y_pts[i - 1])
            f1 = f(x_pts[i - 2], y_pts[i - 2])
            f0 = f(x_pts[i - 3], y_pts[i - 3])

            y_next = y_pts[i] + (h / 24) * (55 * f3 - 59 * f2 + 37 * f1 - 9 * f0)
            y_pts.append(y_next)

        return x_pts, y_pts

    @staticmethod
    def estimate_runge_error(f, method_step_func, x, y, h, p):
        y_h = method_step_func(f, x, y, h)
        y_mid = method_step_func(f, x, y, h / 2)
        y_h2 = method_step_func(f, x + h / 2, y_mid, h / 2)
        return abs(y_h - y_h2) / (2**p - 1)


class ODEApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Лаб №6: Численное решение ОДУ")
        self.geometry("1200x850")
        self.equations = ODEMath.get_equations()

        self.current_x_grid = None
        self.current_y_euler = None
        self.current_y_rk4 = None
        self.current_y_adams = None
        self.current_exact_func = None
        self.current_x0 = None
        self.current_y0 = None
        self.current_xn = None
        self.current_eq_text = ""

        self._setup_ui()
        self._on_eq_change()

    def _setup_ui(self):
        main_frame = ttk.Frame(self, padding=10)
        main_frame.pack(fill="both", expand=True)

        sidebar = ttk.Frame(main_frame, width=320, padding=10)
        sidebar.pack(side="left", fill="y")
        sidebar.pack_propagate(False)

        ttk.Label(
            sidebar, text="1. Выберите уравнение:", font=("Arial", 10, "bold")
        ).pack(anchor="w", pady=5)
        self.eq_var = tk.IntVar(value=1)
        for eq in self.equations:
            ttk.Radiobutton(
                sidebar,
                text=eq["text"],
                variable=self.eq_var,
                value=eq["id"],
                command=self._on_eq_change,
            ).pack(anchor="w", pady=2)

        ttk.Separator(sidebar, orient="horizontal").pack(fill="x", pady=10)

        ttk.Label(
            sidebar, text="2. Параметры Коши и сетки:", font=("Arial", 10, "bold")
        ).pack(anchor="w", pady=5)

        grid_inputs = ttk.Frame(sidebar)
        grid_inputs.pack(fill="x")

        ttk.Label(grid_inputs, text="Начальное x0:").grid(
            row=0, column=0, sticky="w", pady=2
        )
        self.ent_x0 = ttk.Entry(grid_inputs, width=15)
        self.ent_x0.grid(row=0, column=1, sticky="e", pady=2)

        ttk.Label(grid_inputs, text="Начальное y0:").grid(
            row=1, column=0, sticky="w", pady=2
        )
        self.ent_y0 = ttk.Entry(grid_inputs, width=15)
        self.ent_y0.grid(row=1, column=1, sticky="e", pady=2)

        ttk.Label(grid_inputs, text="Конец отрезка xn:").grid(
            row=2, column=0, sticky="w", pady=2
        )
        self.ent_xn = ttk.Entry(grid_inputs, width=15)
        self.ent_xn.grid(row=2, column=1, sticky="e", pady=2)

        ttk.Label(grid_inputs, text="Шаг сетки h:").grid(
            row=3, column=0, sticky="w", pady=2
        )
        self.ent_h = ttk.Entry(grid_inputs, width=15)
        self.ent_h.grid(row=3, column=1, sticky="e", pady=2)

        ttk.Label(grid_inputs, text="Точность (для Рунге):").grid(
            row=4, column=0, sticky="w", pady=2
        )
        self.ent_eps = ttk.Entry(grid_inputs, width=15)
        self.ent_eps.grid(row=4, column=1, sticky="e", pady=2)

        ttk.Separator(sidebar, orient="horizontal").pack(fill="x", pady=10)

        ttk.Label(
            sidebar, text="3. Отображаемые методы:", font=("Arial", 10, "bold")
        ).pack(anchor="w", pady=5)
        self.show_exact = tk.BooleanVar(value=True)
        self.show_euler = tk.BooleanVar(value=True)
        self.show_rk4 = tk.BooleanVar(value=True)
        self.show_adams = tk.BooleanVar(value=True)

        ttk.Checkbutton(
            sidebar,
            text="Точное решение",
            variable=self.show_exact,
            command=self._update_plot_only,
        ).pack(anchor="w")
        ttk.Checkbutton(
            sidebar,
            text="Метод Эйлера",
            variable=self.show_euler,
            command=self._update_plot_only,
        ).pack(anchor="w")
        ttk.Checkbutton(
            sidebar,
            text="Метод Рунге-Кутты 4",
            variable=self.show_rk4,
            command=self._update_plot_only,
        ).pack(anchor="w")
        ttk.Checkbutton(
            sidebar,
            text="Метод Адамса",
            variable=self.show_adams,
            command=self._update_plot_only,
        ).pack(anchor="w")

        btn_calc = ttk.Button(
            sidebar, text="Рассчитать и построить", command=self._process_calculation
        )
        btn_calc.pack(fill="x", pady=15)

        self.notebook = ttk.Notebook(main_frame)
        self.notebook.pack(side="right", fill="both", expand=True, padx=10)

        self.tab_plot = ttk.Frame(self.notebook)
        self.notebook.add(self.tab_plot, text="График решений")
        self._init_matplotlib_figure()

        self.tab_table = ttk.Frame(self.notebook)
        self.notebook.add(self.tab_table, text="Таблица результатов")
        self._init_results_table()

    def _init_matplotlib_figure(self):
        self.fig, self.ax = plt.subplots(figsize=(7, 6))
        self.canvas = FigureCanvasTkAgg(self.fig, master=self.tab_plot)
        self.canvas.get_tk_widget().pack(fill="both", expand=True)

    def _init_results_table(self):
        columns = (
            "i",
            "x",
            "y_euler",
            "y_rk4",
            "y_adams",
            "y_exact",
            "err_runge_euler",
            "err_runge_rk4",
            "err_adams_max",
        )
        self.tree = ttk.Treeview(self.tab_table, columns=columns, show="headings")

        self.tree.heading("i", text="i")
        self.tree.heading("x", text="x")
        self.tree.heading("y_euler", text="Эйлер")
        self.tree.heading("y_rk4", text="Рунге-Кутта 4")
        self.tree.heading("y_adams", text="Адамс")
        self.tree.heading("y_exact", text="Точное")
        self.tree.heading("err_runge_euler", text="Погр. (Рунге Эйлер)")
        self.tree.heading("err_runge_rk4", text="Погр. (Рунге RK4)")
        self.tree.heading("err_adams_max", text="Погр. (Адамс, макс)")

        for col in columns:
            self.tree.column(col, width=110, anchor="center")

        scrollbar = ttk.Scrollbar(
            self.tab_table, orient="vertical", command=self.tree.yview
        )
        self.tree.configure(yscrollcommand=scrollbar.set)

        self.tree.pack(side="left", fill="both", expand=True)
        scrollbar.pack(side="right", fill="y")

    def _on_eq_change(self):
        eq_id = self.eq_var.get()
        eq = next(q for q in self.equations if q["id"] == eq_id)
        defaults = eq["default_inputs"]

        for ent, val in zip(
            [self.ent_x0, self.ent_y0, self.ent_xn, self.ent_h, self.ent_eps], defaults
        ):
            ent.delete(0, tk.END)
            ent.insert(0, val)

    def _parse_float(self, entry_widget):
        raw_val = entry_widget.get().strip()
        cleaned_val = raw_val.replace(",", ".")
        return float(cleaned_val)

    def _process_calculation(self):
        try:
            x0 = self._parse_float(self.ent_x0)
            y0 = self._parse_float(self.ent_y0)
            xn = self._parse_float(self.ent_xn)
            h = self._parse_float(self.ent_h)
            eps = self._parse_float(self.ent_eps)

            if h <= 0:
                raise ValueError("Шаг интегрирования h должен быть строго больше нуля.")
            if xn <= x0:
                raise ValueError(
                    "Конечная точка xn должна быть строго больше начальной x0."
                )

            steps = int(round((xn - x0) / h))
            if steps < 4:
                raise ValueError(
                    "Слишком большой шаг! Требуется минимум 4 точки сетки."
                )

        except ValueError as err:
            messagebox.showerror(
                "Ошибка ввода данных", f"Некорректные параметры: {str(err)}"
            )
            return

        eq_id = self.eq_var.get()
        eq = next(q for q in self.equations if q["id"] == eq_id)
        f = eq["f"]
        exact_sol = eq["exact"]

        x_grid, y_euler = ODEMath.solve_euler(f, x0, y0, xn, h)
        _, y_rk4 = ODEMath.solve_rk4(f, x0, y0, xn, h)
        _, y_adams = ODEMath.solve_adams(f, x0, y0, xn, h)

        self.current_x_grid = x_grid
        self.current_y_euler = y_euler
        self.current_y_rk4 = y_rk4
        self.current_y_adams = y_adams
        self.current_exact_func = exact_sol
        self.current_x0 = x0
        self.current_y0 = y0
        self.current_xn = xn
        self.current_eq_text = eq["text"]

        for item in self.tree.get_children():
            self.tree.delete(item)

        adams_errors = [
            abs(y_adams[i] - exact_sol(x_grid[i], x0, y0)) for i in range(len(x_grid))
        ]
        adams_max = max(adams_errors) if adams_errors else 0.0
        adams_max_str = f"{adams_max:.2e}"

        for i in range(len(x_grid)):
            curr_x = x_grid[i]
            y_ex = exact_sol(curr_x, x0, y0)

            if i < len(x_grid) - 1:
                euler_r_err = ODEMath.estimate_runge_error(
                    f, ODEMath.euler_step, curr_x, y_euler[i], h, p=1
                )
                rk4_r_err = ODEMath.estimate_runge_error(
                    f, ODEMath.runge_kutta_4_step, curr_x, y_rk4[i], h, p=4
                )
                euler_r_err_str = f"{euler_r_err:.2e}"
                rk4_r_err_str = f"{rk4_r_err:.2e}"
            else:
                euler_r_err_str = "-"
                rk4_r_err_str = "-"

            adams_display = adams_max_str if i == 0 else "-"

            self.tree.insert(
                "",
                "end",
                values=(
                    i,
                    f"{curr_x:.4f}",
                    f"{y_euler[i]:.6f}",
                    f"{y_rk4[i]:.6f}",
                    f"{y_adams[i]:.6f}",
                    f"{y_ex:.6f}",
                    euler_r_err_str,
                    rk4_r_err_str,
                    adams_display,
                ),
            )

        self._update_plot_only()

    def _update_plot_only(self):
        if self.current_x_grid is None:
            return

        self.ax.clear()

        if self.show_exact.get():
            x_dense = np.linspace(self.current_x0, self.current_xn, 200)
            y_dense = [
                self.current_exact_func(xi, self.current_x0, self.current_y0)
                for xi in x_dense
            ]
            self.ax.plot(
                x_dense,
                y_dense,
                label="Точное решение",
                color="darkviolet",
                linewidth=2.5,
            )

        if self.show_euler.get():
            self.ax.plot(
                self.current_x_grid,
                self.current_y_euler,
                label="Эйлер",
                color="crimson",
                linewidth=1.5,
                marker="o",
                linestyle="--",
            )

        if self.show_rk4.get():
            self.ax.plot(
                self.current_x_grid,
                self.current_y_rk4,
                label="Рунге-Кутта 4",
                color="navy",
                linewidth=1.5,
                marker="s",
                linestyle="-.",
            )

        if self.show_adams.get():
            self.ax.plot(
                self.current_x_grid,
                self.current_y_adams,
                label="Адамс",
                color="forestgreen",
                linewidth=2,
                marker="^",
                linestyle=":",
            )

        self.ax.set_title(f"Сравнение методов для {self.current_eq_text}")
        self.ax.set_xlabel("X")
        self.ax.set_ylabel("Y")
        self.ax.grid(True, linestyle=":", alpha=0.6)
        self.ax.legend()
        self.canvas.draw()


app = ODEApp()
app.mainloop()
