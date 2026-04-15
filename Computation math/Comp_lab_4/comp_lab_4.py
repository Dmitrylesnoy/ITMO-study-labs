import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import numpy as np
import json
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg


# --- МАТЕМАТИЧЕСКИЙ БЛОК  ---
class MathCore:
    @staticmethod
    def solve_slaue_gauss(A, B):
        """метод Гаусса с выбором главного элемента"""
        try:
            n = len(B)
            matrix = np.column_stack((A, B)).astype(np.float64)
            for i in range(n):
                max_row = i + np.argmax(np.abs(matrix[i:, i]))
                matrix[[i, max_row]] = matrix[[max_row, i]]
                if abs(matrix[i, i]) < 1e-18:
                    continue
                for j in range(i + 1, n):
                    ratio = matrix[j, i] / matrix[i, i]
                    matrix[j, i:] -= ratio * matrix[i, i:]
            x = np.zeros(n)
            for i in range(n - 1, -1, -1):
                if abs(matrix[i, i]) < 1e-18:
                    x[i] = 0
                else:
                    x[i] = (
                        matrix[i, n] - np.dot(matrix[i, i + 1 : n], x[i + 1 : n])
                    ) / matrix[i, i]
            return x
        except:
            return np.zeros(len(B))

    @staticmethod
    def calculate_metrics(x, y, f):
        y_pred = np.array([f(val) for val in x])
        eps = y - y_pred
        s = np.sum(eps**2)
        rms = np.sqrt(s / len(x))
        r2 = 1 - (s / np.sum((y - np.mean(y)) ** 2)) # Коэффициент детерминации
        return {
            "S": float(s),
            "RMS": float(rms),
            "R2": float(r2),
            "y_pred": y_pred.tolist(),
        }

    @staticmethod
    def linear_approx(x, y):
        n = len(x)
        sx, sy, sx2, sxy = np.sum(x), np.sum(y), np.sum(x**2), np.sum(x * y)
        A = [[sx2, sx], [sx, n]]
        B = [sxy, sy]
        coeffs = MathCore.solve_slaue_gauss(A, B)  # [a, b]
        a, b = coeffs[0], coeffs[1]

        xm, ym = np.mean(x), np.mean(y)
        p = np.sum((x - xm) * (y - ym)) / (
            np.sqrt(np.sum((x - xm) ** 2) * np.sum((y - ym) ** 2)) or 1
        )
        return (
            (lambda t: a * t + b),
            f"y = {a:.4f}x + {b:.4f}",
            {"a": a, "b": b, "Pearson": p},
        )

    @staticmethod
    def poly2_approx(x, y):
        sx = [np.sum(x**i) for i in range(5)]
        sxy = [np.sum(y * (x**i)) for i in range(3)]
        A = [[sx[i + j] for j in range(3)] for i in range(3)]
        c = MathCore.solve_slaue_gauss(A, sxy)  # [a0, a1, a2]
        return (
            (lambda t: c[2] * t**2 + c[1] * t + c[0]),
            f"y = {c[2]:.4f}x² + {c[1]:.4f}x + {c[0]:.4f}",
            {"coeffs": c.tolist()},
        )

    @staticmethod
    def poly3_approx(x, y):
        sx = [np.sum(x**i) for i in range(7)]
        sxy = [np.sum(y * (x**i)) for i in range(4)]
        A = [[sx[i + j] for j in range(4)] for i in range(4)]
        c = MathCore.solve_slaue_gauss(A, sxy)
        return (
            (lambda t: c[3] * t**3 + c[2] * t**2 + c[1] * t + c[0]),
            f"y = {c[3]:.4f}x³ + {c[2]:.4f}x² + {c[1]:.4f}x + {c[0]:.4f}",
            {"coeffs": c.tolist()},
        )

    @staticmethod
    def exp_approx(x, y):
        if np.any(y <= 0):
            return None, None, None
        f_lin, _, res = MathCore.linear_approx(x, np.log(y))
        a, b = np.exp(res["b"]), res["a"]
        return (
            (lambda t: a * np.exp(b * t)),
            f"y = {a:.4f} * e^({b:.4f}x)",
            {"a": a, "b": b},
        )

    @staticmethod
    def log_approx(x, y):
        if np.any(x <= 0):
            return None, None, None
        f_lin, _, res = MathCore.linear_approx(np.log(x), y)
        return (
            (lambda t: res["a"] * np.log(t) + res["b"]),
            f"y = {res['a']:.4f}ln(x) + {res['b']:.4f}",
            {"coeffs":res},
        )

    @staticmethod
    def pow_approx(x, y):
        if np.any(x <= 0) or np.any(y <= 0):
            return None, None, None
        f_lin, _, res = MathCore.linear_approx(np.log(x), np.log(y))
        a, b = np.exp(res["b"]), res["a"]
        return (lambda t: a * (t**b)), f"y = {a:.4f} * x^{b:.4f}", {"a": a, "b": b}


# --- ИНТЕРФЕЙС ---
class NumericalMethodsApp(tk.Tk):
    def __init__(self):
        super().__init__()
        self.title("Лабораторная 4. Аппроксимация.")
        self.geometry("1100x750")
        self.results = []
        self.visibility_vars = {}
        self._init_ui()

    def _init_ui(self):
        left = ttk.Frame(self, padding=10)
        left.pack(side=tk.LEFT, fill=tk.Y)

        self.txt = tk.Text(left, width=25, height=10)
        self.txt.insert(
            "1.0",
            "1.1 2.73\n2.3 5.12\n3.7 7.74\n4.5 8.91\n5.8 10.59\n6.3 12.75\n7.2 13.43\n8.1 14.88",
        )
        self.txt.pack()

        ttk.Button(left, text="РАССЧИТАТЬ", command=self.run).pack(fill="x", pady=5)
        ttk.Button(left, text="Сохранить JSON", command=self.save).pack(fill="x")

        self.checks = ttk.LabelFrame(left, text="Графики функций", padding=5)
        self.checks.pack(fill="x", pady=10)

        right = ttk.Frame(self, padding=10)
        right.pack(side=tk.RIGHT, fill=tk.BOTH, expand=True)

        self.fig, self.ax = plt.subplots(figsize=(5, 4))
        self.canvas = FigureCanvasTkAgg(self.fig, master=right)
        self.canvas.get_tk_widget().pack(fill="both", expand=True)

        cols = ("name", "rms", "r2")
        self.tree = ttk.Treeview(right, columns=cols, show="headings", height=6)
        for c in cols:
            self.tree.heading(c, text=c.upper())
        self.tree.pack(fill="x")

    def run(self):
        try:
            lines = self.txt.get("1.0", tk.END).strip().split("\n")
            pts = [list(map(float, l.split())) for l in lines if l.strip()]
            self.x = np.array([p[0] for p in pts])
            self.y = np.array([p[1] for p in pts])

            methods = [
                ("Линейная", MathCore.linear_approx),
                ("Полином 2", MathCore.poly2_approx),
                ("Полином 3", MathCore.poly3_approx),
                ("Экспонента", MathCore.exp_approx),
                ("Логарифм", MathCore.log_approx),
                ("Степенная", MathCore.pow_approx),
            ]

            self.results = []
            for name, m_func in methods:
                f, formula, params = m_func(self.x, self.y)
                if f:
                    met = MathCore.calculate_metrics(self.x, self.y, f)
                    self.results.append(
                        {
                            "name": name,
                            "f": f,
                            "formula": formula,
                            "metrics": met,
                            "params": params,
                        }
                    )

            self.results.sort(key=lambda x: x["metrics"]["RMS"])
            self._update_table_and_checks()
            self.plot()
        except Exception as e:
            messagebox.showerror("Err", str(e))

    def _update_table_and_checks(self):
        for i in self.tree.get_children():
            self.tree.delete(i)
        for widget in self.checks.winfo_children():
            widget.destroy()

        self.visibility_vars = {}
        for r in self.results:
            self.tree.insert(
                "",
                "end",
                values=(
                    r["name"],
                    f"{r['metrics']['RMS']:.4f}",
                    f"{r['metrics']['R2']:.4f}",
                ),
            )
            v = tk.BooleanVar(
                value=(r == self.results[0])
            )
            self.visibility_vars[r["name"]] = v
            ttk.Checkbutton(
                self.checks, text=r["name"], variable=v, command=self.plot
            ).pack(anchor="w")

    def plot(self):
        self.ax.clear()
        self.ax.scatter(self.x, self.y, color="red", label="Data")
        x_grid = np.linspace(min(self.x) - 0.5, max(self.x) + 0.5, 100)

        for r in self.results:
            if self.visibility_vars[r["name"]].get():
                self.ax.plot(x_grid, [r["f"](val) for val in x_grid], label=r["name"])

        self.ax.legend()
        self.ax.grid(True)
        self.canvas.draw()

    def save(self):
        path = filedialog.asksaveasfilename(defaultextension=".json")
        if path:
            out = {
                "input_points": {
                    "x": self.x.tolist(),
                    "y": self.y.tolist()
                },
                "approximations": [
                    {
                        "name": r['name'],
                        "formula": r['formula'],
                        "metrics": {
                            "S": r['metrics']['S'],
                            "RMS": r['metrics']['RMS'],
                            "R2": r['metrics']['R2']
                        },
                        "parameters": r['params']  # Сохраняем весь 3-й словарь
                    } for r in self.results
                ]
            }
            with open(path, "w", encoding="utf-8") as f:
                json.dump(out, f, indent=4, ensure_ascii=False)


NumericalMethodsApp().mainloop()
