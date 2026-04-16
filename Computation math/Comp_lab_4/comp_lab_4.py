import tkinter as tk
from tkinter import ttk, messagebox, filedialog
import numpy as np
from math import *
import json
import matplotlib.pyplot as plt
from matplotlib.backends.backend_tkagg import FigureCanvasTkAgg


# --- МАТЕМАТИЧЕСКИЙ БЛОК  ---
class MathCore:

    @staticmethod
    def solve_slaue_gauss(A, B):
        try:
            n = len(B)
            matrix = np.column_stack((A, B)).astype(np.float64)
            for i in range(n):
                # max_row = i + np.argmax(np.abs(matrix[i:, i]))

                max_val = -1.0
                max_row = i
                for r in range(i, n):
                    val = matrix[r][i]
                    abs_val = val if val >= 0 else -val
                    if abs_val > max_val:
                        max_val = abs_val
                        max_row = r

                if max_row != i:
                    matrix[[i, max_row]] = matrix[[max_row, i]]

                if abs(matrix[i, i]) < 1e-18:
                    continue

                for k in range(i + 1, n):
                    ratio = matrix[k, i] / matrix[i, i]
                    for j in range(i, n + 1):
                        matrix[k, j] -= ratio * matrix[i, j]

            x = np.zeros(n)
            for i in range(n - 1, -1, -1):
                if abs(matrix[i, i]) < 1e-18:
                    x[i] = 0
                else:
                    sum_known = 0.0
                    for j in range(i + 1, n):
                        sum_known += matrix[i][j] * x[j]

                    x[i] = (matrix[i][n] - sum_known) / matrix[i][i]
            return x
        except:
            return np.zeros(len(B))

    @staticmethod
    def calculate_metrics(x, y, f):
        n=len(y)
        y_pred = np.array([f(val) for val in x])
        eps = y - y_pred
        s = 0
        for i in eps: s+=i**2
        # s = np.sum(eps**2)

        rms = sqrt(s / len(x))
        r2=0
        # r2 = 1 - (s / np.sum((y - np.mean(y)) ** 2))
        y_mean = 0
        for i in y: y_mean+=i
        y_mean/=n
        sum1=0
        for i in y:
            sum1+=(i-y_mean)**2
        r2 = 1 - (s/sum1)

        return {
            "S": float(s),
            "RMS": float(rms),
            "R2": float(r2),
            "y_pred": y_pred.tolist(),
        }

    @staticmethod
    def linear_approx(x, y):
        n = len(x)
        sx, sy, sx2, sxy = 0, 0, 0, 0
        # sx, sy, sx2, sxy = np.sum(x), np.sum(y), np.sum(x**2), np.sum(x * y)
        for i in range(n):
            sx += x[i]
            sy += y[i]
            sx2 += x[i]**2
            sxy += x[i]*y[i]

        A = [[sx2, sx], [sx, n]]
        B = [sxy, sy]
        coeffs = MathCore.solve_slaue_gauss(A, B)  # [a, b]
        a, b = coeffs[0], coeffs[1]

        # xm, ym = np.mean(x), np.mean(y)
        xm, ym = sx/n, sy/n
        s1, s2, s3 = 0, 0, 0
        # p = np.sum((x - xm) * (y - ym)) / (
        #     np.sqrt(np.sum((x - xm) ** 2) * np.sum((y - ym) ** 2)) or 1
        # )
        for i in range(n):
            s1 += (x[i]-xm)*(y[i]-ym)
            s2 += (x[i]-xm)**2
            s3 += (y[i]-ym)**2
        p = s1 / (sqrt(s1 * s2))

        return (
            (lambda t: a * t + b),
            f"y = {a:.4f}x + {b:.4f}",
            {"a": a, "b": b, "Pearson": p},
        )

    @staticmethod
    def poly2_approx(x, y):
        n = len(x)
        sx = []
        for i in range(5):
            s = 0
            for val in x:
                s+=val**i
            sx.append(s)

        sxy = []
        for i in range(3):
            s=0
            for j in range(n):
                s += y[j] * x[j]**i
            sxy.append(s)

        A = []
        for i in range(3):
            row = []
            for j in range(3):
                row.append(sx[i + j])
            A.append(row)

        c = MathCore.solve_slaue_gauss(A, sxy)
        return (
            (lambda t: c[2] * t**2 + c[1] * t + c[0]),
            f"y = {c[2]:.4f}x² + {c[1]:.4f}x + {c[0]:.4f}",
            {"coeffs": c.tolist()},
        )

    @staticmethod
    def poly3_approx(x, y):
        n = len(x)
        sx = []
        for i in range(7):
            s = 0
            for val in x:
                s+=val**i
            sx.append(s)

        sxy = []
        for i in range(4):
            s = 0
            for j in range(n):
                s += y[j] * x[j] ** i
            sxy.append(s)

        A = []
        for i in range(4):
            row = []
            for j in range(4):
                row.append(sx[i + j])
            A.append(row)

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
        a, b = exp(res["b"]), res["a"]
        return (
            (lambda t: a * exp(b * t)),
            f"y = {a:.4f} * e^({b:.4f}x)",
            {"a": a, "b": b},
        )

    @staticmethod
    def log_approx(x, y):
        if np.any(x <= 0):
            return None, None, None
        f_lin, _, res = MathCore.linear_approx(np.log(x), y)
        return (
            (lambda t: res["a"] * log(t) + res["b"]),
            f"y = {res['a']:.4f}ln(x) + {res['b']:.4f}",
            {"coeffs":res},
        )

    @staticmethod
    def pow_approx(x, y):
        if np.any(x <= 0) or np.any(y <= 0):
            return None, None, None
        f_lin, _, res = MathCore.linear_approx(np.log(x), np.log(y))
        a, b = exp(res["b"]), res["a"]
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
        ttk.Button(left, text="Загрузить JSON", command=self.load).pack(
            fill="x", pady=5
        )

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
            pts = [list(map(float, l.replace(",",".").split())) for l in lines if l.strip()]
            self.x = np.array([p[0] for p in pts])
            self.y = np.array([p[1] for p in pts])
            if (len(self.x)>12 or len(self.x)<8):
                messagebox.showwarning("Необходимо от 8 до 12 точек данных")

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

    def load(self):
        path = filedialog.askopenfilename(filetypes=[("JSON files", "*.json")])
        if path:
            try:
                with open(path, "r", encoding="utf-8") as f:
                    data = json.load(f)

                if "input_points" in data:
                    x_vals = data["input_points"]["x"]
                    y_vals = data["input_points"]["y"]

                    self.txt.delete("1.0", tk.END)
                    for xi, yi in zip(x_vals, y_vals):
                        self.txt.insert(tk.END, f"{xi} {yi}\n")

                    self.run()
                else:
                    messagebox.showerror(
                        "Ошибка", "В файле не найден ключ 'input_points'"
                    )
            except Exception as e:
                messagebox.showerror("Ошибка при чтении", str(e))


NumericalMethodsApp().mainloop()
