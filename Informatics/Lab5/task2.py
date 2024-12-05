import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.patches as mpatches

df = pd.read_csv("2_task.csv", sep=";")

data = []
days = df["DATE"].unique()

for day in days:
    day_data = df[df["DATE"] == day]
    data.append(day_data["OPEN"].values)
    data.append(day_data["HIGH"].values)
    data.append(day_data["LOW"].values)
    data.append(day_data["CLOSE"].values)

labels = days.tolist()

plt.figure(figsize=(12, 6))

colors = ["cyan", "lime", "yellow", "lightsalmon"]
box = plt.boxplot(data, patch_artist=True)

for i in range(len(box["boxes"])):
    box["boxes"][i].set_facecolor(colors[i % 4])

plt.xticks(ticks=[i * 4 + 2.5 for i in range(4)], labels=labels)

legend_patches = [
    mpatches.Patch(color=colors[i], label=["OPEN", "HIGH", "LOW", "CLOSE"][i])
    for i in range(4)
]

plt.legend(handles=legend_patches)

plt.title("Диаграмма коробка с усами")
plt.grid(True)

plt.show()
