import pandas as pd
import seaborn as sns
import matplotlib.pyplot as plt
import os

# optional W&B logging
USE_WANDB = os.environ.get("USE_WANDB", "0") == "1"
if USE_WANDB:
    import wandb  # pip install wandb
    run = wandb.init(project="epidemic-ddqn", job_type="calib-heatmap")  # [web:535]

df = pd.read_csv("/Users/kaushikmuthukumar/Documents/projects/EpidemicSimulationNew/calib_surface.csv")  # beta, travel, loss [web:582]
pivot = df.pivot(index="beta", columns="travel", values="loss")  # 2D grid [web:582]

plt.figure(figsize=(8, 6))
ax = sns.heatmap(
    pivot.sort_index(ascending=True),
    cmap="mako", annot=False, cbar=True, linewidths=0.3, linecolor="white"
)  # seaborn.heatmap [web:582]
ax.set_title("Calibration loss surface (beta × travel)")
ax.set_xlabel("travel"); ax.set_ylabel("beta")
plt.tight_layout()
plt.savefig("calib_heatmap.png", dpi=180)
plt.close()

if USE_WANDB:
    wandb.log({"calibration/heatmap": wandb.Image("calib_heatmap.png")})  # log media [web:587]
    run.finish()
