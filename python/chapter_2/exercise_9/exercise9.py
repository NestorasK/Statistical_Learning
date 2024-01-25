import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

Auto = pd.read_csv("Data/Auto.csv", na_values="?")
Auto.dropna(axis=0, inplace=True)

# (a)
Auto.dtypes
Auto.describe()
Auto["cylinders"].value_counts()
Auto["year"].value_counts()
Auto["origin"].value_counts()
Auto["name"].value_counts()

quantitative = ["mpg", "displacement", "horsepower", "weight", "acceleration", "year"]
qualitative = []
for coli in list(Auto.columns):
    if coli not in quantitative:
        qualitative.append(coli)

# (b)
Auto_quantitative = Auto[quantitative]
df_range = pd.DataFrame(
    {
        "min": np.min(Auto_quantitative, axis=0),
        "max": np.max(Auto_quantitative, axis=0),
    }
)

# (c)
df_stats = pd.DataFrame(
    {
        "mean": np.mean(Auto_quantitative, axis=0),
        "std": np.std(Auto_quantitative, axis=0),
    }
)

# (d)
inds = np.zeros(Auto_quantitative.shape[0], bool)
inds[0:9] = True
inds[86 : len(inds)] = True
Auto_quantitative_part = Auto_quantitative[inds]

df_range_part = pd.DataFrame(
    {
        "min": np.min(Auto_quantitative_part, axis=0),
        "max": np.max(Auto_quantitative_part, axis=0),
    }
)
df_stats_part = pd.DataFrame(
    {
        "mean": np.mean(Auto_quantitative_part, axis=0),
        "std": np.std(Auto_quantitative_part, axis=0),
    }
)
Auto_quantitative_part.describe()

# (e)
myscatter = pd.plotting.scatter_matrix(frame=Auto_quantitative, figsize=(16, 16))
plt.savefig("python/chapter_2/exercise_9/scatter_matrix.pdf")
