import numpy as np
from matplotlib.pyplot import subplots

x = np.random.normal(size=50)
y = x + np.random.normal(loc=50, scale=1, size=50)

np.corrcoef(x, y)

rng = np.random.default_rng(seed=3)
y = rng.standard_normal(10)
np.mean(y), y.mean()


X = rng.standard_normal(size=(10, 3))
X
X.mean(axis=0)
X.mean(axis=1)

# Plot
fig, ax = subplots(figsize=(8, 8))
x = rng.standard_normal(100)
y = rng.standard_normal(100)
ax.plot(x, y, "o")
fig.savefig("python/chapter_2/test.pdf")

# Multiple plots
fig, axes = subplots(nrows=2, ncols=3, figsize=(15, 5))
axes[0, 1].plot(x, y, "o")
axes[1, 2].scatter(x, y, marker="+")
fig.savefig("python/chapter_2/test.pdf")


fig, ax = subplots(figsize=(8, 8))
x = np.linspace(-np.pi, np.pi, 50)
y = x
f = np.multiply.outer(np.cos(y), 1 / (1 + x**2))
ax.contour(x, y, f)
fig.savefig("python/chapter_2/figure_contour.pdf")


seq2 = np.arange(start=0, stop=10)
seq2[slice(0, 2)]

A = np.array(np.arange(16)).reshape((4, 4))
A[1, 2]
A[[1, 3]]
A[:, [0, 2]]
A[[1, 3]][:, [0, 2]]
A[np.ix_([1, 3], [0, 2, 3])]


keep_rows = np.zeros(A.shape[0], bool)
keep_rows[[1, 3]] = True
keep_rows
A[keep_rows]

import pandas as pd

Auto = pd.read_csv("Data/Auto.csv", na_values=["?"])
Auto["horsepower"]
np.unique(Auto["horsepower"])
Auto["horsepower"].sum()
Auto.shape

Auto_new = Auto.dropna()
Auto_new.shape

Auto = Auto_new
Auto.columns

Auto.index


Auto.cylinders = pd.Series(Auto.cylinders, dtype="category")
Auto.cylinders.dtype
fig, ax = subplots(figsize=(8, 8))
Auto.boxplot(column="mpg", by="cylinders", ax=ax)
fig.savefig("python/chapter_2/auto_boxplots.pdf")

fig, ax = subplots(figsize=(8, 8))
Auto.hist(column="mpg", ax=ax)
fig.savefig("python/chapter_2/auto_hist.pdf")

pd.plotting.scatter_matrix(Auto)
fig.savefig("python/chapter_2/scatter_matrix.pdf")

Auto[["mpg", "weight"]].describe()
