import pandas as pd
import numpy as np
from matplotlib.pyplot import subplots

x = np.array([3, 4, 5])
y = np.array([4, 9, 7])

x + y

x = np.array([[1, 2], [3, 4]])
x

x.ndim
x.dtype

np.array([[1, 2], [3.0, 4]]).dtype

np.array([[1, 2], [3, 4]], dtype=float).dtype

x.shape

x = np.array([1, 2, 3, 4])
x.sum()

np.sum(a=x)


x = np.array([1, 2, 3, 4, 5, 6])
print("beginning x:\n", x)

x_reshape = x.reshape((2, 3))
print("reshaped x:\n", x_reshape)


x_reshape.shape
x_reshape.ndim
x_reshape.T

np.sqrt(x)

x**2


x = np.random.normal(size=50)
y = x + np.random.normal(loc=50, scale=1, size=50)

np.corrcoef(x, y)

rng = np.random.default_rng(seed=1303)
print(rng.normal(scale=5, size=2))

rng = np.random.default_rng(seed=3)
y = rng.standard_normal(10)
np.mean(y), y.mean()
np.var(y), y.var(), np.mean((y - y.mean()) ** 2)


X = rng.standard_normal(size=(10, 3))
X
X.mean(axis=0)
X.mean(axis=1)

# 2.3.4 Graphics
fig, ax = subplots(figsize=(8, 8))
x = rng.standard_normal(100)
y = rng.standard_normal(100)
ax.plot(x, y)
fig.savefig("python/chapter_2/test.pdf")

fig, ax = subplots(figsize=(8, 8))
ax.plot(x, y, "o")
fig.savefig("python/chapter_2/test_scatterplot.pdf")

fig, ax = subplots(figsize=(8, 8))
ax.scatter(x, y, marker="o")
fig.savefig("python/chapter_2/test_scatterplot2.pdf")

fig, ax = subplots(figsize=(8, 8))
ax.scatter(x, y, marker="o")
ax.set_xlabel("this is the x-axis")
ax.set_ylabel("this is the y-axis")
ax.set_title("Plot of X vs Y")
fig.set_size_inches(12, 3)
fig.savefig("python/chapter_2/test_scatterplot3.pdf")

# Multiple plots
fig, axes = subplots(nrows=2, ncols=3, figsize=(15, 5))
axes[0, 1].plot(x, y, "o")
axes[1, 2].scatter(x, y, marker="+")
fig.savefig("python/chapter_2/multiple_plots.pdf")

axes[0, 1].set_xlim([-1, 1])
fig.savefig("python/chapter_2/multiple_plots_updated.pdf")


fig, ax = subplots(figsize=(8, 8))
x = np.linspace(-np.pi, np.pi, 50)
y = x
f = np.multiply.outer(np.cos(y), 1 / (1 + x**2))
ax.contour(x, y, f)
fig.savefig("python/chapter_2/figure_contour.pdf")

# 2.3.5 Sequences and Slice Notation
seq1 = np.linspace(start=0, stop=10, num=11)
seq1
seq2 = np.arange(start=0, stop=10)
seq2
seq2[slice(0, 2)]
seq2[0:3]

"hello world"[3:6]

# 2.3.6 Indexing Data
A = np.array(np.arange(16)).reshape((4, 4))
A[1, 2]
A[[1, 3]]
A[:, [0, 2]]
A[[1, 3]][:, [0, 2, 3]]
idx = np.ix_([1, 3], [0, 2, 3])
A[idx]
A[1:4:2, 0:3:2]


# Boolean Indexing
keep_rows = np.zeros(A.shape[0], bool)
keep_rows[[1, 3]] = True
keep_rows
A[keep_rows]


np.all(keep_rows == np.array([0, 1, 0, 1]))
A[np.array([0, 1, 0, 1])]
A[keep_rows]

keep_cols = np.zeros(A.shape[1], bool)
keep_cols[[0, 2, 3]] = True
idx_bool = np.ix_(keep_rows, keep_cols)
A[idx_bool]

idx_mixed = np.ix_([1, 3], keep_cols)
A[idx_mixed]

# 2.3.7 Loading data
Auto = pd.read_csv("Data/Auto.csv")
Auto["horsepower"]
np.unique(Auto["horsepower"])

Auto = pd.read_csv("Data/Auto.csv", na_values=["?"])
Auto["horsepower"].sum()
Auto.shape

Auto_new = Auto.dropna()
Auto_new.shape

Auto = Auto_new
Auto.columns

Auto[:3]
idx_80 = Auto["year"] > 80
Auto[idx_80]
Auto[["mpg", "horsepower"]]
Auto.index

Auto_re = Auto.set_index("name")
Auto_re
Auto_re.columns

rows = ["amc rebel sst", "ford torino"]
Auto_re.loc[rows]
Auto_re.iloc[[3, 4]]

Auto_re.iloc[:, [0, 2, 3]]
Auto_re.iloc[[3, 4], [0, 2, 3]]
Auto_re.loc["ford galaxie 500", ["mpg", 'origin']]
idx_80 = Auto_re["year"] > 80

Auto_re.loc[idx_80, ['weight', 'origin']]

Auto_re.loc[lambda df: df["year"] > 80, ["weight", "origin"]]

Auto_re.loc[lambda df: (df["year"] > 80) & (
    df["mpg"] > 30), ["weight", "origin"]]

Auto_re.loc[lambda df: (df.index.str.contains("ford") | df.index.str.contains(
    "datsun")) & (df["displacement"] < 300), ["weight", "origin"]]


# 2.3.8 For loops
total = 0
for value in [3, 2, 19]:
    total += value

print(f'total is: {total}')


total = 0
for value in [2, 3, 19]:
    total = total + value
    for weight in [3, 2, 1]:
        total += (value * weight)

print(f'total is: {total}')


total = 0
for value, weight in zip([2, 3, 19], [0.2, 0.3, 0.5]):
    total += value * weight

print(f"Weighted average is: {total}")


# String Formatting
rng = np.random.default_rng(seed=1)
A = rng.standard_normal((127, 5))
M = rng.choice([0, np.nan], p=[0.8, 0.2], size=A.shape)
A += M
D = pd.DataFrame(A, columns=["food", "bar", "pickle", "snack", "popcorn"])
D[:3]
D["food"].isna().mean()

for col in D.columns:
    template = "Column '{0}' has {1:.2%} missing values"
    print(template.format(col, np.isnan(D[col]).mean()))


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
