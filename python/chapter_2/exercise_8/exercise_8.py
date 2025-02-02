import pandas as pd
import matplotlib.pyplot as plt

# (a)
college = pd.read_csv("Data/College.csv")

# (b)
college

college2 = pd.read_csv(filepath_or_buffer="Data/College.csv", index_col=0)
college2

college3 = college.rename({"Unnamed: 0": "College"}, axis=1)
college3
college3 = college3.set_index("College")
college3
college = college3

# (c)
college.describe()

# (d)
my_scatter = pd.plotting.scatter_matrix(college[["Top10perc", "Apps", "Enroll"]])
plt.savefig("python/chapter_2/exercise_8/exercise_8_scatterplot.pdf")

# (e)
fig, ax = plt.subplots(figsize=(8, 8))
college.boxplot("Outstate", by="Private", ax=ax)
ax.set_ylabel("Outstate")
fig.savefig("python/chapter_2/exercise_8/exercise_8_boxplot_e.pdf")

# (f)
college["Elite"] = pd.cut(college["Top10perc"], [0, 50, 100], labels=["No", "Yes"])
college["Elite"].value_counts()

fig, ax = plt.subplots(figsize=(8, 8))
college.boxplot(column="Outstate", by="Elite", ax=ax)
fig.savefig("python/chapter_2/exercise_8/exercise_8_boxplot_f.pdf")

# (g)
fig, axes = plt.subplots(figsize=(8, 8), nrows=2, ncols=2)
college.hist(column="Outstate", color="blue", ax=axes[0, 0])
college.hist(column="Personal", color="lightblue", bins=12, ax=axes[0, 1])
college.hist(column="Expend", color="red", bins=12, ax=axes[1, 0])
college.hist(column="PhD", color="grey", bins=12, ax=axes[1, 1])
fig.savefig("python/chapter_2/exercise_8/exercise_8_g.pdf")
