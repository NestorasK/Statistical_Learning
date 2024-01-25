# (a)
from ISLP import load_data
import matplotlib.pyplot as plt
import pandas as pd

Boston = load_data(dataset="Boston")
boston_df = pd.DataFrame(Boston)

# (b)
print(f"The boston dataset has {boston_df.shape[0]} rows - cases examined.")
print(f"The boston dataset has {boston_df.shape[1]} columns.")
print("The columns are:")
for coli in boston_df.columns:
    print(f" - {coli}")

# (c)
my_scatter = pd.plotting.scatter_matrix(frame=boston_df, figsize=(16, 16))
plt.savefig("python/chapter_2/exercise_10/scatter_matrix.pdf")
