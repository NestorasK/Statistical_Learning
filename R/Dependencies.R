install.packages("ISLR")
install.packages("MASS")
install.packages("e1071")

# Deep learning
install.packages("remotes")
remotes::install_github("rstudio/tensorflow")
reticulate::install_python()
library(tensorflow)
install_tensorflow(envname = "r-tensorflow")
install_tensorflow(envname = "r-tensorflow", extra_packages = "pillow")
