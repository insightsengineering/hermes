# In order to ensure that `hermes` does not make `dplyr` filter method unusable,
# we have this separate test as we need to first load `dplyr` and then `hermes`.
library(dplyr)
library(hermes)
filter(iris, Species == "setosa")
