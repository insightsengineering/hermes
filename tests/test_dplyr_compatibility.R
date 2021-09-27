# In order to ensure that `hermes` does not make `dplyr` functions unusable,
# we have these separate tests as we need to first load `dplyr` and then `hermes`.
library(dplyr)
library(hermes)
filter(iris, Species == "setosa")
rename(iris, petal_length = Petal.Length)
