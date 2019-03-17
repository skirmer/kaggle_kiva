#install.packages("reticulate")
library(reticulate)
?`reticulate-package`


use_condaenv("work")
pd <- import("pandas")
np <- import("numpy", convert = FALSE)
np2 <- import("numpy")
testing <- py_run_string("x = 10")
testing

#Experimenting with python typed vs R typed obj
a <- np$array(c(1:4))
a

b <- np2$array(c(1:4))
b

