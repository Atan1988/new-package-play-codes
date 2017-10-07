setwd("~/GitHub/new-package-play-codes/Modeling/tfruns")

library(tfruns)

training_run("mnist_mlp.R")

training_run("mnist_mlp_2.R")

latest_run()

compare_runs()