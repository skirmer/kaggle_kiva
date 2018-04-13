# One time code for starting the project

loans <- read.csv("~/kaggle_kiva/data/kiva_loans.csv", stringsAsFactors = FALSE)
feather::write_feather(loans, "~/kaggle_kiva/data/loans.feather")
