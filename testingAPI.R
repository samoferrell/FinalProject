library(plumber)
r <- plumb("myAPI.R")
r$run(port = 8000)
