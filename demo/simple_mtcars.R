here::i_am("demo/simple_mtcars.R")

# This table is too long to be reasonable for formatting
tabtex(mtcars)

# Reformat the mtcars dataset and save it as a LaTeX table
cars <- mtcars[, c("mpg", "hp", "wt", "am")]
cars$am[cars$am == 1] <- "Manual"
cars$am[cars$am == 0] <- "Automatic"
names(cars) <- c("MPG", "Horsepower", "Weight (1000 lbs)", "Transmission")

# This table may still be unreasonably long, but it's clean enough to format
tabtex(cars, out = here::here("demo/out/simple_cars.tex"))
