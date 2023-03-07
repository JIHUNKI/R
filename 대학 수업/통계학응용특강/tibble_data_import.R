library(tidyverse)

vignette("tibble")

## Creating tibbles

#`tibble()` works by assembling individual vectors:
  
  
x <-c(1, 2, 5)
y <-c("a", "b", "h")
tibble(x, y)

tibble(
  x1 = x,
  x2 = c(10, 15, 25),
  y = sqrt(x1^2 + x2^2)
)

#Every column in a data frame or tibble must be same length:

tibble(
  x = c(1, 5),
  y = c("a", "b", "c")
)

#Individual values will be recycled to the same length as everything else:
  
tibble(
  x = 1:5,
  y = "a",
  z = TRUE
)

#`tribble()`: short for **tr**ansposed t**ibble**.

#`column headings start with `~` and entries are separated by commas.

tribble(
  ~x, ~y, ~z,
  "a", 2, 3.6,
  "b", 1, 8.5
)


## Creating tibbles 

#`data.frame` can be turned into a tibble with `as_tibble()`:
  
  #The inverse of `as_tibble()` is `as.data.frame()`.

mtcars
(temp<-as_tibble(mtcars))
as.data.frame(temp)




## Non-syntactic names

tb <-tibble(
  `:)` = "smile", 
  ` ` = "space",
  `2000` = "number"
)
tb




## Tibbles vs. data.frame

### Printing

tibble(
  a = lubridate::now() + runif(1e3) * 86400,
  b = lubridate::today() + runif(1e3) * 30,
  c = 1:1e3,
  d = runif(1e3),
  e = sample(letters, 1e3, replace = TRUE)
)

#Explicitly `print()` the data frame and control the number of rows (`n`) and the `width` of the display. 

# `width = Inf` will display all columns:
  
  
library(nycflights13)
flights %>% 
  print(n = 10, width = Inf)

#  `options(tibble.print_max = n, tibble.print_min = m)`: print only `m` rows. 

# `options(tibble.print_min = Inf)`: print all rows.

#  `options(tibble.width = Inf)`: print all columns, regardless of the width of the screen.

package?tibble


flights %>% View()

### Extracting variables

#To pull out a single variable, use `dplyr::pull()`:
  
tb <-tibble(
  id = LETTERS[1:5],
  x1  = 1:5,
  y1  = 6:10
)
tb %>% pull(x1) # by name
tb %>% pull(1)  # by position
tb$x1
tb[[1]]
#Specifies the column to be used as names for a named vector.

tb %>% pull(x1, name = id)


tb %>% pull(2,1)

#`[[` can extract by name or position; `$` only extracts by name. 


# Extract by name
tb$x1
tb[["x1"]]
# Extract by position
tb[[1]]

# Tibbles never do partial matching

tb$x
tb$z
# Data frame use partial matching and don't complain if a column doesn't exist
(df <-as.data.frame(tb))
df$x
df$z

#To use these in a pipe, you’ll need to use the special placeholder .:
  
tb %>% .$x1
tb %>% .[[1]]

#Using `[` returns another tibble.


tb[1]
tb[1:2]

## Data Import

read_lines("students.csv") %>% cat(sep = "\n")

#`read_csv()`prints out the column specifications (name, type).

students <- read_csv("students.csv")
spec(students)

###  Inline csv file.
read_csv("a,b,c
1,2,3
4,5,6")

# `skip = n` : skip the first `n` lines
# `comment = "#"`: drop all lines that start with (e.g.) `#`.

read_csv("The first line of metadata
      The second line of metadata
      x,y,z
      1,2,3", skip = 2)
read_csv("# A comment I want to skip
      x,y,z
      1,2,3", comment = "#")


## Reading data from a file `read_csv()`

# If the data do not have column names, set `col_names = FALSE`
# (`"\n"` is a convenient shortcut for adding a new line.) 

read_csv("1,2,3\n4,5,6", col_names = FALSE)

# Set your column names using `col_names`:

read_csv("1,2,3\n4,5,6", col_names = c("x", "y", "z"))

# Specify `NA`:

read_csv("a,b,c\n1,2,.", na = ".")


## `students` data

# In the `favourite.food` column, there is a character string `N/A`, which we need to treat it as `NA`. 

students <- read_csv("students.csv", na = c("N/A", ""))
students

# Transform variable names into snake case at once

library(janitor)
students %>%
  clean_names()

# Convert `meal_type` to a factor:

students <- students %>%
  clean_names() %>%
  mutate(meal_plan = factor(meal_plan))
students

## Reading data from multiple files 

# `read_csv()` read multiple files and stack them on top of each other in a single data frame.

# `id` added a new column called `file` to the resulting data frame that identifies the file the data come from.

sales_files <- c("01-sales.csv", "02-sales.csv", "03-sales.csv")
read_csv(sales_files, id = "file")



### Matching a pattern in file names

# `dir()`: find the files by matching a pattern in the file names.


sales_files <- dir("data", pattern = "sales\\.csv$", full.names = TRUE)
sales_files

#`fs::dir_ls()` is also useful.

library(fs)
sales_files <- dir_ls("data", glob = "*sales.csv")
sales_files


## Writing to a file

write_csv(students, "students.csv")

students
write_csv(students, "students-2.csv")
read_csv("students-2.csv")

#  `write_rds()` and `read_rds()` are uniform wrappers around the base functions `readRDS()` and `saveRDS()`.

write_rds(students, "students.rds")
read_rds("students.rds")

# The feather package implements a fast binary file format that can be shared across programming languages

library(feather)
write_feather(students, "students.feather")
read_feather("students.feather")

