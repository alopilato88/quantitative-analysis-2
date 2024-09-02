
# Base R ------------------------------------------------------------------

## Objects 

object_1 <- "Hello, World."

## Integer & Double Vectors

integer_vec <- c(1L, 2L, 50L)
numeric_vec <- c(1, 2, 50, 45.23)

## Character Vectors

character_vec <- c("1", "abc", "$#2")

## Logical Vector 

logical_vec <- c(TRUE, FALSE)

## Adding Attributes

days_of_week_1 <- 1:7 
names(days_of_week_1) <- c("mon", "tues", "wed", "thurs", "fri", "sat", "sun")
names(days_of_week_1)
attributes(days_of_week_1)

days_of_week_2 <- 1:14
dim(days_of_week_2) <- c(2, 7) # 2 Rows, 7 Columns
attributes(days_of_week_2)
class(days_of_week_2)

## Creating a factor

days_of_week_factor <- factor(c("mon", "tues", "wed", "thurs", "fri", "sat", "sun"))
typeof(days_of_week_factor)
attributes(days_of_week_factor)

## Creating a data frame

data_frame_1 <- data.frame(NUMERIC = c(1, 3), CHARACTER = c("a", "b"), 
                           LOGICAL = c(TRUE, FALSE))

View(data_frame_1)

## Selecting elements (rows and columns) 

data_frame_1[1, 1] # Index the row and/or column
data_frame_1[, 1] # Leaving the column or row index blank selects the whole vector
data_frame_1$NUMERIC # Use a $ operator to reference the column name

## Functions in R

sum(c(1, 3))

x <- c(1, 4, 6)
sum(x) 
mean(x)
min(x)

## Linking (or chaining) functions together 

sum(abs(c(-1, -1, 1, 1)))

c(-1, -1, 1, 1) |>
  abs() |>
  sum()

## Packages & Reading Data

data_employees <- peopleanalytics::employees

# Tidyverse ---------------------------------------------------------------

library(tidyverse)

## Tibbles 

data_employees_tbl <- tibble::as_tibble(data_employees)
data_employees_tbl

## dplyr -- Row Functions

dplyr::filter(data_employees_tbl, job_lvl %in% c(4, 5))

dplyr::distinct(data_employees_tbl, ed_lvl, ed_field)

dplyr::arrange(data_employees_tbl, work_exp)

## dplyr -- Column Functions

dplyr::select(data_employees_tbl, dept)

dplyr::rename(data_employees_tbl, job_level = job_lvl)

dplyr::mutate(data_employees_tbl, salary = monthly_comp * 12)

dplyr::relocate(data_employees_tbl, job_lvl, .before = employee_id)

## dplyr -- Operate on Groups 

data_employees_tbl |>
  dplyr::group_by(
    job_lvl
  ) |>
  dplyr::summarize(
    annual_comp_mean = mean(annual_comp),
    annual_comp_median = median(annual_comp)
  )

## dplyr -- Table Functions

data_job <- peopleanalytics::job |> tibble::as_tibble()

data_payroll <- peopleanalytics::payroll |> tibble::as_tibble()

data_job_payroll <- 
  data_job |>
  dplyr::left_join(
    data_payroll,
    by = "employee_id"
  )

