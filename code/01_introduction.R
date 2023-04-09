# Introduction to R ----

# RStudio
# Explain panes
# RStudio options: uncheck .RData at startup, rearrange panes


# Get output from R simply by typing math in the console.

3 + 5
12 / 7

## Saving your code in a R script ----

## Object assignment ----

# To do useful and interesting things, we need to store these outputs to objects. 
weight_kg <- 55

# Shortcut key for <- : `Alt` + `-` in Windows and `Options` + `-` in MacOS

weight_kg <- 55    # doesn't print anything
weight_kg          # and so does typing the name of the object

# Now that you have created `weight_kg` object in R environment (i.e., in your computer memory), you
# can check whether that is the case by checking Environment pane (shortcut `Ctrl/Cmd` + `8`). You
# should see `weight_kg` and the associated value there.

# do arithmetic with `weight_kg`
2.2 * weight_kg # pounds

# We can also change an object's value by assigning it a new one.

weight_kg <- 57.5
2.2 * weight_kg

# In R, assigning a value to one object does not change the values of other objects. For example, let's store the weight in pounds in a new object, `weight_lb`:

weight_lb <- 2.2 * weight_kg

# and then change weight_kg to 100.

weight_kg <- 100

# What do you think is the current content of the object `weight_lb`? 126.5 or 220?

## Naming conventions in R ----

# Object names can be anything such as `x`, `current_temperature`, or `subject_id` although there
# are some restrictions to it. Here are some guidelines on naming objects:
#
# -   Be explicit and not too long.
# -   Cannot start with a number (`2x` is not valid, but `x2` is).
# -   R is case sensitive. So, `weight_kg` is different from `Weight_kg`.
# -   Some names cannot be used because they are reserved words in R
#     (e.g., `if`, `else`, `for`, see https://stat.ethz.ch/R-manual/R-devel/library/base/html/Reserved.html) for a complete list)
# -   Use nouns for object names and verbs for function names.
# -   Be consistent in the styling of your code, such as where you put spaces, how you name objects,
#     etc. Naming styles include "lower_snake", "UPPER_SNAKE", "lowerCamelCase", "UpperCamelCase", etc.

## Comments ----

### Practice ----

# What are the values after each statement in the following?

mass <- 47.5            # mass?
age  <- 122             # age?
mass <- mass * 2.0      # mass?
age  <- age - 20        # age?
mass_index <- mass/age  # mass_index?

## Functions and their arguments

# An example of a function call
weight_kg <- sqrt(10)
weight_kg

# Read function documentation to find out arguments
?sqrt

# Let's try a function that can take multiple arguments: `round()`.

round(3.141593)

# Here, we have called `round()` with just one argument, `3.141593`, and it has returned the value
# `3`. That's because the default is to round to the nearest whole number. If we want more digits we
# can see how to do that by looking at the documentation of the `round` function. We can look at the
# help for this function using `?round` or put your cursor on `round` and press F1.

# In the documentation, we see that in addition a number, `round()` also takes digits argument that
# accepts a number indicating the number of decimal places. The default value of `digits` is 0:
# return a whole number. If we want a different decimal number, change `digits` argument.

round(3.141593, digits = 2)

# We can specify the argument value by position. Then, we don't need to add argument name in the function call. In `round()`, the first argument is a number and the second `digits`.

round(3.141593, 2)

# We can also specify the argument value by argument name.

round(x = 3.141593, digits = 2)

# This way the arguments can be in any position and is more fool-proof. Here, we specify `digits` first and `x` second.

round(digits = 2, x = 3.141593)

## Vectors and data types ----

### Numeric vectors ----
# Put together a series of values as a vector using the `c()` function.
weight_kg <- c(50, 60, 70, 80)
weight_kg
class(weight_kg)
length(weight_kg)


### Character vectors ----
countries <- c("Pakistan", "Singapore", "Bangladesh")
countries
class(weight_kg)
length(weight_kg)

# Characters are quoted in R. Without the quotes, R assumes that objects with names `Pakistan`,
# `Singapore`, and `Bangladesh` already exists in the environment (memory) and will look for those
# objects. When they are not found, R throws an error message.

# We can use `c()` to add more elements to a vector.

# Now, let's add a new number at the end of `weight_kg`.

weight_kg <- c(weight_kg, 99)
weight_kg

# Next, add a new number at the beginning of `weight_kg`.

weight_kg <- c(40, weight_kg)
weight_kg


### Practice ----

# 1. What happens if we try to mix these types in a single vector?

# 2. What will happen in each of these examples? (hint: use `class()` to check the data type of your
# objects):

num_char <- c(1, 2, 3, "a")
num_logical <- c(1, 2, 3, TRUE)
char_logical <- c("a", "b", "c", TRUE)
tricky <- c(1, 2, 3, "4")

# 3. Why do you think it happens?
#
# 4. How many values in `combined_logical` are `"TRUE"` (as a character) in the following example
# (reusing the 2 `..._logical`s from above):

combined_logical <- c(num_logical, char_logical)

# 5. You've probably noticed that objects of different types get converted into a single, shared type within a vector. In R, we call converting objects from one class into another class *coercion*. These conversions happen according to a hierarchy, whereby some types get preferentially coerced into other types. Can you draw a diagram that represents the hierarchy of how these data types are coerced?
#
# logical → numeric → character ← logical

# Convert between vectors ----

# `as.numeric()`: Convert vector to a `double` vector
# `as.integer()`: Convert vector to a `integer` vector
# `as.character()`: Convert vector to a `character` vector
# `as.logical()`: Convert vector to a `logical` vector

x <- c("1", "2", "3")
x
class(x)
x_dbl <- as.numeric(x) 
x_dbl
class(x_dbl)
x_int <- as.integer(x)
x_int
class(x_int)

y <- c(TRUE, FALSE)
y
class(x)
y_int <- as.integer(y)
y_int
class(y_int)
y_chr <- as.character(y)
y_chr
class(y_chr)


## Missing data ----

# Represented as `NA`
height_cm <- c(160, 180, NA, 167, 177)

# Running function on it will return `NA`. Add the argument `na.rm = TRUE` to remove missing values before calculating statistics
mean(height_cm)
max(height_cm)
mean(height_cm, na.rm = TRUE)
max(height_cm, na.rm = TRUE)

# If you want to programmatically check whether your data include `NA`, you can use `is.na()` to do it. The result is a logical vector of the same length with NA are indicated as `TRUE` and otherwise, `FALSE`. More on this later.
is.na(height_cm)

## Dataframe ----

# Most of our datasets are stored as data.frame.

# Dataframes are vectors of the same length arranged in columns

animal_data <- data.frame(
  animal = c("dog", "cat", "sea cucumber", "sea urchin"),
  feel = c("furry", "furry", "squishy", "spiny"),
  weight = c(45, 8, 1.1, 0.8)
)

animal_data
str(animal_data)
