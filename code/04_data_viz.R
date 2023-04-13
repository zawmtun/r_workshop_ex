# Data visualisation

# Preparation ----
# Load package
  
library(tidyverse)
library(patchwork)

# Read data
surveys2 <- readRDS("data/surveys2.rds")

# Base plot ----

ggplot(data = surveys2,
       mapping = aes(x = education, y = age))

# Add geom layer ----
## Histogram ----

ggplot(data = surveys2, mapping = aes(x = age)) +
  geom_histogram()

# Change binwidth

ggplot(data = surveys2, mapping = aes(x = age)) +
  geom_histogram(binwidth = 1)

## Density plot ----

ggplot(data = surveys2, mapping = aes(x = age)) +
  geom_density()

# Split the data by residence and change colours of the curves

ggplot(data = surveys2,
       mapping = aes(x = age, colour = residence)) +
  geom_density()

# Split the data by residence and fill area under the curve with colours

ggplot(data = surveys2,
       mapping = aes(x = age, fill = residence)) +
  geom_density()

# Make the plot transparent

ggplot(data = surveys2,
       mapping = aes(x = age, fill = residence)) +
  geom_density(alpha = 0.3)

## Scatter plot ----

ggplot(data = surveys2,
       mapping = aes(x = age, y = birth_order_last_child)) +
  geom_point()

# Assign plot to an object.

p1 <- ggplot(data = surveys2,
             mapping = aes(x = age, y = birth_order_last_child))

p1

p1 + geom_point()


ggplot(data = surveys2,
       aes(x = age, y = birth_order_last_child)) +
  geom_point()

# The plot shows that the birth order of the last child is higher among older
# women. That makes sense.

# But the points are too cramped and overlapped. Let's make the points more
# transparent. Alpha argument ranges from 0 (totally transparent) to 1 (totally
# opaque).

ggplot(data = surveys2,
       aes(x = age, y = birth_order_last_child)) +
  geom_point(alpha = 0.2)

# Let's disperse the points a little bit in random direction. We can do that using `geom_jitter()`.

ggplot(data = surveys2,
       aes(x = age, y = birth_order_last_child)) +
  geom_jitter(alpha = 0.2)

# Now, we see that most points are clustered between 20 and 30 years of age.

# Now add a colour to all points.

ggplot(data = surveys2,
       aes(x = age, y = birth_order_last_child)) +
  geom_jitter(alpha = 0.2, colour = "blue")

# We can also add colours by urban/rural residence (use `residence`). When the parameter value is a variable, we need to wrap `colour` in `aes()`.

ggplot(data = surveys2,
       aes(x = age, y = birth_order_last_child)) +
  geom_jitter(alpha = 0.2, aes(colour = residence))

# We do not see any obvious difference in the distribution between urban and rural populations.

## Boxplot ----

ggplot(data = surveys2,
       mapping = aes(x = education, y = birth_order_last_child)) +
  geom_boxplot()

# We can overlay points to the corresponding boxplot to have a better idea of the distribution of
# the measurements.

ggplot(data = surveys2,
       mapping = aes(x = education, y = birth_order_last_child)) +
  geom_boxplot(alpha = 0) +
  geom_jitter(alpha = 0.3, color = "tomato")

# Now the boxplot is behind the points. Let's change the code to bring the boxplot to the front.

ggplot(data = surveys2,
       mapping = aes(x = education, y = birth_order_last_child)) +
    geom_jitter(alpha = 0.3, color = "tomato") +
    geom_boxplot(alpha = 0)

### Practice ----

# 1.  Boxplots are useful summaries, but hide the shape of the distribution. For example, if there
# is a bimodal distribution, it would not be observed with a boxplot. An alternative to the boxplot
# is the violin plot, where the shape (of the density of points) is
# drawn.

# Replace the box plot with a violin plot; see `geom_violin()`.

ggplot(data = surveys2,
       mapping = aes(x = education,
                     y = birth_order_last_child,
                     fill = education)) +
  geom_violin()

# 2.  So far, we've looked at the distribution of birth order within education
# attainment categories. Try making a new plot to explore the distribution of
# another variable within education attainment categories.

# - Create boxplot for birth order. Overlay the boxplot layer on a jitter layer
#   to show actual measurements.

# - Add color to the data points on your boxplot according to urban/rural
#   residence.

ggplot(data = surveys2,
       mapping = aes(x = education, y = birth_order_last_child)) +
  geom_jitter(alpha = 0.2, aes(colour = residence)) +
  geom_boxplot(alpha = 0)

## Bar plots ----

# Let's count the mothers by their employment.

surveys2 |> 
  count(wealth)

# We can use a bar chart to display the distribution of wealth quantiles. Let's
# plot the employment on X-axis.

ggplot(data = surveys2, aes(x = wealth)) +
  geom_bar()

# Let's tell ggplot to plot each wealth quintile using a distinct colour.

ggplot(data = surveys2, aes(x = wealth, fill = wealth)) +
    geom_bar()

# Since the first argument of `ggplot()` is a dataframe for the plot, we can use
# `|>` to add data to `ggplot()`.

surveys2 |> 
  ggplot(aes(x = wealth, fill = wealth)) +
  geom_bar()

### Practice ----

# We have plotted wealth quintile vertically earlier. Now, plot the bars horizontally.

surveys2 |> 
  ggplot(aes(y = wealth, fill = wealth)) +
  geom_bar()

# Faceting ----

# Faceting is a powerful technique to plot subsets of data.

# Let's plot wealth quintile horizontally and facet our data by `residence` using `facet_wrap()`.

ggplot(data = surveys2, aes(y = wealth)) +
  geom_bar() +
  facet_wrap(facets = vars(residence))

# Note that we need to wrap the facetting variable in `vars()`.

# Now, let's disaggregate data in each facet by tetanus vaccination status using `facet_wrap()`.

ggplot(surveys2, aes(y = wealth, fill = tetanus_vacc)) +
  geom_bar() +
  facet_wrap(facets =  vars(residence))

# `facet_wrap()` show the panels in one dimension with titles at the top.

# It would be helpful to put columns of vaccinated and unvaccinated mothers side by side.

ggplot(surveys2, aes(y = wealth, fill = tetanus_vacc)) +
  geom_bar(position = "fill") +
  facet_wrap(facets =  vars(residence))

# We can also facet by both residence and state/region. Now, we want to facet
# the plot in two dimensions: residence on x-axis (panel title at the top) and
# age group on y-axis (panel title on the side). For that, we need to use
# `facet_grid()`.

ggplot(surveys2, aes(y = wealth, fill = tetanus_vacc)) +
  geom_bar(position = "fill") +
  facet_grid(rows = vars(state_region), cols =  vars(residence))

# You can also show the panels by rows.

ggplot(surveys2, aes(y = wealth, fill = tetanus_vacc)) +
  geom_bar(position = "fill") +
  facet_grid(rows = vars(agegrp))

# Or by columns.

ggplot(surveys2, aes(y = wealth, fill = tetanus_vacc)) +
  geom_bar(position = "fill") +
  facet_grid(cols = vars(agegrp))

# Themes ----

## Pre-defined themes ----

# In addition geoms and facets, we can change how the plot looks, like background colour, axis
# grids, font type/size, etc by using generic `theme()` function, as we will see below. There are
# also pre-defined themes already built in `ggplot2`. In the code below, `theme_bw()` produce a
# white background and dark panel borders, replacing the default grey background and white borders.

ggplot(surveys2, aes(y = wealth, fill = residence)) +
  geom_bar() +
  facet_wrap(vars(residence))


ggplot(surveys2, aes(y = wealth, fill = residence)) +
  geom_bar() +
  facet_wrap(vars(residence)) +
  theme_bw()

# There are several other pre-defined themes that come with `ggplot2`.

# -   `theme_linedraw()`
# -   `theme_light()`
# -   `theme_dark()`
# -   `theme_minimal()`
# -   `theme_classic()`
# -   `theme_void()`

# The `ggthemes` package provides a wide variety of options.

### Practice

# Try different pre-defined themes from ggplot2 in the code below:

ggplot(surveys2, aes(y = wealth, fill = residence)) +
  geom_bar() +
  facet_wrap(vars(residence)) +
  theme_light()

# Use the theming you just learned to create a plot to show distribution of birth order of last child by education attainment faceted by residence.

ggplot(surveys2, aes(x = education, y = birth_order_last_child)) +
  geom_jitter(alpha = 0.2, colour = "salmon") +
  geom_boxplot(alpha = 0) +
  facet_wrap(vars(residence))

ggplot(surveys2, aes(x = education, y = birth_order_last_child)) +
  geom_jitter(alpha = 0.2, colour = "salmon") +
  geom_boxplot(alpha = 0) +
  facet_wrap(vars(residence)) +
  theme_minimal()

## Customisation ----

# Take a look at the plot below and think of ways you could improve it.

ggplot(surveys2, aes(y = wealth, fill = residence)) +
  geom_bar() +
  facet_wrap(vars(residence)) +
  theme_bw()

# First, let's change the axis titles to something more informative than `count` and `wealth`. Then,
# add a plot title. We will use `labs()` for that.

ggplot(surveys2, aes(y = wealth, fill = residence)) +
  geom_bar() +
  labs(x = "Number of mothers", y = "Wealth quintile",
       title = "Wealth quintile distribution by urban and rural residence") +
  facet_wrap(vars(residence)) +
  theme_bw()

# Next, let's increase the font size to the texts more readable. We can do it using the generic `theme()` function.

ggplot(surveys2, aes(y = wealth, fill = residence)) +
  geom_bar() +
  labs(x = "Number of mothers", y = "Wealth quintile",
       title = "Wealth quintile distribution by urban and rural residence") +
  facet_wrap(vars(residence)) +
  theme_bw() +
  theme(text = element_text(size = 16))

# We can change the facet label text (`strip.text`) to italicize the genus names.

ggplot(surveys2, aes(y = wealth, fill = residence)) +
  geom_bar() +
  labs(x = "Number of mothers", y = "Wealth quintile",
       title = "Wealth quintile distribution by urban and rural residence") +
  facet_wrap(vars(residence)) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "italic"),
    text = element_text(size = 16)
  )

# If you want to apply the same theme to other plots, we can save the theme as a R object and pipe it to other ggplot2 objects like this:

my_theme <- theme_bw() +
  theme(
    strip.text = element_text(face = "italic"),
    text = element_text(size = 16)
  )

ggplot(surveys2, aes(x = education, y = birth_order_last_child)) +
  geom_jitter(alpha = 0.2, colour = "salmon") +
  geom_boxplot(alpha = 0) +
  facet_wrap(vars(residence)) +
  my_theme

### Practice ----

# With all of this information in hand, please take another five minutes to either improve the plot above.
# 
# Here are some ideas:
# 
# -   X-axis texts are not visible. See if you can rotate it to a certain degree.
# -   Add titles for axes and a plot title.
# -   Vertical gridlines seems redundant. Remove them.

ggplot(surveys2, aes(x = education, y = birth_order_last_child)) +
  geom_jitter(alpha = 0.2, colour = "salmon") +
  geom_boxplot(alpha = 0) +
  labs(x = "Educational attainment",
       y = NULL,
       title = "Birth order of the last child by educational attainment") +
  facet_wrap(vars(residence)) +
  my_theme +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Arranging plots ----

# Faceting is great for splitting a plot into multiple plots of the same type but sometimes we want to put together different plot types into one. In that case, we can use `patchwork` package to combine multiple ggplot2 objects. We can install the package using the following command.

# After loading patchwork package, we now can use `+` to combine two plots side-by-side and use `/` to put together the plots vertically. `plot_layout()` can determine how much space each plot uses.

p_birth <- ggplot(data = surveys2,
             aes(x = age, y = birth_order_last_child)) +
  geom_jitter(alpha = 0.2, aes(colour = residence)) +
  labs(x = "Age of mothers", y = "Birth order of last child",
       colour = NULL)

p_wealth <- ggplot(surveys2, aes(y = wealth, fill = wealth)) +
  geom_bar(show.legend = FALSE) +
  labs(x = "Number of mothers", y = "Wealth quintile")

p_birth / p_wealth + 
  plot_layout(heights = c(3, 2))

# `patchwork` can also produce more complex layouts. Check out [patchwork website](https://patchwork.data-imaginist.com/) for more examples.

# Exporting plots ----

# After creating a plot, let's save it as a file. `ggplot2` supports many formats: png, jpeg, pdf, and svg are popular.

my_plot <- ggplot(surveys2, aes(y = wealth, fill = residence)) +
  geom_bar() +
  labs(x = "Number of mothers", y = "Wealth quintile",
       title = "Wealth quintile distribution by urban and rural residence") +
  facet_wrap(vars(residence)) +
  theme_bw() +
  theme(
    strip.text = element_text(face = "italic"),
    text = element_text(size = 16)
  )

my_plot

ggsave("figs/wealth_by_residence.png", my_plot, width = 15, height = 10)

# This also works for plots combined with patchwork package.

plot_combined <- p_birth / p_wealth + 
  plot_layout(heights = c(3, 2))

plot_combined

ggsave("figs/plot_combined.png", plot_combined, width = 10, dpi = 300)
