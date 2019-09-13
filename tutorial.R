# Introduction to R
# Rick Scavetta
# 11.09.2019
# DAwR workshop in Ulm

# Clear the workspace
rm(list = ls())

# load packages
# First, install packages ONCE
# load every session
library(tidyverse)

# R syntax
n <- log2(8)
n

# PlantGrowth case study
# Using a built-in data set
PlantGrowth

# Make this a "tibble"
PlantGrowth <- as_tibble(PlantGrowth)

# This prints a nicer version:
PlantGrowth

# Explore - Descriptive statistics
# Number of groups (i.e. levels)?
nlevels(PlantGrowth$group)

# What are the groups?
levels(PlantGrowth$group)

# Mean and SD - global
mean(PlantGrowth$weight)

# Mean and SD - group-wise
# use tidyverse functions
# the pip operator, %>%, say "take ... and then..."
# type ctrl+shift+m
PlantGrowth %>% 
  group_by(group) %>% 
  summarise(avg = mean(weight),
            stdev = sd(weight)) -> PG_summary

# Transformations - Z-scores
# use scale() 
PlantGrowth %>% 
  group_by(group) %>% 
  mutate(Z = scale(weight))

# Plot data
# Raw data:
# 3 essential components
# 1 - data (PlantGrowth)
# 2 - aesthetic mappings (which variable to which axis?)
# 3 - geometry (how does it look?)
# Individual points
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_jitter(width = 0.15, alpha = 0.6)

# Boxplots
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot()

# Mean and sd
ggplot(PG_summary, aes(x = group, y = avg, 
                       ymin = avg + stdev, 
                       ymax = avg - stdev)) +
  geom_pointrange()

# both raw and summary:
ggplot(PlantGrowth, aes(group, weight)) +
  geom_jitter(width = 0.15, col = "blue", alpha = 0.6) +
  geom_pointrange(data = PG_summary, 
                  aes(x = group, y = avg, 
                      ymin = avg + stdev, 
                      ymax = avg - stdev), 
                  color = "red")

# Inferential statistics
# define a linear model:
PG_lm <- lm(weight ~ group, PlantGrowth)
PG_lm

# t-test
# typically, use t.test(...) but here... 
summary(PG_lm)

# ANOVA
anova(PG_lm)

# Element 2: Functions
# Everything that happens, 
# is because of a function
34 + 6
`+`(34, 6)

# order of operations
# BEDMAS - brackets, exp, div, mult, add, sub
2 - 3/4 # 1.25 
(2 - 3)/4 # 0.25

# make some simple objects
n <- 34 # Assign the value of 34 to n
p <- 6 # Assign the value of 6 to p

# Use like numbers
n + p

# Generic form of functions:
# fun_name(arg_name = ...)

# Call args using name or position
log2(8) # positional matching
log2(x = 8) # naming
log(x = 8, base = 2) # long, naming
log(8, 2) # long, position
log(8, base = 2) # mixture, very typical!

# What if:
log(2, x = 8) # Confusing!

# be cautious of partial naming!
log(8, b = 2)

b <- 2
log(8, b)

# Functions can have 0 to many args
ls()
getwd()

# Arguments may be named or not
# e.g. "combine" - c()
xx <- c(3, 8, 9, 23)
xx # four "elements"

myNames <- c("healthy", "tissue", "quantity")
myNames

# seq is another typical function:
# regular sequence
# seq(from = 1, to = 100, by = 7)
foo1 <- seq(1, 100, 7)

# use objects in functions:
foo2 <- seq(1, n, p)

# regular sequence with interval 1:
# the : operator
1:10
seq(1, 10, 1)

# Two major types of math functions:
# 1 - Aggregration functions
# 1 (or a small number of) output
# mean, sd, median, IQR, sum, prod

# 2 - Transformation functions
# as many output as input
# z-score, normalization, log, sqrt, +, -, /

# ex 6.1
foo2 + 100 # trans
foo2 + foo2 # trans
sum(foo2) + foo2 # agg, then trans
1:3 + foo2 # trans, Recycles

########################### IMPORTANT
########################### Vector Recycling

1:4 + foo2

# 3 kinds of messages:
# information - neutral
# warning - potential problem
# error - full stop

# Calculate a linear transformation on xx
# y = mx+b
m <- 1.12
b <- -0.4

y <- m * xx + b
y

# What about two transformations (i.e. two slopes)?
m_2 <- c(5, 1.12)
m_2 * xx + b

# Manually calculate the z-score of foo1
foo1
(foo1 - mean(foo1)) / sd(foo1)
scale(foo1)

# Element 3: Objects
# Anything that exists is an object

# Vectors - 1 dimensional, homogenous
foo1 # 15 "elements"
myNames # 3 elements
m # 1-element long

# 4 most common "user-defined" 
# atomic vector types
# logical - TRUE/FALSE, T/F, 1/0 (aka boolean, binary)
# integers - whole numbers
# double - real numbers (3.14, 6.8, 2.718) (aka float)
# character - anything (aka string)

# numeric - either int or dbl

foo3 <- c("Liver", "Brain", "Testes", "Muscle",
          "Intestine", "Heart")
foo4 <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE)

typeof(foo2)
typeof(foo3)
typeof(foo4)

test <- c(1:10, "bob")
typeof(test)
mean(test)

# test_2: Type coercion
test_2 <- as.numeric(test)
typeof(test_2)
mean(test_2, na.rm = TRUE)

# test_3: remove bob, then coerce
test_3 <- test[-11]
test_3 <- as.numeric(test_3)
mean(test_3)

# test_4: remove NAs
test_4 <- as.numeric(test)
test_4 <- na.omit(test_4)
mean(test_4)

# Lists: 1 dimensional, heterogenous
# e.g.
typeof(PG_lm)

# each element may be named or not:
names(PG_lm) # This is an "attribute" (like meta-data)
length(PG_lm) # 13

# Anything that is names can be accessed
# with $
PG_lm$coefficients # 3-element named vector
PG_lm$residuals # 30-element named vector
PG_lm$model # a data frame
PG_lm$model$weight

# access attributes:
attributes(PG_lm)

# The most common attributes can be 
# accessed with "accessor" functions
# e.g. names()
class(PG_lm)

# classes tell R what to do with this object
# e.g. summary() on an lm object gives t-tests 
# that's not the case here:
summary(PlantGrowth)
# what's the class of PlantGrowth?
typeof(PlantGrowth)
class(PlantGrowth) #data frame

# Data frame: 2 dimensional, heterogenous
# A special class of type list
# Each element must be a vector of the same length!
foo_df <- data.frame(foo4, foo3, foo2)
foo_df

attributes(foo_df)
names(foo_df) <- myNames

foo_df$quantity

# rows == observations
# columns == variables

# explore:
str(foo_df) # "structure"
glimpse(foo_df) # from dplyr, in tidyverse
summary(foo_df)
dim(foo_df) # <row, col>
nrow(foo_df)
ncol(foo_df)

# Caution:
length(foo_df) # the number of elements (i.e. columns)
length(foo2) # 6 elements in foo2


# Element 4: Logical Expressions
# Asking and combining Yes/No questions

# Relational operators - Asking questions
# == equivalency
# != non-equivalency
# >, <, >=, <=
# !x, the negation of x, where x is a logical vector

foo4
!foo4
n > p
n < p

# The output will ALWAYS be a logical vector!

# Logical operators - Combining questions
# & AND - ALL TRUE
# | OR - at least ONE TRUE
# %in% WITHIN - shortcut for many == with |

# Examles
# Logical type
# All healthy samples
foo_df %>% 
  filter(healthy)

# All unhealthy samples
foo_df %>% 
  filter(!healthy)

# Numeric type (int or dbl)
# below 10
foo_df %>% 
  filter(quantity < 10)

# between 10 and 20 (middle)
foo_df %>% 
  filter(quantity > 10 & quantity < 20)
foo_df %>% 
  filter(quantity > 10, quantity < 20)

# Meaningless
foo_df %>% 
  filter(quantity > 10 | quantity < 20)

# beyond 10 and 20 (tails)
foo_df %>% 
  filter(quantity < 10 | quantity > 20)

# Impossible
foo_df %>% 
  filter(quantity < 10 & quantity > 20)

# What we really did was:
foo_df$quantity < 10
foo_df$quantity > 20

# Character type
# NO PATTERN MATCHING
# Only heart samples
foo_df %>% 
  filter(tissue == "Heart")

# Heart and liver samples
# basic:
foo_df %>% 
  filter(tissue == "Heart" | tissue == "Liver")

# efficient:
foo_df %>% 
  filter(tissue %in% c("Heart", "Liver"))
foo_df %>% 
  filter(tissue %in% c("Liver", "Heart"))

# terrible -- NEVER do this!
foo_df %>% 
  filter(tissue == c("Heart", "Liver"))
foo_df %>% 
  filter(tissue == c("Liver", "Heart"))

# Element 5: Indexing
# Finding info according to position using []

# Vectors (1D)
foo1[6] # The 6th element
foo1[p] # The pth element
foo1[3:p] # The 3rd to the pth element
foo1[p:length(foo1)] # the pth to the last element 

# Use a combination of:
# Integers, objects and functions

# This is nice, but....
# The exciting part is... LOGICAL VECTORS
# i.e. a result of a logical expression (see above)
foo1[foo1 < 50] # All values less than 50

# Data frames (2D)
# [ rows , cols ]
foo_df[3,] # 3rd row, ALL columns
foo_df[,3] # ALL rows, 3rd column - Vector
foo_df[3] # ALL rows, 3rd column - Dataframe

# The 3rd to the pth row, only quantity column
foo_df[3:p, 3] # by position
foo_df[3:p, "quantity"] # by name
# the same, excluding quantity
foo_df[3:p, -3] # exclude by position

# This is exactly what filter does!
# e.g. the tissues with low quantity (below 10)
foo_df[foo_df$quantity < 10 , "tissue"]

foo_df %>% 
  filter(foo_df$quantity < 10) %>% 
  select(tissue)

# arranging data:
foo_df %>% 
  arrange(desc(quantity)) %>% 
  .[1:2,]

foo_df %>% 
  arrange(desc(quantity)) %>% 
  head(2)

foo_df %>% 
  arrange(desc(quantity)) %>% 
  slice(1:2)

foo_df %>% 
  top_n(2, quantity)

# Element 8: Factor variables with many "levels"
# aka categorical (discrete, qualitative) variables 
# with many "groups"
# i.e. includes nominal, ordinal, binary
# count data

# e.g.
PlantGrowth$group
foo_df$tissue

# what is it:
typeof(PlantGrowth$group) # integer
class(PlantGrowth$group) # factor
# factor is a special class of type integer

# We can see this with str()
str(PlantGrowth)

# R likes to make factors out of characters
# e.g.
foo3 # chr vector
# also, see read.delim when importing

# Get the levels
levels(PlantGrowth$group)

# Common problem - numeric factors
xx <- c(23:27, "bob")
xx

# in a dataframe, this would be a factor:
myDF <- data.frame(xx)
myDF$xx
mean(myDF$xx) # doesn't work
# so just coerce, but first make it a character!
as.numeric(as.character(myDF$xx))

# Element 9: Tidy Data
# Create a new play dataset to work on:
PlayData <- data.frame(type = rep(c("A", "B"), each = 2),
                       time = 1:2,
                       height = seq(10, 40, 10),
                       width = seq(50, 80, 10))

# Use the tidyr package to get tidy data:
# four arguments for gather():
# 1 - the messy data set
# 2,3 - key/value pair - Names of the OUTPUT columns
# 4 - the ID (type time) or the MEASURE (height width) variables

# using ID variables
gather(PlayData, key, value, -c(type, time))

# using MEASURE variables
PlayData_t <- gather(PlayData, key, value, c(height, width))


# apply transformation functions, i.e. Ratio
# ratio height/width
PlayData$height/PlayData$width

# ratio time1/time2
PlayData_t %>% 
  spread(time, value) %>% 
  mutate(timeratio = `1`/`2`)

# ratio typeA/typeB
PlayData_t %>% 
  spread(type, value) %>% 
  mutate(typeratio = A/B)

# Element 10: Split-Apply-Combine with dplyr
# Grammar of data analysis

# Punctuation:The pipe operator %>%
# The five verbs of dplyr:
# filter(),
# arrange(),
# select(),
# mutate(),
# summarise()
# The adverb group_by()

# In the context of PlayData
# mean across height & width, i.e. group by type and time
PlayData_t %>% 
  group_by(type, time) %>% 
  summarise(avg = mean(value))

# mean across time 1 & time 2, i.e. group by type and key
PlayData_t %>% 
  group_by(type, key) %>% 
  summarise(avg = mean(value))

# mean across type A & type B, i.e. group by time and key
PlayData_t %>% 
  group_by(time, key) %>% 
  summarise(avg = mean(value))