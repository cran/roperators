## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  echo     = TRUE,
  collapse = TRUE,
  comment  = "#>"
)
library(roperators)

## -----------------------------------------------------------------------------
"foo" %+% "bar"               # string addition
(0.1 + 0.1 + 0.1) %~=% 0.3    # floating-point equality that just works
c(1, NA) %==% c(1, NA)        # NA == NA is treated as TRUE here

name <- "you"
f("hello {name}, 2 + 2 = {2 + 2}")   # f-strings!

## -----------------------------------------------------------------------------
my_string <- "using infix (%) operators " %+% "lets R do string addition"
my_string

# subtraction removes a pattern
my_string %-% "lets R do string addition"

# multiplication repeats (%*% was already taken, so it's %s*%)
"ha" %s*% 3

## -----------------------------------------------------------------------------
"an apple a day keeps the malignant spirit of Steve Jobs at bay" %s/% "a"

# with a regular expression
"an apple a day keeps the malignant spirit of Steve Jobs at bay" %s/% "Steve Jobs|apple"

## -----------------------------------------------------------------------------
x <- 1
x %+=% 2
x

d <- iris
# add 1 to setosa sepal lengths, in place
d$Sepal.Length[d$Species == "setosa"] %+=% 1

## -----------------------------------------------------------------------------
x <- "ab"
x %+=% "c"
x

## -----------------------------------------------------------------------------
x <- c(NA, 1, 2, 3)
x %na<-% 0
x

x <- c("a1b", "b1", "c", "d0")
x %regex=% c("\\d+", "#")   # replace just the matched part
x

## -----------------------------------------------------------------------------
a <- c(NA, "foo", "foo", NA)
b <- c(NA, "foo", "bar", "bar")

a == b      # base R: the NA leaks through
a %==% b    # roperators: NA == NA is treated as TRUE

## -----------------------------------------------------------------------------
(0.1 + 0.1 + 0.1) == 0.3    # FALSE (!)
(0.1 + 0.1 + 0.1) %~=% 0.3  # TRUE

# greater/less-than-or-approximately-equal
(0.1 + 0.1 + 0.1) %>~% 0.3
(0.1 + 0.1 + 0.1) %<~% 0.3

## -----------------------------------------------------------------------------
5 %><% c(1, 10)    # strictly between
1 %>=<% c(1, 10)   # inclusive
5 %><% c(10, 1)    # reversed bounds are fine too — no need to worry about order

# %===% is strict value-AND-class equality, like JavaScript's ===
x <- int(2)
x == 2       # TRUE
x %===% 2    # FALSE (different class)
x %===% int(2)

## -----------------------------------------------------------------------------
"z" %ni% c("a", "b", "c")    # not in
TRUE %xor% FALSE             # exclusive or
TRUE %aon% TRUE              # all-or-nothing: both TRUE, or both FALSE

# SQL-style LIKE
c("FOO", "bar", "fizz") %rlike% "foo"   # case-insensitive
c("dOe", "doe")         %perl% "[a-z]O" # case-sensitive, Perl regex

## -----------------------------------------------------------------------------
who <- "Ben"; n <- 2
f("Hi {who}, you have {n} new message{if (n != 1) 's'}")
f("today's first letters: {head(LETTERS, n)}")   # vectors are tidied up for you

## -----------------------------------------------------------------------------
sqrt("not a number") %else% NA_real_
(1:3)[[99]]          %else% "out of range"

## -----------------------------------------------------------------------------
c(10, 20, 30) %/0% c(2, 0, 5)

## -----------------------------------------------------------------------------
5 %+-% 0.5
4.9 %><% (5 %+-% 0.5)

## -----------------------------------------------------------------------------
"  Yes " %~% "yes"
c("Apple", "PEAR") %~% c("apple", "pear")

## -----------------------------------------------------------------------------
as.percent(c(0.1, 0.005, 2 / 3))
as.percent(2 / 3, digits = 0)

## -----------------------------------------------------------------------------
chr(42)      # as.character()
int(42.9)    # as.integer()
num("4.2")   # as.numeric()
bool("TRUE") # as.logical()

# the famous factor-to-number stumble, smoothed over:
fac <- factor(c(11, 22, 33))
as.numeric(fac)     # 1 2 3  -- almost never what you wanted
f.as.numeric(fac)   # 11 22 33

# and convert to a class chosen at run time
as.class(255, "roman")

## -----------------------------------------------------------------------------
# would any of these break a calculation?
is.bad_for_calcs(c(1, NA, Inf, NaN, 5))

is.scalar(1)
is.constant(c(1, 1, 1))
is.binary(c("a", "b", "a"))

## -----------------------------------------------------------------------------
# pulling pieces out of vectors and strings
get_1st_word("Ada Lovelace")
get_last_word("Ada Lovelace")
get_most_frequent(c("a", "b", "b", "c", "b"))

# Oxford-comma joining, done for you
paste_oxford("Tom", "Dick", "Harry")

# complete-cases stats: just add _cc for na.rm = TRUE
mean_cc(c(1, 2, NA))
sd_cc(c(1, 2, 3, NA))

# little environment checks
get_os()
get_R_version()

# and file-extension checks
is_csv_file(c("a.csv", "b.txt"))

