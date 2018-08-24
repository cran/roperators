#' @include type_checks.R
NULL


#### String operators   -----------------
#' String operators
#'
#' Perform string concatenation and arithmetic is a similar way to other languages
#'
#' @param x a string
#' @param y a string
#' @examples
#' ("ab" %+% "c") == "abc" # TRUE
#' ("abc" %-% "b") == "ac" # TRUE
#' ("ac" %s*% 2) == "acac" # TRUE
#' ("acac" %s/% "c") == 2  # TRUE
#'
#' @rdname string_arithmetic


#' @rdname string_arithmetic
#' @export
`%+%`   <- function(x, y) {
  paste0(x, y)
}

#' @rdname string_arithmetic
#' @export
`%-%`   <- function(x, y) {
  gsub(pattern = y, replacement = '', x = x)
}

#' @rdname string_arithmetic
#' @export
`%s*%`  <- function(x, y) {
  sapply(x, function(x)  paste0(rep(x, y), collapse = ''))
}

#' @rdname string_arithmetic
#' @export
`%s/%`  <- function(x, y) {
  lengths(regmatches(x, gregexpr(y, x)))
}


# Not using for now - was messing up env
#' @param operator function to apply
#' @param lhs variable name on left hand side
#' @param rhs values on right hand side
.create_operator <- function(lhs, rhs, operator){
  v_name  <- substitute(lhs, env = parent.frame())
  v_value <- operator(lhs, rhs)
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 2))
}


#### For assignment operators  ------------------

#### Apply mathematical operator and reassignment
#' @rdname assign_ops
#' @export
`%root=%`<- function(x, y) {
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  v_value <- x^(1/y)
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}

#' Assignment operators
#'
#' Modifies the stored value of the left-hand-side object by the right-hand-side object.
#'     Equivalent of operators such as += -= *= /= in languages like c++ or python.
#' %+=% and %-=% can also work with strings.
#'
#' @param x a stored value
#' @param y value to modify stored value by
#' @examples
#' x <- 1
#'
#' x %+=% 2
#'
#' x == 3 # TRUE
#'
#' x %-=% 3
#'
#' x == 0 # TRUE
#'
#' # Or with data frames...
#' test <- iris
#'
#' # Simply modify in-place
#' test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] %+=% 1
#'
#' # Which is much nicer than typing:
#' test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] <-
#' test$Sepal.Length[test$Species == 'setosa' & test$Petal.Length < 1.5] + 1
#' # ...which is over the 100 character limit for R doccumentation!
#'
#' # %+=% and %-=% also work with strings
#'
#'    x <- "ab"
#'
#'    x %+=% "c"
#'
#'    x %-=% "b"
#'
#'    x == "ac" # TRUE
#'
#' # %-=% can also take regular expressions
#'
#'    x <- "foobar"
#'
#'    x %-=% "[f|b]"
#'
#'    print(x)
#'    # "ooar"
#' @rdname assign_ops

#' @rdname assign_ops
#' @export
`%+=%`   <- function(x, y){
  if(is.character(x)){
    #.create_operator(x, y, `%+%`)
    v_name  <- substitute(x) #with user defined functions, can't run in parent frame
    v_value <- paste0(x,y)
    eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
    }
  else{
    #.create_operator(x, y, `+`)
    v_name  <- substitute(x) #with user defined functions, can't run in parent frame
    v_value <- x+y
    eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
    }
}

#' @rdname assign_ops
#' @export
`%-=%`  <- function(x, y){
  if(is.character(x)){
    #.create_operator(x, y, `%-%`)
    v_name  <- substitute(x) #with user defined functions, can't run in parent frame
    v_value <- gsub(y, '', x)
    eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
    } else{
      #.create_operator(x, y, `-`)
      v_name  <- substitute(x) #with user defined functions, can't run in parent frame
      v_value <- x-y
      eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
    }
}

#' @rdname assign_ops
#' @export
`%*=%`   <- function(x, y) {
  #.create_operator(x, y, `*`)
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  v_value <- x*y
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}

#' @rdname assign_ops
#' @export
`%/=%`   <- function(x, y) {
  #.create_operator(x, y, `/`)
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  v_value <- x/y
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}

#' @rdname assign_ops
#' @export
`%^=%`   <- function(x, y) {
  #.create_operator(x, y, `^`)
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  v_value <- x^y
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}

#' @rdname assign_ops
#' @export
`%log=%` <- function(x, y){
  #.create_operator(x, y, log)
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  v_value <- log(x, y)
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}


#' Modify or overwrite existing object by regular expression
#' @param x a character vector
#' @param value c(pattern, replacement)
#' @examples
#' # Apply a regular expression/substitution to x:
#'
#'  x <- c("a1b", "b1", "c", "d0")
#'
#'  # change any number to "x"
#'
#'   x %regex=% c("\\d+", "x")
#'
#'  print(x)
#'
#'  # "axb" "b" "c" "dx"
#' @rdname overwrite_by_regex
#' @export
`%regex=%`<- function(x, value){
  if(length(value) != 2) warning("right-hand-side isn't length 2 but it MUST be in the form c(pattern, replacement)")
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  v_value <- gsub(value[1], value[2], x)
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}


####### CONDITIONAL ASSIGNMENT    ----------------------------------------------


#' Assign to vector only where regular expression is matched
#' @param x a character vector
#' @param value c(pattern, replacement)
#' @examples
#' # Overwrite elements that match regex:
#'
#'  x <- c("a1b", "b1", "c", "d0")
#'
#'  # overwrite any element containing a number
#'
#'  x %regex<-% c("\\d+", "x")
#'
#'  print(x)
#'
#'  # "x" "b" "c" "x"
#' @rdname assign_by_regex
#' @export
`%regex<-%`<- function(x, value){
  if(length(value) != 2) warning("right-hand-side isn't length 2 but it MUST be in the form c(pattern, replacement)")
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  idx <- grep(value[1], x)
  v_value <- x
  v_value[idx] <- value[2]
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}




#' Assign value to a vector's missing values
#' @param x a vector
#' @param value value to replace vector's missing values with
#' @examples
#'  x <- c("a", NA, "c")
#'
#'  x %na<-% "b"
#'
#'  print(x)
#'  # "a" "b" "c"
#'
#'  x <- c(1, NA, 3, NA)
#'
#'  x %na<-% c(2,4)
#'
#'  print(x)
#'  # 1 2 3 4
#'
#' @rdname overwrite_missing
#' @export
`%na<-%` <- function(x, value) {
  v_name  <- substitute(x) #with user defined functions, can't run in parent frame
  x[is.na(x)] <- value
  v_value <- x
  eval(call("<-", v_name, v_value), envir = parent.frame(n = 1))
}


#### Logical operators (because why not)  --------------------------

# equality with missings
#' Test of equality including equality of missing values
#' @param x a vector
#' @param y a vector
#' @examples
#'  c(1, NA, 3, 4)  ==  c(1, NA, 4, 3)
#'  #  TRUE    NA  FALSE FALSE
#'
#'  c(1, NA, 3, 4) %==% c(1, NA, 4, 3)
#'  #  TRUE  TRUE  FALSE FALSE
#'
#'  c(1, NA, 3, 4) %>=% c(1, NA, 4, 3)
#'  #  TRUE  TRUE FALSE  TRUE
#'
#'  c(1, NA, 3, 4) %<=% c(1, NA, 4, 3)
#'  #  TRUE  TRUE TRUE  FALSE
#' @rdname na_equal
#' @export
`%==%` <- function(x, y){
  bad_x <- is.bad_for_calcs(x)
  bad_y <- is.bad_for_calcs(y)

  ((x == y) & !bad_x & !bad_y) | is.bad_and_equal(x, y)
}

# >= with missings as false
#' @rdname na_equal
#' @export
`%>=%` <- function(x, y){
  bad_x <- is.bad_for_calcs(x)
  bad_y <- is.bad_for_calcs(y)
  ((x >= y) & !bad_x & !bad_y) | is.bad_and_equal(x, y)
}

# <= with missings as false
#' @rdname na_equal
#' @export
`%<=%` <- function(x, y){
  bad_x <- is.bad_for_calcs(x)
  bad_y <- is.bad_for_calcs(y)
  ((x <= y) & !bad_x & !bad_y) | is.bad_and_equal(x, y)
}


# Logicals
#' Logical operators
#' @param x a vector
#' @param y a vector
#' @examples
#'
#' #### Floating point test of equality ####
#'  (0.1 + 0.1 + 0.1) == 0.3   # FALSE
#'
#'  (0.1 + 0.1 + 0.1) %~=% 0.3 # TRUE
#'
#'  # NOTE parentheses surrounding expression before this operator are necessary
#'  # Without parentheses it would be interpreted as .1 + .1 + (.1 %~=% .3)
#'
#'
#'  #### Not in ####
#'
#'  "z" %ni%  c("a", "b", "c")
#'  #  TRUE
#'
#'  #### Exclusive or  ####
#'
#'  TRUE %xor% TRUE
#'  # FALSE
#'
#'  FALSE %xor% FALSE
#'  # FALSE
#'
#'  FALSE %xor% TRUE
#'  # TRUE
#'
#'  #### All-or-nothing ####
#'
#'  TRUE %aon% TRUE
#'  # TRUE
#'
#'  FALSE %aon% FALSE
#'  # TRUE
#'
#'  FALSE %aon% TRUE
#'  # FALSE
#'
#'  #### Between ####
#'
#'  # ends excluded
#'
#'  2 %><% c(1, 3)
#'  # TRUE
#'
#'  3 %><% c(1, 3)
#'  # FALSE
#'
#'  # ends included
#'
#'  2 %>=<% c(1, 3)
#'  # TRUE
#'
#'  3 %>=<% c(1, 3)
#'  # TRUE
#'
#' @rdname logicals
#' @export


#' Floating point equality
#' @rdname logicals
#' @export
`%~=%` <- function(x, y) {
  isTRUE(all.equal(x, y))
}


#' Not in
#' @rdname logicals
#' @export
`%ni%` <- function(x, y) {
  !(x %in% y)
}

#' exclusive or
#' @rdname logicals
#' @export
`%xor%`<- function(x, y){
  xor(x, y)
}

#' All or nothing
#' @rdname logicals
#' @export
`%aon%`<- function(x, y){
  (x && y) || (!x && !y)
}

#' between (ends excluded)
#' @rdname logicals
#' @export
`%><%` <- function(x, y){
  if(length(y) != 2) warning("right-hand-side isn't length 2 but it MUST be in the form c(lower_bound, upper_bound)")
  x > y[1] & x < y[2]
}

#' between (ends included)
#' @rdname logicals
#' @export
`%>=<%`<- function(x, y){
  if(length(y) != 2) warning("right-hand-side isn't length 2 but it MUST be in the form c(lower_bound, upper_bound)")
  x >= y[1] & x <= y[2]
}

