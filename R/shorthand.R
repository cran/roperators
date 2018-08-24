# These are just space savers

#' Cleaner conversion functions
#' @examples
#' @param x Value to be converted
#' @param ... other args for as. conversion
#'
#' # Shorthand aliases for common conversions
#' # Nothing magical here, but it can make your code more readable
#'
#' chr(42)   # "42" = as.character
#' int(42.1) # 42L  = as.integer
#' dbl("42L") # 42.0 = as.double
#' num("42") # 42   = as.numeric
#' bool(42)  # TRUE = as.logical
#'
#' @export
#' @rdname cleaner_conversions
chr  <- function(x, ...) as.character(x, ...)

#' @export
#' @rdname cleaner_conversions
int  <- function(x, ...)  as.integer(x, ...)

#' @export
#' @rdname cleaner_conversions
dbl <- function(x, ...) as.double(x, ...)

#' @export
#' @rdname cleaner_conversions
num <- function(x, ...) as.numeric(x, ...)

#' @export
#' @rdname cleaner_conversions
bool  <- function(x, ...) as.logical(x, ...)



#### Conversion shorthands
#' Convert factor with numeric labels into numeric vector
#' @param x a factor with numeric labels
#' @examples
#' # Apply a regular expression/substitution to x:
#'
#'  x <- factor(c(11, 22, 33, 99))
#'  as.numeric(x)
#'  # 1 2 3 4   # NOT typically the desired.expected output
#'
#'  f.as.numeric(x)
#'  # 11 22 33 99  # Typically desired output
#'
#'  # Or...
#'  as.numeric(as.character(x)) # A tad unsightly
#'
#'
#' @export
#' @rdname factor_conversion
f.as.numeric <- function(x){
  as.numeric(as.character(x)) # for factors
}
