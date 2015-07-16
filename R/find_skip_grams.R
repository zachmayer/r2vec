#' Convert a list of tokens to skip-grams
#'
#' This code takes a list of text vectors, and returns a list of skip-grams
#'
#' @param dat a list of character vectors
#' @param n the number of skip frams
#' @param verbose whether to print a progress bar
#' @return a list of character vectors
#' @export
#' @importFrom pbapply pblapply
#' @references
#' \url{http://stackoverflow.com/questions/16489748/converting-a-list-of-tokens-to-n-grams}
#' @examples
#' find_skip_grams(
#'   list(
#'     c('one'), c('sent', 'one'),
#'     c('this', 'is', 'sentence', 'two'),
#'     c('finally', 'we', 'have', 'a', 'third', 'longer', 'sentence'),
#'     character(0),
#'     NULL,
#'     NA,
#'     NaN
#'   ),
#'   n=3,
#'   verbose=TRUE
#' )
find_skip_grams <- function(dat, n, verbose = FALSE){
  stopifnot(is.list(dat))
  stopifnot(is.numeric(n))
  stopifnot(n > 0)
  n <- n + 1
  APPLYFUN <- lapply
  if (verbose) {
    APPLYFUN <- pblapply
  }
  APPLYFUN(dat, function(y) {
    if (length(y) <= 1)
      return(NULL)
    unlist(lapply(2:n, function(n_i) {
      n_i <-  n_i + 1
      if (n_i > length(y)) return(NULL)
      o <- embed(y, n_i)
      paste(o[,ncol(o)], o[,1])
    }))
  })
}
