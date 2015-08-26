#' Convert a list of tokens to ngrams
#'
#' This code takes a list of text vectors, and returns a list of text vectors, including n-grams
#'
#' @param dat a list of character vectors
#' @param n the number of n-grams to compute
#' @param verbose whether to print a progress bar
#' @return a list of character vectors
#' @export
#' @importFrom pbapply pblapply
#' @references
#' \url{http://stackoverflow.com/questions/16489748/converting-a-list-of-tokens-to-n-grams}
#' \url{https://github.com/markvanderloo/stringdist/issues/39}
#' \url{https://gist.github.com/markvanderloo/9ae6a15f7d74a0159aec}
#' @examples
#' find_ngrams(
#'   list(
#'     c('one'), c('sent', 'one'),
#'     c('this', 'is', 'sentence', 'two'),
#'     c('this', 'is', 'sentence', 'three', 'sentence', 'three'),
#'     c('finally', 'we', 'have', 'a', 'fourth', 'longer', 'sentence'),
#'     character(0),
#'     NULL,
#'     NA,
#'     NaN
#'   ),
#'   n=3,
#'   verbose=TRUE
#' )
find_ngrams <- function(dat, n, verbose=FALSE){
  stopifnot(is.list(dat))
  stopifnot(is.numeric(n))
  stopifnot(n>0)
  if(n == 1) return(dat)

  APPLYFUN <- lapply
  if(verbose) APPLYFUN <- pblapply

  APPLYFUN(dat, function(y) {
    if(length(y)<=1) return(y)
    c(y, unlist(lapply(2:n, function(n_i) {
      if(n_i > length(y)) return(NULL)
      do.call(paste, unname(rev(data.frame(embed(y, n_i), stringsAsFactors=FALSE))), quote=FALSE)
    })))
  })


}
