#' Convert a list of tokens to ngrams
#'
#' This code takes a list of text vectors, and returns a list of text vectors, including n-grams
#'
#' @param x A character vector
#' @param split The token to split on
#' @param regex whether to treat the split token as a regular expression.
#' @return a list of character vectors
#' @export
#' @importFrom stringi stri_split_fixed stri_split_regex
find_tokens <- function(x, split=' ', regex=FALSE){
  if(regex){
    return(stri_split_fixed(x, split))
  } else {
    return(stri_split_regex(x, split))
  }
}
