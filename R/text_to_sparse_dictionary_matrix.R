#' Tokenize a vector of text and convert to a sparse matrix based on the presense or absense of a dictionary of substrings.
#'
#' This code takes a vector of text, uses grepl to detect each of a set of substrings within the text, and builts a sparse matrix
#'
#' @param x a character vector
#' @param dictionary the dictionary to lookup in the character vector
#' @param verbose whether to print a log while performing the operations.
#' @param fixed whether to treat the dictionary as-is (TRUE) or as regular expressions (FALSE).  TRUE is usually faster
#' @param freqCutoff columns below this pct frequency will be removed from the final object
#' @param absCutoff columns below this absolute frequency will be removed from the final object
#' @export
#' @importFrom pbapply pblapply
#' @importFrom SnowballC wordStem
#' @importFrom Matrix sparseMatrix colSums
#' @importFrom foreach foreach %dopar%
#' @return a sparse_dict_text_matrix object
#' @references
#' \url{http://stackoverflow.com/questions/22489996/r-grepl-quickly-match-multiple-strings-against-multiple-substrings-returning-a}
#' @examples
#' x <- c(
#'   'i like this package written by zach mayer',
#'   'this package is so much fun',
#'   'thanks zach for writing it',
#'   'this package is the best package',
#'   'i want to give zach mayer a million dollars')
#' text_to_sparse_dictionary_matrix(x, dictionary=c('zac', 'may', 'writ'))
text_to_sparse_dictionary_matrix <- function(x, dictionary, verbose=FALSE, fixed=TRUE, freqCutoff=0, absCutoff=0){
  stopifnot(!any(duplicated(dictionary)))

  #Find matches. This is the slow part
  matches <- foreach(i=1:length(dictionary), .verbose=verbose) %dopar% {
    which(grepl(dictionary[i], x, fixed=TRUE))
  }

  #Make sparse matrix
  n.ids <- sapply(matches, length)
  j <- rep(seq_along(n.ids), n.ids)
  i <- unlist(matches)

  #Omit rows with no entries (these are novel queries)
  j <- j[!is.na(j)]
  i <- i[!is.na(j)]

  #Make a sparse matrix
  M <- sparseMatrix(i=i, j=j, x=rep(1, length(i)), dims=c(length(x), length(dictionary)))
  colnames(M) <- make.names(dictionary)

  #Remove extremely low-frequency words
  if(freqCutoff>0){
    if(verbose) print('Removing words with a low relative frequency')
    stopifnot(freqCutoff<1)
    keep <- colSums(sign(M))/nrow(M)>freqCutoff
    M <- M[,keep,drop=FALSE]
    dictionary <- dictionary[keep]
  }

  if(absCutoff>0){
    stopifnot(nrow(M)>absCutoff)
    if(verbose) print('Removing low frequency words')
    keep <- colSums(sign(M))>absCutoff
    M <- M[,keep,drop=FALSE]
    dictionary <- dictionary[keep]
  }

  #Return
  out <- list(M=M, dictionary=dictionary, fixed=fixed)
  class(out) <- 'sparse_dict_text_matrix'
  return(out)
}

#' Project a new character vector of data into the featurespace defined by a previous model's dictionary.  THIS DOCUMENTATION COULD USE SOME WORK!
#'
#' This code takes a vector of text, and applies a pre-defined text to sparse matrix mapping to it.
#'
#' @param object an object of class sparse_dict_text_matrix
#' @param newdata a character vector of new data to make into a sparse matrix by the model$dictionary
#' @param ... passed to text_to_sparse_dictionary_matrix
#' @export
#' @return a sparse_dict_text_matrix object
#' @references
#' \url{http://stackoverflow.com/questions/22489996/r-grepl-quickly-match-multiple-strings-against-multiple-substrings-returning-a}
#' @examples
#' x <- c(
#'   'i like this package written by zach mayer',
#'   'this package is so much fun',
#'   'thanks zach for writing it',
#'   'this package is the best package',
#'   'i want to give zach mayer a million dollars')
#' y <- 'this is a new sentence about how much i like this package written
#' by zach mayer it is a cool package and he is a cool dude'
#' a <- text_to_sparse_dictionary_matrix(x, dictionary=c('zac', 'may', 'writ'))
#' predict(a, y)
predict.sparse_dict_text_matrix <- function(object, newdata=NULL, ...){
  if(is.null(newdata)) return(object)
  text_to_sparse_dictionary_matrix(newdata, dictionary=object$dictionary, fixed=object$fixed, freqCutoff=0, absCutoff=0, ...)
}
