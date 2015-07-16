
#' Tokenize a vector of text and convert to a sparse matrix. TODO: tf-idf is BROKEN!!
#'
#' This code takes a vector of text, cleans it up, tokenizes it, spellchecks it,
#' removes stopwords, stems it, finds n-grams, crates a bag og words and
#' converts it to a sparse matrix using a bag of words model.  Optinally
#' it also applies td-idf weighting to the matrix.  This function can be slow.
#' Note that freqCutoff and absCutoff are relative to the number of documents
#' the term appears in, and ignore its frequency within documents.
#'
#' @param x a character vector
#' @param split_token token to use to split the text data.  If NULL, text will not be tokenized and will be treated as dummy variables.
#' @param verbose whether to print a log while performing the operations
#' @param freqCutoff columns below this pct frequency will be removed from the final object
#' @param absCutoff columns below this absolute frequency will be removed from the final object
#' @param tfidf whether to apply tfidf weighting.  NOTE THAT THIS WILL CREATE A DENSE MATRIX, WHICH IN MANY CASES IS BAD.
#' @param bagofwords input bagofwords to use to construct the final matrix
#' @param remove_stopwords if TRUE, english stopwords will be removed from the tokens
#' @param spellcheck if TRUE tokens will be spellchecked before they are stemmed
#' @param stem if TRUE the tokens will be stemmed, after tokenizing and before creating a matrix
#' @param ngrams If great than 1, n-grams of this degree will be added to the word bag
#' @param skips If great than 0, skips of this degree will be added to the word bag
#' @param stops Optional list of stopwords, otherwise a default list will be used.
#' @export
#' @importFrom pbapply pblapply
#' @importFrom SnowballC wordStem
#' @importFrom Matrix sparseMatrix colSums
#' @return a sparse_text_matrix object
#' @references
#' \url{http://stackoverflow.com/questions/4942361/how-to-turn-a-list-of-lists-to-a-sparse-matrix-in-r-without-using-lapply}
#' \url{http://en.wikipedia.org/wiki/Tf-idf}
#' @examples
#' x <- c(
#'   'i like this package written by zach mayer',
#'   'this package is so much fun',
#'   'thanks zach for writing it',
#'   'this package is the best package',
#'   'i want to give zach mayer a million dollars')
#' text_to_sparse_matrix(
#'   x,
#'   absCutoff=1, ngrams=2, stem=TRUE, verbose=TRUE)
#' text_to_sparse_matrix(
#'   x,
#'   absCutoff=1, ngrams=2, skips=1, stem=TRUE, verbose=TRUE, tfidf=TRUE)
#' text_to_sparse_matrix(
#'   x,
#'   split_token=NULL, stem=TRUE, verbose=TRUE)
text_to_sparse_matrix <- function(x, split_token=' ', verbose=FALSE, freqCutoff=0, absCutoff=0, tfidf=FALSE, bagofwords=NULL, spellcheck=FALSE,
  remove_stopwords=FALSE, stem=FALSE, ngrams=1, skips=0,
  stops=NULL){

  stopifnot(is.character(x))

  #Tokenize
  if(verbose) print('Tokenizing')
  if(is.null(split_token)){
    stopifnot(ngrams==1)
    tokens <- as.list(x)
  } else{
    tokens <- strsplit(x, split_token)
  }

  #Spellcheck
  if(spellcheck){
    if(verbose) {print('Starting spell checking')}
    warning('Spellcheck not currently supported')
  }

  #Remove Stopwords
  if(is.null(stops)){
    stops <- c(
      "i", "me", "my", "myself", "we", "our", "ours", "ourselves",
      "you", "your", "yours", "yourself", "yourselves", "he", "him",
      "his", "himself", "she", "her", "hers", "herself", "it", "its",
      "itself", "they", "them", "their", "theirs", "themselves", "what",
      "which", "who", "whom", "this", "that", "these", "those", "am",
      "is", "are", "was", "were", "be", "been", "being", "have", "has",
      "had", "having", "do", "does", "did", "doing", "would", "should",
      "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're",
      "they're", "i've", "you've", "we've", "they've", "i'd", "you'd",
      "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll",
      "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't",
      "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't",
      "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot",
      "couldn't", "mustn't", "let's", "that's", "who's", "what's",
      "here's", "there's", "when's", "where's", "why's", "how's", "a",
      "an", "the", "and", "but", "if", "or", "because", "as", "until",
      "while", "of", "at", "by", "for", "with", "about", "against",
      "between", "into", "through", "during", "before", "after", "above",
      "below", "to", "from", "up", "down", "in", "out", "on", "off",
      "over", "under", "again", "further", "then", "once", "here",
      "there", "when", "where", "why", "how", "all", "any", "both",
      "each", "few", "more", "most", "other", "some", "such", "no",
      "nor", "not", "only", "own", "same", "so", "than", "too", "very"
    )
  }
  if(remove_stopwords){
    if(verbose) print('Removing Stopwords')
    if(verbose){
      tokens <- pblapply(tokens, function(x){x[!x  %in% stops]})
    } else{
      tokens <- lapply(tokens, function(x){x[!x  %in% stops]})
    }
  }

  #Stem the Tokens
  if(stem){
    if(verbose) print('Stemming')
    if(verbose){
      tokens <- pblapply(tokens, wordStem)
    } else {
      tokens <- lapply(tokens, wordStem)
    }
  }

  #Skip-grams
  if(skips >= 1){
    if(verbose) print('computing skips')
    skip_grams <- find_skip_grams(tokens, n=skips, verbose=verbose)
  }

  #N-grams
  if(ngrams > 1){
    if(verbose) print('computing ngrams')
    tokens <- find_ngrams(tokens, n=ngrams, verbose=verbose)
  }

  #Add skips to main tokenset (original + ngrams)
  if(skips >= 1){
    tokens <- mapply(c, tokens, skip_grams)
  }

  #Create word bag
  if(is.null(bagofwords)){
    bagofwords <- sort(unique(unlist(tokens)))
  }

  #Make row and column vectors
  if(verbose) print('Making Sparse Matrix')
  n.ids <- sapply(tokens, length)
  i <- rep(seq_along(n.ids), n.ids)
  j <- match(unlist(tokens), bagofwords)

  #Omit rows with no entries (these are novel queries)
  i <- i[!is.na(j)]
  j <- j[!is.na(j)]

  #Make a sparse matrix
  M <- sparseMatrix(i=i, j=j, x=rep(1, length(i)), dims=c(length(x), length(bagofwords)))
  colnames(M) <- make.names(bagofwords)

  #Remove extremely low-frequency words
  if(freqCutoff>0){
    if(verbose) print('Removing words with a low relative frequency')
    stopifnot(freqCutoff<1)
    keep <- colSums(sign(M))/nrow(M)>freqCutoff
    M <- M[,keep,drop=FALSE]
    bagofwords <- bagofwords[keep]
  }

  if(absCutoff>0){
    stopifnot(nrow(M)>absCutoff)
    if(verbose) print('Removing low frequency words')
    keep <- colSums(sign(M))>absCutoff
    M <- M[,keep,drop=FALSE]
    bagofwords <- bagofwords[keep]
  }

  #td-idf weighting
  #Our matrix already contains term frequencies for each document
  if(tfidf){
    if(verbose) print('Doing td-idf weighting')
    idf <- log(nrow(M)/colSums(sign(M)))
    tf_idf <- M * t(sapply(1:nrow(M), function(x) idf)) #TODO: IMPROVE THIS! NOT SPARSE!!!

    #Remove terms with all zero weights
    keep <- colSums(tf_idf) > 0
    tf_idf <- tf_idf[,keep,drop=FALSE]
    idf <- idf[keep]
    bagofwords <- bagofwords[keep]

    #Use this new matrix
    M <- tf_idf
  } else {
    idf <- NULL
  }

  out <- list(M=M, split_token=split_token, bagofwords=bagofwords, tfidf=tfidf, idf=idf, spellcheck=spellcheck, stem=stem, ngrams=ngrams, skips=skips)
  class(out) <- 'sparse_text_matrix'
  return(out)
}

#' Take a sparse text matrix and construct a matric with the same specification from a new dataset
#'
#' This code takes an existing sparse text matrix, and a character vector of new data.  It then tokenizes the new data, spellchecks it (todo),
#' applies n-grams, and uses the bag of words from the input matrix to contruct a new matrix with the exact same columns.  It then optionally
#' applies the input matrix's tf-idf weightings to the new matrix and returns a new sparse_text_matrix object. TODO: COMBINE THIS WITH THE MATRIX FUNCTION!!!
#'
#' @param object object of class sparse_text_matrix
#' @param newdata new data to apply the same matrix format to
#' @param verbose print diagnostic messages
#' @param ... ignored
#' @method predict sparse_text_matrix
#' @export
#' @importFrom pbapply pblapply
#' @importFrom SnowballC wordStem
#' @importFrom Matrix sparseMatrix
#' @return a sparse_text_matrix object
#' @references
#' \url{http://stackoverflow.com/questions/4942361/how-to-turn-a-list-of-lists-to-a-sparse-matrix-in-r-without-using-lapply}
#' \url{http://en.wikipedia.org/wiki/Tf-idf}
#' @examples
#' x <- c(
#'   'i like this package written by zach mayer',
#'   'this package is so much fun',
#'   'thanks zach for writing it',
#'   'this package is the best package',
#'   'i want to give zach mayer a million dollars')
#' y <- 'this is a new sentence about how much i like this package written
#' by zach mayer it is a cool package and he is a cool dude'
#' a <- text_to_sparse_matrix(
#'   x,
#'   absCutoff=1, ngrams=2, stem=TRUE, verbose=TRUE)
#' b <- text_to_sparse_matrix(
#'   x,
#'   absCutoff=1, ngrams=2, stem=TRUE, verbose=TRUE, tfidf=TRUE)
#' predict(a, y)
#' predict(b, y)
predict.sparse_text_matrix <- function(object, newdata=NULL, verbose=FALSE, ...){
  if(is.null(newdata)) return(object)

  #Checks
  if(is.null(object$M) & is.null(object$bagofwords)){
    stop('object must at least have the M slot or the bag_of_words slot filled.  (input matrix or input bag_of_words)')
  }
  if(is.null(object$split_token)){
    warning('assuming split_token is " " - a single space')
    object$split_token <- ' '
  }
  if(is.null(object$bagofwords)){
    warning('Using colnames(object$M) as the bag of words')
    object$bagofwords <- colnames(object$M)
  }
  if(is.null(object$spellcheck)){
    warning('assuming no spell checking')
    object$spellcheck <- FALSE
  }
  if(is.null(object$stem)){
    warning('assuming no spell stemming')
    object$stem <- FALSE
  }
  if(is.null(object$tfidf)){
    object$tfidf <- object$tdidf
  }
  if(is.null(object$tfidf)){
    warning('assuming no tdidf weighting')
    object$tfidf <- FALSE
  }
  if(is.null(object$ngrams)){
    warning('assuming no ngrams')
    object$ngrams <- 1
  }
  if(is.null(object$skips)){
    warning('assuming no skips')
    object$skips <- 0
  }
  if(object$tfidf & is.null(object$idf)){
    stop('object$tfidf is TRUE, but the idf weights are not provided')
  }
  if(object$tfidf & ! is.null(object$idf)){
    if(! all(is.finite(object$idf))){
      stop('object$idf must be all FINITE')
    }
  }

  #Tokenize
  if(verbose) {print('Tokenizing')}
  tokens <- strsplit(newdata, object$split_token)

  #Spellcheck
  if(object$spellcheck){
    warning('Spellcheck not currently supported')
    if(verbose) {}
  }

  #Stem the Tokens
  if(object$stem){
    if(verbose) print('Stemming')
    if(verbose){
      tokens <- pblapply(tokens, wordStem)
    } else {
      tokens <- lapply(tokens, wordStem)
    }
  }

  #Skip-grams
  if(object$skips >= 1){
    if(verbose) print('computing skips')
    skip_grams <- find_skip_grams(tokens, n=object$skips, verbose=verbose)
  }

  #N-grams
  if(object$ngrams > 1){
    if(verbose) print('computing ngrams')
    tokens <- find_ngrams(tokens, n=object$ngrams, verbose=verbose)
  }

  #Add skips to main tokenset (original + ngrams)
  if(object$skips >= 1){
    tokens <- mapply(c, tokens, skip_grams)
  }

  #Make row and column vectors
  if(verbose) print('Making Sparse Matrix')
  n.ids <- sapply(tokens, length)
  i <- rep(seq_along(n.ids), n.ids)
  j <- match(unlist(tokens), object$bagofwords)

  #Omit rows with no entries (these are novel queries)
  i <- i[!is.na(j)]
  j <- j[!is.na(j)]

  #Make a sparse matrix
  M <- sparseMatrix(i=i, j=j, x=rep(1, length(i)), dims=c(length(newdata), length(object$bagofwords)))
  colnames(M) <- make.names(object$bagofwords)

  #td-idf weighting
  #http://en.wikipedia.org/wiki/Tf%E2%80%93idf
  #Our matrix already contains term frequencies for each document
  if(object$tfidf){
    if(verbose) print('Doing td-idf weighting')
    tf_idf <- M * t(sapply(1:nrow(M), function(x) object$idf)) #TODO: IMPROVE THIS! NOT SPARSE!!!
    M <- tf_idf
  }

  out <- list(M=M, split_token=object$split_token, bagofwords=object$bagofwords, tfidf=object$tfidf, idf=object$idf, spellcheck=object$spellcheck, stem=object$stem, ngrams=object$ngrams, skips=object$skips)
  class(out) <- 'sparse_text_matrix'
  return(out)
}
