
#' Tokenize a vector of text and convert to a sparse matrix. TODO: tf-idf is BROKEN, n-grams are broken, skip-grams are broken!
#'
#' This code takes a vector of text, cleans it up, tokenizes it, spellchecks it,
#' removes stopwords, stems it, finds n-grams, crates a bag og words and
#' converts it to a sparse matrix using a bag of words model.  Optinally
#' it also applies td-idf weighting to the matrix.  This function can be slow.
#' Note that freqCutoff and absCutoff are relative to the number of documents
#' the term appears in, and ignore its frequency within documents.
#'
#' @param x a character vector
#' @param normalize normalize the character vector by converting to lowercase, removing accents, and converting punctuation and spaces to single spaces and then trimming the string.
#' @param split_token token to use to split the text data.  If NULL, text will not be tokenized and the bagofwords will be detected via regular expressions.
#' @param verbose whether to print a log while performing the operations
#' @param freqCutoff columns below this pct frequency will be removed from the final object
#' @param absCutoff columns below this absolute frequency will be removed from the final object
#' @param tfidf whether to apply tfidf weighting.  NOTE THAT THIS WILL CREATE A DENSE MATRIX, WHICH IN MANY CASES IS BAD.
#' @param idf Pre-computed inverse document frequencies (perhaps from another,  larger dataset)
#' @param bagofwords input bagofwords to use to construct the final matrix
#' @param remove_stopwords if TRUE, english stopwords will be removed from the tokens
#' @param spellcheck if TRUE tokens will be spellchecked before they are stemmed
#' @param stem if TRUE the tokens will be stemmed, after tokenizing and before creating a matrix
#' @param ngrams If great than 1, n-grams of this degree will be added to the word bag
#' @param skips If great than 0, skips of this degree will be added to the word bag
#' @param stops Optional list of stopwords, otherwise a default list will be used.
#' @param pca Apply PCA after transforming text to sparse matrix?
#' @param pca_comp Number of components to use for PCA
#' @param pca_rotation Rotation matrix to use for PCA.  If NULL, will be computed by irlba.
#' @param tsne Apply the tsne transformation after the PCA rotation?
#' @param tsne_dims Dimension of the final TSNE embedding. Should be smaller than pca_comp.
#' @param tsne_perplexity Preplexity for the tsne transformation.
#' @export
#' @importFrom pbapply pblapply
#' @importFrom SnowballC wordStem
#' @importFrom Matrix sparseMatrix colSums
#' @importFrom foreach foreach %dopar%
#' @importFrom stringi stri_detect_fixed
#' @importFrom Rtsne Rtsne
#' @return a textVectors object
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
#' textVectors(
#'   x,
#'   absCutoff=1, ngrams=2, stem=TRUE, verbose=TRUE)
#' textVectors(
#'   x,
#'   absCutoff=1, ngrams=2, skips=1, stem=TRUE, verbose=TRUE, tfidf=TRUE)
textVectors <- function(
  x,
  normalize=FALSE,
  split_token=' ',
  verbose=FALSE,
  freqCutoff=0,
  absCutoff=0,
  tfidf=FALSE,
  idf=NULL,
  bagofwords=NULL,
  spellcheck=FALSE,
  remove_stopwords=FALSE,
  stem=FALSE,
  ngrams=1,
  skips=0,
  stops=NULL,
  pca=FALSE,
  pca_comp=5,
  pca_rotation=NULL,
  tsne=FALSE,
  tsne_dims=2,
  tsne_perplexity=30){

  stopifnot(is.character(x))
  if(tsne & !pca){
    stop('If tsne is specified, you MUST do PCA first.  Set pca to TRUE or tsne to FALSE')
  }

  if(normalize){
    if(verbose) print('Normalizing')
    x <- text_normalization(
      x,
      lowercase=TRUE,
      remove=c("'"),
      spaces=c("[[:punct:]]+", "[[:space:]]+"),
      remove_accents=TRUE,
      trim=TRUE)
  }

  #Normal tokenize/stop/stem/skip-gram/n-gram flow
  if(!is.null(split_token)){

    if(verbose) print('Tokenizing')
    tokens <- find_tokens(x, split_token, regex=FALSE)

    #Spellcheck
    if(spellcheck){
      if(verbose) {print('Starting spell checking')}
      warning('Spellcheck not currently supported')
    }

    #Remove Stopwords
    if(is.null(stops)){

      #SPLIT TO OWN FUNCTION
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
      tokens <- as.relistable(tokens)
      tokens <- unlist(tokens)
      unique_tokens <- unique(tokens)
      token_map <- match(tokens, unique_tokens)
      unique_tokens <- wordStem(unique_tokens)
      tokens[] <- unique_tokens[token_map]
      tokens <- relist(tokens)
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

  } else {

    #Lookup bagowords as regex flow
    if(!is.null(bagofwords)){
      i <- foreach(i=1:length(bagofwords), .verbose=verbose) %dopar% {
        which(stri_detect_fixed(x, bagofwords[i]))
      }
      n.ids <- sapply(i, length)
      j <- rep(seq_along(n.ids), n.ids)
      i <- unlist(i)
    } else{
      stop('split_token and bagofwords cannot both be NULL')
    }
  }

  #Omit rows with no entries (these are novel queries)
  i <- i[!is.na(j)]
  j <- j[!is.na(j)]

  #Make a sparse matrix
  M <- sparseMatrix(i=i, j=j, x=rep(1, length(i)), dims=c(length(x), length(bagofwords)))
  colnames(M) <- bagofwords

  #Remove extremely low-frequency words
  if(freqCutoff>0){
    if(verbose) print('Removing words with a low relative frequency')
    stopifnot(freqCutoff<1)
    tmp <- freqCutoff * nrow(M)
    if(tmp > absCutoff){
      if(verbose) print('Replacing absCutoff with higher value based on freqCutoff * nrow(M)')
      absCutoff <- tmp
    } else{
      if(verbose) print('Ignoring freqCutoff: freqCutoff * nrow(M) is less than absCutoff')
    }
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
    if(is.null(idf)){
      if(verbose) print('Computing idf weights')
      idf <- log(nrow(M)/colSums(sign(M)))
    } else{
      if(verbose) print('Using pre-defined idf weights')
    }

    #TODO: IMPROVE THIS! NOT SPARSE!!!
    if(verbose) print('Applying idf weights')
    tf_idf <- M * t(sapply(1:nrow(M), function(x) idf))
    M <- tf_idf
  }

  if(pca){
    if(is.null(pca_rotation)){
      if(verbose) print('Using irlba to determine pca rotation')
      pca_rotation <- sparseprcomp(M, n=pca_comp)$rotation
    } else{
      if(verbose) print('Using saved pca rotation')
    }
    x <- M %*% pca_rotation
    x <- as.matrix(x)
  } else{
    x <- NULL
  }

  if(tsne){
    tsne_proj <- Rtsne(x, dims=tsne_dims, pca=FALSE, perplexity=tsne_perplexity)$Y
    colnames(tsne_proj) <- paste0('TSNE', seq_along(1:ncol(tsne_proj)))
  } else{
    tsne_proj <- NULL
  }

  out <- list(
    M=M,
    x=x,
    tsne_proj=tsne_proj,
    normalize=normalize,
    split_token=split_token,
    verbose=verbose,
    freqCutoff=freqCutoff,
    absCutoff=absCutoff,
    tfidf=tfidf,
    idf=idf,
    bagofwords=bagofwords,
    spellcheck=spellcheck,
    remove_stopwords=remove_stopwords,
    stem=stem,
    ngrams=ngrams,
    skips=skips,
    stops=stops,
    pca=pca,
    pca_comp=pca_comp,
    pca_rotation=pca_rotation,
    tsne=tsne,
    tsne_dims=tsne_dims,
    tsne_perplexity=tsne_perplexity
    )
  class(out) <- 'textVectors'
  return(out)
}

#' Take a sparse text matrix and construct a matric with the same specification from a new dataset
#'
#' This code takes an existing sparse text matrix, and a character vector of new data.  It then tokenizes the new data, spellchecks it (todo),
#' applies n-grams, and uses the bag of words from the input matrix to contruct a new matrix with the exact same columns.  It then optionally
#' applies the input matrix's tf-idf weightings to the new matrix and returns a new textVectors object. TODO: COMBINE THIS WITH THE MATRIX FUNCTION!!!
#'
#' @param object object of class textVectors
#' @param newdata new data to apply the same matrix format to
#' @param verbose print diagnostic messages
#' @param ... ignored
#' @method predict textVectors
#' @export
#' @importFrom pbapply pblapply
#' @importFrom SnowballC wordStem
#' @importFrom Matrix sparseMatrix
#' @return a textVectors object
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
#' a <- textVectors(
#'   x,
#'   absCutoff=1, ngrams=2, stem=TRUE, verbose=TRUE)
#' b <- textVectors(
#'   x,
#'   absCutoff=1, ngrams=2, stem=TRUE, verbose=TRUE, tfidf=TRUE)
#' predict(a, y)
#' predict(b, y)
predict.textVectors <- function(object, newdata=NULL, verbose=FALSE, ...){
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
    warning('assuming no stemming')
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
  if(object$tsne){
    warning('Cannot apply tsne transformation to new data.  ONLY PCA transformation will be applied to new data.  If you use this new data in a pre-trained model you will get very wrong predictions. Consider re-training your model with tsne=FALSE to get correct results for prediciton.')
    object$tsne <- FALSE
  }
  out <- textVectors(
    newdata,
    normalize=object$normalize,
    split_token = object$split_token,
    verbose = verbose,
    freqCutoff = 0,
    absCutoff = 0,
    tfidf = object$tfidf,
    idf = object$idf,
    bagofwords = object$bagofwords,
    spellcheck = object$spellcheck,
    remove_stopwords = object$remove_stopwords,
    stem = object$stem,
    ngrams = object$ngrams,
    skips = object$skips,
    stops = object$stops,
    pca=object$pca,
    pca_comp=object$pca_comp,
    pca_rotation=object$pca_rotation,
    tsne=object$tsne,
    tsne_dims=object$tsne_dims,
    tsne_perplexity=object$tsne_perplexity)

  return(out)
}

#' Print a text vectors object
#'
#' @param x object of class textVectors
#' @param ... ignored
#' @method print textVectors
#' @export
print.textVectors <- function(x,  ...){
  print('A text vectors object')
  print('Sparse components:')
  min <- min(7, ncol(x$M))
  print(as.matrix(head(x$M[, 1:min])))
  if(!is.null(x$x)){
    print('Dense components:')
    min <- min(10, ncol(x$x))
    print(head(x$x[,1:min]))
  }
  if(!is.null(x$tsne_proj)){
    print('TSNE components:')
    min <- min(10, ncol(x$tsne_proj))
    print(head(x$tsne_proj[,1:min]))
  }
}

#' Plot a text vectors object
#'
#' @param x object of class textVectors
#' @param ... ignored
#' @method plot textVectors
#' @export
plot.textVectors <- function(x, ...){
  if(is.null(x$tsne_proj)){
    if(is.null(x$x)){
      dat <- as.matrix(x$M[,1:2])
    } else{
      dat <- x$x[,1:2]
    }
  } else{
    dat <- x$tsne_proj[,1:2]
  }
  plot(dat)
}
