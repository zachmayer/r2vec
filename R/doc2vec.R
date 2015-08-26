
#' A hacked together take on doc2vc
#'
#' Start with GloVe, which seems simple to implement in R.  Then add some doc2vec magic
#'
#' @param x A list of vectors, where each vector is a document, and each element of each vector is a word.
#' @return a matrix
#' @importFrom data.table data.table rbindlist
#' @importFrom Matrix sparseMatrix
#' @importFrom irlba irlba
#' @references
#' \url{http://rare-technologies.com/making-sense-of-word2vec/}
#' \url{http://nbviewer.ipython.org/github/fbkarsdorp/doc2vec/blob/master/doc2vec.ipynb}
#' \url{http://multithreaded.stitchfix.com/blog/2015/03/11/word-is-worth-a-thousand-vectors/#footnote5}
doc2vec <- function(
  x,
  n=3
  ){
  stop('Not implemented')

  x <- list(
    c('this', 'is', 'sentence', 'number', 'one'),
    c('this', 'sentences', 'number', 'um', 'the', 'number', 'is', 'one')
  )

  x <- find_tokens(
    text_normalization(
      inaugTexts,
      lowercase=TRUE,
      remove=c("'"),
      spaces=c("[[:punct:]]+", "[[:space:]]+"),
      remove_accents=TRUE,
      trim=TRUE
      )
    )

  out <- lapply(x, function(y) {
    if (length(y) <= 1)
      return(NULL)
    out <- lapply(2:n, function(n_i) {
      n_i <-  n_i + 1
      if (n_i > length(y)) return(NULL)
      o <- embed(y, n_i)
      data.table(
        context=c(o[,ncol(o)], o[,1]),
        word=c(o[,1], o[,ncol(o)]),
        count=log(n_i-1)
        )
    })
    rbindlist(out)
  })
  out <- rbindlist(out)

  keys <- c('context', 'word')
  setkeyv(out, keys)
  out <- out[,list(count=sum(count)), by=keys]

  bagofwords <- sort(unique(out$context))
  i <- out[,match(context, bagofwords)]
  j <- out[,match(word, bagofwords)]
  x <- out$count

  M <- sparseMatrix(i=i, j=j, x=x)
  M <- M[rowSums(sign(M)) > 5,]
  M <- M[colSums(sign(M)) > 5,]
  M[M == log1p(1)] <- 0
  M <- drop0(M)

  model <- irlba(M, nu=0, nv=2)
  m <- model$v
  row.names(m) <- bagofwords

  subset <- head(order(rowSums(sign(M))), 10)

  m <- data.frame(m)
  m$word <- bagofwords
  ggplot(m[subset,], aes(x=X1, y=X2, label=word)) + geom_text() + theme_bw()

  y <- x[[1]]
  o <- embed(y, 3)
  o
  cbind(o[,ncol(o)], o[,1])

  o <- embed(y, 4)
  o
  cbind(o[,ncol(o)], o[,1])

  o <- embed(y, 5)
  o
  cbind(o[,ncol(o)], o[,1])

}
