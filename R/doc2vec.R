#
# #' A hacked together take on doc2vc
# #'
# #' Start with GloVe, which seems simple to implement in R.  Then add some doc2vec magic
# #'
# #' @param x A list of vectors, where each vector is a document, and each element of each vector is a word.
# #' @param n number of vectors
# #' @return a matrix
# #' @importFrom data.table data.table rbindlist
# #' @importFrom Matrix sparseMatrix
# #' @importFrom irlba irlba
# #' @references
# #' \url{http://rare-technologies.com/making-sense-of-word2vec/}
# #' \url{http://nbviewer.ipython.org/github/fbkarsdorp/doc2vec/blob/master/doc2vec.ipynb}
# #' \url{http://multithreaded.stitchfix.com/blog/2015/03/11/word-is-worth-a-thousand-vectors/#footnote5}
# #' \url{http://arxiv.org/pdf/1507.07998v1.pdf}
# #' \url{http://www-cs.stanford.edu/~quocle/paragraph_vector.pdf}
# doc2vec <- function(
#   x,
#   n=3
#   ){
#   stop('Not implemented')
#
#   #MO IDEAS:
#   # ngrams, (1, 2, 3, etc) are CONEXTS
#   # Skipgram zero are ngrams/context alone
#   # Skipgrams added ON TOP OF existing contexts
#     #skip zero = context alone
#     #skip 1 = context + skip one
#     #etc
#
#   #IDEAS:
#   #1. Skip gram model.  context -> word, where context occurs BEFORE word
#   #2. Use skip-0 (bigrams), skip-1, skip-2, etc. to 10 or something.
#   #3. log1p(0), where skip0=1, skip1=0.69, ect.
#   #4. Sum up all the skips into one feature context -> word, with decaying weights
#   #5. Re-weight this final vector, such that in-frequenct context -> words get underweighted
#   #6. Make a word (rows) X context (columns) matrix
#   #7. Add rows to this matrix for documents, where the columns are the contexts (e.g. bag of words)
#   #7a. (Could include bigrams and trigrams as contexts, with skip0s, 1s, 2s, etc.  These new contexts would appear in #6 and #7)
#   #8. Factor this matrix!  You now have vectors for words and paragraphs!
#   #9. TODO: predict for new data?
#     # - generate unigrams (contexts)
#     # - multiply by rotation matrix from step #8
#     # - paragraph vectors for new data
#     # - this seems to ignore pairs of words and just look at single words
#     # - maybe bigrams/trigrams should be included as contexts (this is harder to program)
#
#   dev.off()
#   x <- c(
#     'this is sentence number one',
#     'this sentences number um the number is one',
#     'some text in a sentence wahoo',
#     'I write really bad movie reviews',
#     'I need to put some more words here',
#     'The king is great',
#     'The queen is great',
#     'The king is a man',
#     'The queen is a woman',
#     'The woman is female',
#     'The man is male'
#   )
#
#   library(quanteda)
#   library(data.table)
#   library(Matrix)
#   library(irlba)
#   library(ggplot2)
#   library(pbapply)
#   library(Rtsne)
#   library(SnowballC)
#   data(inaugTexts)
#
#   n <- 5
#   x <- inaugTexts
#   tokens <- find_tokens(
#     text_normalization(
#       x,
#       lowercase=TRUE,
#       remove=c("'"),
#       spaces=c("[[:punct:]]+", "[[:space:]]+"),
#       remove_accents=TRUE,
#       trim=TRUE
#       )
#     )
#   a <- sort(unique(unlist(tokens)))
#   head(a)
#   tail(a)
#
#   stops <- c(
#     "i", "me", "my", "myself", "we", "our", "ours", "ourselves",
#     "you", "your", "yours", "yourself", "yourselves", "he", "him",
#     "his", "himself", "she", "her", "hers", "herself", "it", "its",
#     "itself", "they", "them", "their", "theirs", "themselves", "what",
#     "which", "who", "whom", "this", "that", "these", "those", "am",
#     "is", "are", "was", "were", "be", "been", "being", "have", "has",
#     "had", "having", "do", "does", "did", "doing", "would", "should",
#     "could", "ought", "i'm", "you're", "he's", "she's", "it's", "we're",
#     "they're", "i've", "you've", "we've", "they've", "i'd", "you'd",
#     "he'd", "she'd", "we'd", "they'd", "i'll", "you'll", "he'll",
#     "she'll", "we'll", "they'll", "isn't", "aren't", "wasn't", "weren't",
#     "hasn't", "haven't", "hadn't", "doesn't", "don't", "didn't",
#     "won't", "wouldn't", "shan't", "shouldn't", "can't", "cannot",
#     "couldn't", "mustn't", "let's", "that's", "who's", "what's",
#     "here's", "there's", "when's", "where's", "why's", "how's", "a",
#     "an", "the", "and", "but", "if", "or", "because", "as", "until",
#     "while", "of", "at", "by", "for", "with", "about", "against",
#     "between", "into", "through", "during", "before", "after", "above",
#     "below", "to", "from", "up", "down", "in", "out", "on", "off",
#     "over", "under", "again", "further", "then", "once", "here",
#     "there", "when", "where", "why", "how", "all", "any", "both",
#     "each", "few", "more", "most", "other", "some", "such", "no",
#     "nor", "not", "only", "own", "same", "so", "than", "too", "very",
#     "states", "will", "us", "united"
#   )
#   tokens <- pblapply(tokens, function(x){x[!x  %in% stops]})
#   tokens <- pblapply(tokens, wordStem)
#
#   out <- lapply(tokens, function(y) {
#     if (length(y) <= 1)
#       return(NULL)
#     out <- lapply(2:(n+2), function(n_i) {
#       if (n_i > length(y)) return(NULL)
#       o <- embed(y, n_i)
#       data.table(
#         context=c(o[,ncol(o)], o[,1]),
#         word=c(o[,1], o[,ncol(o)]),
#         count=1 / (n_i - 1) #Change to gen logit, 1 to 0.1, midpoint at 5
#         )
#     })
#     rbindlist(out)
#   })
#   out <- rbindlist(out)
#
#   keys <- c('context', 'word')
#   setkeyv(out, keys)
#   out <- out[,list(count=sum(count)), by=keys]
#   out
#
#   uniq_words <- sort(unique(out$word))
#   uniq_contexts <- sort(unique(out$context))
#   i <- out[,match(word, uniq_words)]
#   j <- out[,match(context, uniq_contexts)]
#   x <- out$count
#
#   #Word vectors
#   M1 <- sparseMatrix(
#     i=i, j=j, x=x,
#     dimnames=list(
#       uniq_words,
#       uniq_contexts
#     )
#   )
#   #M1 <- M1[rowSums(sign(M1)) > 5,]
#   #M1 <- M1[colSums(sign(M1)) > 5,]
#   M1 <- drop0(M1)
#   M1 <- unique(M1)
#
#   #Document vectors
#   n.ids <- sapply(tokens, length)
#   i <- rep(seq_along(n.ids), n.ids)
#   j <- match(unlist(tokens), uniq_contexts)
#   i <- i[!is.na(j)]
#   j <- j[!is.na(j)]
#
#   #Make a sparse matrix
#   M2 <- sparseMatrix(
#     i=i, j=j,
#     x=rep(1, length(i)),
#     dims=c(length(tokens), ncol(M1))
#     )
#   colnames(M2) <- colnames(M1)
#
#   df <- data.frame(
#     Year = inaugCorpus$documents$Year,
#     President = inaugCorpus$documents$President
#   )
#   df$Label <- paste0(df$President, ' (', substr(df$Year, 3, 4), ')')
#   df$Year <- as.numeric(as.character(df$Year))
#   rownames(M2) <- df$Label
#
#   #Combine document matrix with word matrix
#   M <- rbind(M1, M2) #Pre-allocate?
#
#   #tfidf
#   idf <- log(nrow(M)/colSums(sign(M)))
#   tf_idf <- M * t(sapply(1:nrow(M), function(x) idf))
#   M <- tf_idf
#
#   #Combined model
#   set.seed(1)
#   model <- irlba(M, nu=25, nv=0)
#   m <- model$u
#   row.names(m) <- row.names(M)
#
#   #TSNE
#   set.seed(2)
#   tsne_proj <- Rtsne(m[(nrow(M1) + 1):nrow(M),], dims=2, pca=FALSE, perplexity=5)$Y
#   colnames(tsne_proj) <- paste0('TSNE', seq_along(1:ncol(tsne_proj)))
#
#   set.seed(3)
#   m2 <- irlba(M[(nrow(M1) + 1):nrow(M),], nu=25, nv=0)$u
#   set.seed(4)
#   tsne_proj2 <- Rtsne(m2, dims=2, pca=FALSE, perplexity=5)$Y
#   colnames(tsne_proj2) <- paste0('TSNE', seq_along(1:ncol(tsne_proj)))
#
#   #Plot1 - document vectors
#   plt <- data.frame(tsne_proj)
#   plt$Label <-  df$Label
#   plt$Year <- df$Year
#   #ggplot(plt, aes(x=TSNE1, y=TSNE2, label=Label)) + geom_text() + theme_bw()
#
#   p1 <- ggplot(plt, aes(x=TSNE1, y=TSNE2, fill=Year, label=Label)) +
#     scale_fill_gradient2(low='#d73027', mid='#ffffbf', high='#4575b4', midpoint=1900) +
#     geom_point(pch=21, size=5, alpha=.80) +
#     geom_point(pch=21, size=5, colour = "black") +
#     geom_text(size=3, vjust=1.5, alpha=.80) +
#     theme_bw()
#   print(p1 + ggtitle('doc vectors'))
#
#   #Plot2 - bagofwords
#   plt2 <- data.frame(tsne_proj2)
#   plt2$Label <-  df$Label
#   plt2$Year <- df$Year
#   #ggplot(plt2, aes(x=TSNE1, y=TSNE2, label=Label)) + geom_text() + theme_bw()
#
#   p2 <- ggplot(plt2, aes(x=TSNE1, y=TSNE2, fill=Year, label=Label)) +
#     scale_fill_gradient2(low='#d73027', mid='#ffffbf', high='#4575b4', midpoint=1900) +
#     geom_point(pch=21, size=5, alpha=.80) +
#     geom_point(pch=21, size=5, colour = "black") +
#     geom_text(size=3, vjust=1.5, alpha=.80) +
#     theme_bw()
#   print(p2 + ggtitle('bagofwords'))
#
#
#   most_similar <- function(word='democraci', mat=m){
#     mat <- mat %*% diag(1/colSums(mat))
#     x <- mat[word,,drop=FALSE]
#     sim <- x %*% t(mat[-which(row.names(mat)==word),])
#     sim <- sim[1,]
#     sim <- sort(sim, decreasing=TRUE)
#     head(sim, 50)
#   }
#   most_similar('democraci')
#   most_similar('steel')
#   most_similar('spain')
#   most_similar('health')
#   most_similar('fear')
#   most_similar('defend')
#   most_similar('america')
#   most_similar('god')
#   most_similar('trust')
#   most_similar('trumpet')
#
#   #WORK
#   y <- x[[1]]
#   o <- embed(y, 3)
#   o
#   cbind(o[,ncol(o)], o[,1])
#
#   o <- embed(y, 4)
#   o
#   cbind(o[,ncol(o)], o[,1])
#
#   o <- embed(y, 5)
#   o
#   cbind(o[,ncol(o)], o[,1])
#
# }
