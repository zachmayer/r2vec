context("Full text to sparse pipeline")
test_that("Full text to sparse pipeline", {
  skip_on_cran()
  library(quanteda)
  data("inaugTexts")

  set.seed(1)
  M_plain <- textVectors(
    inaugTexts,
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
    tsne=FALSE)
  expect_equal(dim(M_plain$M), c(length(inaugTexts), length(M_plain$bagofwords)))

  set.seed(1)
  M_ngram <- textVectors(
    inaugTexts,
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
    ngrams=5,
    skips=0,
    stops=NULL,
    pca=FALSE,
    tsne=FALSE)
  expect_equal(dim(M_ngram$M), c(length(inaugTexts), length(M_ngram$bagofwords)))

  set.seed(1)
  M_skip <- textVectors(
    inaugTexts,
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
    skips=5,
    stops=NULL,
    pca=FALSE,
    tsne=FALSE)
  expect_equal(dim(M_skip$M), c(length(inaugTexts), length(M_skip$bagofwords)))

  set.seed(1)
  M_norm <- textVectors(
    inaugTexts,
    normalize=TRUE,
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
    tsne=FALSE)
  expect_equal(dim(M_norm$M), c(length(inaugTexts), length(M_norm$bagofwords)))

  set.seed(1)
  M_stem <- textVectors(
    inaugTexts,
    normalize=TRUE,
    split_token=' ',
    verbose=FALSE,
    freqCutoff=0,
    absCutoff=0,
    tfidf=FALSE,
    idf=NULL,
    bagofwords=NULL,
    spellcheck=FALSE,
    remove_stopwords=FALSE,
    stem=TRUE,
    ngrams=1,
    skips=0,
    stops=NULL,
    pca=FALSE,
    tsne=FALSE)
  set.seed(1)
  expect_equal(dim(M_stem$M), c(length(inaugTexts), length(M_stem$bagofwords)))

  M_pca <- textVectors(
    inaugTexts,
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
    pca=TRUE,
    pca_comp=15,
    pca_rotation=NULL,
    tsne=FALSE)
  expect_equal(dim(M_pca$M), c(length(inaugTexts), length(M_pca$bagofwords)))

  set.seed(1)
  M_tnse <- textVectors(
    inaugTexts,
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
    pca=TRUE,
    pca_comp=15,
    pca_rotation=NULL,
    tsne=TRUE,
    tsne_dims=2,
    tsne_perplexity=3)
  expect_equal(dim(M_tnse$M), c(length(inaugTexts), length(M_tnse$bagofwords)))

  set.seed(1)
  M_dictionary <- textVectors(inaugTexts, bagofwords=letters)
  expect_equal(dim(M_dictionary$M), c(length(inaugTexts), length(letters)))

  set.seed(1)
  M_complicated <- textVectors(
    inaugTexts,
    normalize=TRUE,
    split_token=' ',
    verbose=FALSE,
    freqCutoff=.05,
    absCutoff=5,
    tfidf=TRUE,
    remove_stopwords=TRUE,
    stem=TRUE,
    ngrams=3,
    skips=3,
    pca=TRUE,
    pca_comp=15,
    tsne=TRUE,
    tsne_dims=2,
    tsne_perplexity=3)
  expect_equal(dim(M_complicated$M), c(length(inaugTexts), length(M_complicated$bagofwords)))

  expect_more_than(length(M_plain$bagofwords), length(M_norm$bagofwords))
  expect_more_than(length(M_plain$bagofwords), length(M_stem$bagofwords))
  expect_less_than(length(M_plain$bagofwords), length(M_ngram$bagofwords))
  expect_less_than(length(M_plain$bagofwords), length(M_skip$bagofwords))
  expect_more_than(length(M_plain$bagofwords), length(M_complicated$bagofwords))
  expect_more_than(length(M_ngram$bagofwords), length(M_complicated$bagofwords))
  expect_more_than(length(M_ngram$bagofwords), length(M_skip$bagofwords))
  expect_more_than(length(M_skip$bagofwords), length(M_complicated$bagofwords))

  expect_equal(M_plain$bagofwords, M_tnse$bagofwords)
  expect_equal(M_plain$bagofwords, M_pca$bagofwords)

  expect_equal(M_plain$M, M_tnse$M)
  expect_equal(M_plain$M, M_pca$M)

  expect_equal(M_plain$pca_rotation, NULL)
  expect_equal(M_norm$pca_rotation, NULL)
  expect_equal(M_stem$pca_rotation, NULL)

  expect_equal(M_plain$tsne_proj, NULL)
  expect_equal(M_norm$tsne_proj, NULL)
  expect_equal(M_stem$tsne_proj, NULL)

  expect_equal(M_tnse$pca_rotation, M_pca$pca_rotation)
  expect_equal(M_tnse$pca_rotation, M_pca$pca_rotation)

  #Check PCA dims
  for(model in list(
    M_pca,
    M_tnse,
    M_complicated
  )){
    expect_equal(dim(model$x), c(length(inaugTexts), 15))
    expect_equal(ncol(model$pca_rotation), 15)
    expect_equal(nrow(model$pca_rotation), length(model$bagofwords))
    expect_equal(nrow(model$pca_rotation), ncol(model$M))
  }

  #Check PCA dims
  for(model in list(
    M_tnse,
    M_complicated
  )){
    expect_equal(dim(model$tsne_proj), c(length(inaugTexts), 2))
  }
})
