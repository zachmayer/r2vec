context("Text normalization")
test_that("Text normalization", {

  letter_map <- list(
    'a'=c('\uE0', '\uE1', '\uE2', '\uE3', '\uE4', '\uE5'),
    'e'=c('\uE8', '\uE9', '\uEA', '\uEB'),
    'i'=c('\uEC', '\uED', '\uEE', '\uEF'),
    'o'=c('\uF2', '\uF3', '\uF4', '\uF5', '\uF6'),
    'u'=c('\uF9', '\uFA', '\uFB', '\uFC'),
    'y'=c('\uFD', '\uFF'),
    'ae'=c('\uE6'),
    'oe'=c('\u153'),
    'c'=c('\uE7'),
    'n'=c('\uF1'))

  letter_map <- sapply(letter_map, clean_accent)
  expect_true(all(sapply(letter_map, function(x) length(unique(x) == 1)) == 1))
  for(n in names(letter_map)){
    expect_true(all(n == letter_map[[n]]))
  }
})
