#' Remove accents from text data
#'
#' This function removes accents from a, e, i, o, u, y, c, and n.  It also converts 'ae' and 'oe' single unicode characters to 2 ascii characters.
#' Note that it only currently works on lowercase text data.  See http://unicodelookup.com/
#' @param x a character vector
#' @export
#' @return a character vector
clean_accent <- function(x){

  letter_map <- list(
    'a'=c('\uE0','\uE1','\uE2','\uE3','\uE4','\uE5'),
    'e'=c('\uE8','\uE9', '\uEA','\uEB'),
    'i'=c('\uEC','\uED','\uEE','\uEF'),
    'o'=c('\uF2','\uF3','\uF4','\uF5','\uF6'),
    'u'=c('\uF9','\uFA','\uFB','\uFC'),
    'y'=c('\uFD','\uFF'),
    'ae'=c('\uE6'),
    'oe'=c('\u153'),
    'c'=c('\uE7'),
    'n'=c('\uF1'))

  all_letters <- unlist(letter_map)
  if(any(duplicated(all_letters))) stop('duplicate letter mappings defined')
  i <- grepl(paste(all_letters, collapse='|'), x)
  for(end_letter in names(letter_map)){
    start_letters <- paste(letter_map[[end_letter]], collapse='|')
    x[i] <- gsub(start_letters, end_letter, x[i])
  }
  return(x)
}
