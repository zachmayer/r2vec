#' Remove accents from text data
#'
#' This function removes accents from a, e, i, o, u, y, c, and n.  It also converts 'ae' and 'oe' single unicode characters to 2 ascii characters.
#' Note that it only currently works on lowercase text data.  See http://unicodelookup.com/
#' @param x a character vector
#' @export
#' @return a character vector
#' @importFrom stringi stri_detect_regex stri_replace_all_regex
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

  letters_to_change <- unlist(letter_map)
  if(any(duplicated(letters_to_change))) stop('duplicate letter mappings defined')

  for(new_letter in names(letter_map)){
    regex <- paste(letter_map[[new_letter]], collapse='|')
    i <- stri_detect_regex(x, regex)
    x[i] <- stri_replace_all_regex(x[i], regex, new_letter)
  }
  return(x)
}

#' Normalize text data
#'
#' Normalize text data by removing accents, special characters, and normalizing whitespace.  This can remove a lot of information from your strings.
#'
#' @param x a character vector
#' @param lowercase Decide whether or not to convert the text to lowercase
#' @param remove is a character vector (or regex) of characters to remove entirely
#' @param spaces is a character vector (or regex) of characters to convert to spaces
#' @param remove_accents boolean, decide whether or not to remove accents
#' @param trim boolean, trim the final string before returning
#' @export
#' @importFrom stringi stri_trim
#' @importFrom pbapply pblapply
#' @importFrom stringi stri_trans_tolower stri_detect_regex stri_replace_all_regex
#' @return a character vector
text_normalization <- function(
  x,
  lowercase=TRUE,
  remove=c("'"),
  spaces=c("[[:punct:]]+", "[[:space:]]+"),
  remove_accents=FALSE,
  trim=TRUE
){

  #Lowercase
  if(lowercase) x <- stri_trans_tolower(x)

  #Remove some characters
  for (regular_expr in remove){
    #i <- stri_detect_regex(x, regular_expr)
    #x[i] <- stri_replace_all_regex(x[i], regular_expr, "")
    i <- grepl(regular_expr, x)
    x[i] <- gsub(regular_expr, "", x[i])
  }

  #Convert some characters to spaces
  for (regular_expr in spaces){
    #i <- stri_detect_regex(x, regular_expr)
    #x[i] <- stri_replace_all_regex(x[i], regular_expr, " ")
    i <- grepl(regular_expr, x)
    x[i] <- gsub(regular_expr, " ", x[i])
  }

  #Remove accents and special characters
  if (remove_accents){
    x <- clean_accent(x)
  }

  #Trim leading and trailing spaces
  x <- stri_trim(x)

  #Return
  return(x)
}
