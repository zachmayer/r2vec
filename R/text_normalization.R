#' Preprocess text data for use in PCA
#'
#' Normalize text data by removing accents, special characters, and normalizing whitespace.
#'
#' @param x a character vector
#' @param remove is a character vector (or regex) of characters to remove entirely
#' @param spaces is a character vector (or regex) of characters to convert to spaces
#' @param remove_accents boolean, decide whether or not to remove accents
#' @param lowercase Decide whether or not to convert the text to lowercase
#' @export
#' @importFrom stringr str_trim
#' @return a character vector
text_normalization <- function(x, remove=c("'"), spaces=c("[[:punct:]]", "[[:space:]]+"), remove_accents=FALSE, lowercase=TRUE){

  #Lowercase
  if(lowercase) x <- tolower(x)

  #Remove some characters
  for(regular_expr in remove){
    i <- grepl(regular_expr, x)
    x[i] <- gsub(regular_expr, "", x[i])
  }

  #Convert other characters to spaces
  for(regular_expr in spaces){
    i <- grepl(regular_expr, x)
    x[i] <- gsub(regular_expr, " ", x[i])
  }

  #Remove accents and special characters
  if (remove_accents){
    x <- clean_accent(x)
  }

  #Trim leading and trailing spaces
  x <- str_trim(x)

  #Return
  return(x)
}
