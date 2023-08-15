#' Suggest Code for Creating Age Bands
#'
#' Prints out suggested dplyr code to generate age bands. This is intended to serve as a guide only
#' and therefore code generated should be reviewed and modified as necessary.
#'
#' @param col Name of age column to transform.
#' @param age_bands This should be a vector of age bands containing numeric values such as c("0-14","15-65","65+"), where the
#' order of the vector specifies the order with which the age_band appears within the returned dplyr::case_when statement.
#'
#' This parameter can be left blank if common_age_bands has been specified as 1,2 or 3.
#' In these cases the age bands corresponding to these values (as specified in the common_age_bands parameter) will be used.
#' If common_age_bands is specified as 1,2 or 3 and age_bands is also specified, the values from common_age_bands will be used.
#' @param common_age_bands When this is null or outside the specified values below, the function
#'  will return dplyr code corresponding to the age bands specified in the parameter age_bands.
#'  If, instead, a numeric value 1,2 or 3 is inputted for this parameter, the following age bands below will be used instead:
#'
#' 1 = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
#' "50-54","55-59","60-64","65+")
#'
#' 2 = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
#' "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100+")
#'
#' 3 = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+").
#'
#' @return A statement printed in the console, giving the suggested dplyr code to create age bands from an
#' age column
#' @export
#'@seealso [order_age_bands()] for suggesting code to order age bands
#' @examples
#'
#'
#' data <- data.frame(
#'   Age = c(
#'     5L, 110L, 87L, 108L, 57L, 168L, 133L, 62L,
#'     125L, 140L, 38L, 168L, 159L, 155L, 98L, 37L, 24L, 48L, 141L,
#'     34L, 14L, 129L, 70L, 121L, 102L, 8L, 113L, 113L, 3L, 71L, 111L,
#'     95L, 41L, 26L, 170L, 160L, 73L, 76L, 84L, 106L, 73L, 1L, 58L,
#'     190L, 22L, 118L, 52L, 79L
#'   ),
#'   ID = c(
#'     2251558L, 2540739L, 2121923L, 268210L,
#'     1695141L, 103196L, 485232L, 2431101L, 1565663L, 1687369L, 762799L,
#'     293488L, 1476981L, 1764643L, 2601051L, 463056L, 867640L,
#'     426493L, 1424699L, 1166167L, 1314136L, 2079697L, 1230127L, 1055453L,
#'     2529562L, 909298L, 831791L, 2346930L, 2341395L, 573434L,
#'     1669739L, 1501823L, 590632L, 186521L, 495621L, 1007120L, 1404587L,
#'     626912L, 2593705L, 2148515L, 1051717L, 1540477L, 1337032L,
#'     1694720L, 101417L, 1592089L, 2065567L, 93434L
#'   ),
#'   Value = c(
#'     3L, 7L, 10L, 10L, 6L, 10L, 8L, 4L, 1L, 3L,
#'     10L, 4L, 2L, 2L, 8L, 7L, 1L, 7L, 1L, 10L, 3L, 9L, 6L, 8L, 4L,
#'     5L, 1L, 1L, 10L, 2L, 3L, 8L, 4L, 8L, 5L, 4L, 5L, 9L, 8L, 4L, 8L,
#'     7L, 4L, 2L, 7L, 6L, 8L, 2L
#'   )
#' )
#'
#' age_bands2 <- c("<5", "5-9", "10-15", "20-99", "100-119", "120+")
#'
#' generate_age_bands("Age", age_bands2)
#' generate_age_bands("Age", age_bands2, 2)
#'
generate_age_bands <- function(col = "Age", age_bands = NULL, common_age_bands = 999) {
  if (common_age_bands == 1) {
    age_bands <- c(
      "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
      "50-54", "55-59", "60-64", "65+"
    )
  }
  if (common_age_bands == 2) {
    age_bands <- c(
      "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49",
      "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85-89", "90-94", "95-99", "100+"
    )
  }

  if (common_age_bands == 3) {
    age_bands <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70-79", "80-89", "90-99", "100+")
  }

  if (common_age_bands != 1 & common_age_bands != 2 & common_age_bands != 3 & common_age_bands != 999) {
    warning("The parameter common_age_bands has been specified but the value is not equal to the default value of 999 or
            the accepted values of 1,2 or 3. The function will return suggested code based on the age_bands parameter instead.")
  }

  if (common_age_bands != 1 & common_age_bands != 2 & common_age_bands != 3 &
      (is.null(age_bands) | !is.character(age_bands))) {
    stop("The parameter common_age_bands has not been specified, or has been specified with the wrong value.
         Additionally the age_bands parameter has not been specified or has been specified in an incorrect format.")
  }



  age_bands_formatted <- stringr::str_extract_all(age_bands, "[[:digit:]]+")

  if (sum(lapply(age_bands_formatted, length) %>% unlist() == 0, na.rm = T)) {
    stop("Vector supplied in age_bands parameter contains character values which do not contain numeric values
         and are not NA values.")
  }

  string <- paste0("mutate( \n", "age_group = dplyr::case_when( \n")


  for (i in seq_along(age_bands)) {
    if (length(as.vector(age_bands_formatted[[i]])) == 1 & i == 1) {
      string <- paste0(string, paste0(col, "<", as.numeric(age_bands_formatted[[i]]), "   ~", "'", age_bands[i]), "' ,", "\n")
    }
    if (length(as.vector(age_bands_formatted[[i]])) == 1 & i != 1) {
      string <- paste0(string, paste0(col, ">", as.numeric(age_bands_formatted[[i]]), "   ~", "'", age_bands[i]), "'", "\n")
    }

    if (length(as.vector(age_bands_formatted[[i]])) == 2 & i != length(age_bands_formatted)) {
      paste0(col, "<=", as.numeric(as.vector(age_bands_formatted[[i]][2])), "   ~", "'", age_bands[i])
      string <- paste0(string, paste0(col, "<=", as.numeric(as.vector(age_bands_formatted[[i]][2])), "   ~", "'", age_bands[i]), "' ,", "\n")
    }
    if (length(as.vector(age_bands_formatted[[i]])) == 2 & i == length(age_bands_formatted)) {
      paste0(col, "<=", as.numeric(as.vector(age_bands_formatted[[i]][2])), "   ~", "'", age_bands[i])
      string <- paste0(string, paste0(col, "<=", as.numeric(as.vector(age_bands_formatted[[i]][2])), "   ~", "'", age_bands[i]), "'", "\n")
    }
  }

  return(cat(paste0(string, "))")))
}


#' Suggest Code for Ordering of Age Bands
#'
#'@description
#' Returns a suggested ordering of age bands. This is intended to serve as a guide only
#' and therefore code generated should be reviewed and modified as necessary.
#'
#' The function will output an vector containing the sugggested ordering, and also
#' print out the code needed to create this vector, to support any modification required.
#'
#' The function is intended for use in creating visualisations ordered by age groups.
#'
#' @param y This parameter should be a vector or data frame column of age bands.
#'
#'
#' @return A vector of the unique age band values reordered
#' @export
#'@seealso [generate_age_bands()] for suggesting code to create age bands from an age vector
#' @examples
#'
#'
#' data3 <- data.frame(
#'   stringsAsFactors = FALSE,
#'   Age = c(
#'     5L, 110L,
#'     87L, 108L, 57L, 168L,
#'     133L, 62L, 125L,
#'     140L, 38L, 168L, 159L,
#'     155L, 98L, 37L,
#'     24L, 48L, 141L, 34L,
#'     14L, 129L, 70L, 121L,
#'     102L, 8L, 113L,
#'     113L, 3L, 71L, 111L,
#'     95L, 41L, 26L, 170L,
#'     160L, 73L, 76L, 84L,
#'     106L, 73L, 1L, 58L,
#'     190L, 22L, 118L, 52L,
#'     79L
#'   ),
#'   ID = c(
#'     2251558L,
#'     2540739L, 2121923L,
#'     268210L, 1695141L,
#'     103196L, 485232L,
#'     2431101L, 1565663L,
#'     1687369L, 762799L,
#'     293488L, 1476981L,
#'     1764643L, 2601051L,
#'     463056L, 867640L, 426493L,
#'     1424699L, 1166167L,
#'     1314136L, 2079697L,
#'     1230127L, 1055453L,
#'     2529562L, 909298L,
#'     831791L, 2346930L,
#'     2341395L, 573434L,
#'     1669739L, 1501823L,
#'     590632L, 186521L,
#'     495621L, 1007120L,
#'     1404587L, 626912L,
#'     2593705L, 2148515L,
#'     1051717L, 1540477L,
#'     1337032L, 1694720L,
#'     101417L, 1592089L,
#'     2065567L, 93434L
#'   ),
#'   Value = c(
#'     3L, 7L,
#'     10L, 10L, 6L, 10L, 8L,
#'     4L, 1L, 3L, 10L, 4L,
#'     2L, 2L, 8L, 7L, 1L,
#'     7L, 1L, 10L, 3L,
#'     9L, 6L, 8L, 4L, 5L,
#'     1L, 1L, 10L, 2L, 3L,
#'     8L, 4L, 8L, 5L, 4L,
#'     5L, 9L, 8L, 4L, 8L,
#'     7L, 4L, 2L, 7L, 6L,
#'     8L, 2L
#'   ),
#'   age_group = c(
#'     "5-9",
#'     "100+", "85-89", "100+",
#'     "55-59", "100+",
#'     "100+", "60-64",
#'     "100+", "100+", "35-39",
#'     "100+", "100+",
#'     "100+", "95-99",
#'     "35-39", "20-24", "45-49",
#'     "100+", "30-34",
#'     "10-14", "100+",
#'     "70-74", "100+", "100+",
#'     "5 9", "100+",
#'     "100+", "<4", "70-74",
#'     "100+", "95-99",
#'     "40-44", "25-29", "100+",
#'     "100+", "70-74",
#'     "75-79", "80-84",
#'     "100+", "70-74", "<4",
#'     "55-59", "100+",
#'     "20-24", "100+",
#'     "50-54", "75-79"
#'   )
#' )
#'
#' order_age_bands(data3$age_group)
#'
#' data3$age_group <- factor(x = data3$age_group, levels = order_age_bands(data3$age_group), ordered = T)
# '
# '
#' ggplot2::ggplot(data3, ggplot2::aes(x = age_group)) +
#'   ggplot2::geom_bar(stat = "count")
order_age_bands <- function(y) {
  if ((is.null(y) | !is.character(y))) {
    stop("The y parameter has not been specified or has been specified in an incorrect format.")
  }


  convert <- function(x) { # For a singular number
    if (is.na(x)) {
      return(NA)
    }
    formatted <- stringr::str_extract_all(x, "[[:digit:]]+")
    if (sum(lapply(formatted, length) %>% unlist() == 0, na.rm = T)) {
      stop("Vector supplied in parameter contains character values which do not contain numeric values
         and are not NA values.")
    }
    formatted <- formatted |> unlist()
    formatted <- as.numeric(formatted)
    return(max(formatted, na.rm = T))
  }

  y1 <- lapply(y, convert)


  y1[sapply(y1, is.null)] <- NA
  y1 <- y1 |> unlist()

  a <- data.frame(y = y, y1 = y1)
  a <- unique(a[, c("y", "y1")]) |> dplyr::arrange(y1)

  a <- a$y[order(a$y1, a$y)]
  cat(paste0("Suggested ordering as vector: ", "c('", paste(a, collapse = "','"), "')", "\n\n"))
  return(a)
}

