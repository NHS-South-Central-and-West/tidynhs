#' Transform Dates to Quarters
#'
#' Given a date, this will return the NHS financial year, quarter, or financial year and quarter
#'
#' @param date This parameter is the date to be transformed. This must be a date object otherwise
#' the function will return an error. The function will not accept a vector of dates. For use with
#' dplyr syntax, the code will need to be written along the lines of
#' data%>%mutate(Quarter= vapply(date,date_to_qtr,"Q")).
#' @param type The default values "FYQ" returns the financial year and quarter
#' in the format YYYY/YY QX. This parameter also provides additional options in how
#' the output is returned.
#'
#' If this is specified as "Q": only the quarter is returned in the format "QX"
#' If this is specified as "FY": only the financial year is returned in the format "YYYY/YY".
#'
#' @return A character string returning the financial year and/or quarter
#' @export
#'@seealso [qtr_to_date()] for financial Year and quarter to date conversion
#' @examples
#' date_to_qtr(as.Date("2022-04-01", format = "%Y-%m-%d"))
#' date_to_qtr(as.Date("2022-04-01", format = "%Y-%m-%d"), type = "Q")
#' date_to_qtr(as.Date("2022-04-01", format = "%Y-%m-%d"), type = "FY")
#' date_to_qtr(as.Date("04-01-2022", format = "%d-%m-%Y"), type = "FYQ")
date_to_qtr <- function(date, type = "FYQ") {
  if (!inherits(date, "Date")) {
    stop("Please check data type of parameter 'date' is date.")
  }
  if (type != "FYQ" & type != "Q" & type != "FY") {
    stop("Please check parameter 'type' is within accepted values")
  }

  x <- date
  xadj <- seq(lubridate::floor_date(x, "month"), length = 2, by = "-3 months")[2]


  if (lubridate::month(x) >= 4) {
    if (type == "Q") {
      return(substr(zoo::as.yearqtr(xadj, format = "%Y-%m-%d"), 6, 7))
    }
    if (type == "FY") {
      return(paste0(
        substr(as.character(lubridate::year(x)), 1, 4), "/",
        as.character(as.numeric(
          substr(as.character(lubridate::year(x)), 3, 4)
        ) + 1)
      ))
    }

    if (type == "FYQ") {
      return(
        paste0(
          substr(as.character(lubridate::year(x)), 1, 4), "/",
          as.character(as.numeric(
            substr(as.character(lubridate::year(x)), 3, 4)
          ) + 1),
          " ", substr(zoo::as.yearqtr(xadj, format = "%Y-%m-%d"), 6, 7)
        )
      )
    }
  } else {
    if (lubridate::month(x) < 4) {
      if (type == "Q") {
        return(substr(zoo::as.yearqtr(xadj, format = "%Y-%m-%d"), 6, 7))
      }
      if (type == "FY") {
        return(paste0(
          substr(as.character(lubridate::year(xadj)), 1, 4), "/",
          as.character(as.numeric(
            substr(as.character(lubridate::year(xadj)), 3, 4)
          ) + 1)
        ))
      }
      if (type == "FYQ") {
        return(
          paste0(
            substr(as.character(lubridate::year(xadj)), 1, 4), "/",
            as.character(as.numeric(
              substr(as.character(lubridate::year(xadj)), 3, 4)
            ) + 1),
            " ", substr(zoo::as.yearqtr(xadj, format = "%Y-%m-%d"), 6, 7)
          )
        )
      }
    }
  }
}


#' Transform Quarters to Dates
#'
#'@description
#' Given text in one of the following formats this should return the start date corresponding to this quarter:
#'
#' 1. Financial Year, followed by financial quarter i.e 2022/23 Qtr 3 separated by a word (i.e quarter) less than or equal to 7 letters long.
#' Additionally, the delimiter between financial years must be / or -.
#'
#' 2. Quarter, followed by financial year, separated by a space i.e Q3 22/23, where the quarter starts with a word (i.e quarter)
#' less than or equal to 7 letters long, followed by the quarter number with no space in between.
#' Additionally, the delimiter between financial years must be / or -.
#'
#'
#' @param col Vector to transform. The vector values should be a character string containing both the Quarter
#' and financial year in the format specified in the description. If a vector value is not in a format  accepted by the function
#' a NA will be return with a corresponding warning message.
#' @param Year_first The default value of TRUE assumes that the parameter col contains character values
#' which specify the financial year before the quarter. If, instead, the quarter is specified first, this
#' should set to FALSE.
#'
#' @return A date value or vector returning the start date of the input financial year and quarter
#' @export
#'@seealso [date_to_qtr()] for date to Financial year and/or Quarter conversion
#' @examples
#'
#' colyf <- c(
#'   "22/23 Qtr1", "2022/2023 Quarter01",
#'   " 2022/23 Qtr4", "2022/23 Q1", "22/23 QTR 3"
#' )
#' qtr_to_date(colyf, Year_first = T)
#' colqf <- c(
#'   "Q322/23", "Q3 22/23", "Q3 2022/2023", "q3 22/23",
#'   "Quarter3 2022/23"
#' )
#' qtr_to_date(colqf, Year_first = F)
qtr_to_date <- function(col, Year_first = T) {
  if (Year_first != T & Year_first != F) {
    stop("Parameter Year first is not a boolean TRUE/FALSE value")
  }


  qtrtomonth <- function(x) {
    if (is.na(x)) {
      return(NA)
    }
    if (length(x) == 0) {
      return(NA)
    }
    if (x == 1) {
      return("-04-01")
    }
    if (x == 2) {
      return("-07-01")
    }
    if (x == 3) {
      return("-10-01")
    }
    if (x == 4) {
      return("-01-01")
    }
  }

  yytoyyyy <- function(x) {
    if (is.na(x)) {
      return(NA)
    }
    if (length(x) == 0) {
      return(NA)
    }
    if (nchar(x) == 2) {
      return(paste0("20", x))
    }

    if (nchar(x) == 4) {
      return(x)
    }

    if (nchar(x) != 2 & nchar(x) != 4) {
      return(NA)
    }
  }



  if (Year_first == T) {
    # Creates a vector aa which is NA where col does not meet REGEX requirements
    # This is used later within the if condition to ensure any values
    # not meeting REGEX requirements are returned as NA.

    a <- stringr::str_extract_all(col, "^\\d{2,4}[/|-]\\d{2,4}\\s[a-zA-Z]{1,7}\\s?\\d{1,2}\\b")
    a <- purrr::map(a, 1)
    a[sapply(a, is.null)] <- NA
    aa <- a |> unlist()
    aa[lapply(a, length) != 1] <- NA

    if (sum(is.na(a)) > 0) {
      warning("Some formats of FY Qtr may not be within accepted formats. These will be coerced to NA.")
    }


    col2 <- tolower(col)
    col2[is.na(aa)] <- "22/23 q1" # default value will be converted to NA at end
    col2 <- strsplit(col2, "[a-zA-Z]{1,7}")
    fy <- purrr::map(col2, 1)
    q <- purrr::map(col2, 2)
    fy_split <- stringr::str_extract_all(fy, "[[:digit:]]+")
    q <- stringr::str_extract_all(q, "[[:digit:]]+")
    q <- as.numeric(q)
    if (length(q[q != 1 & q != 2 & q != 3 & q != 4 & !is.na(q)]) > 0) {
      warning("Quarter value is not within accepted values of 1,2,3 or 4. Function will return NA for this value.")
    }

    q[q != 1 & q != 2 & q != 3 & q != 4] <- NA

    q2 <- lapply(q, qtrtomonth) |> unlist()

    fy_split1 <- as.vector(purrr::map(fy_split, 1) |> unlist())
    fy_split1 <- lapply(fy_split1, yytoyyyy)


    q2[q < 4 & !is.na(q)] <- paste0(fy_split1[q < 4], q2[q < 4])
    q2[q == 4] <- paste0(as.character(as.numeric(fy_split1[q == 4]) + 1), q2[q == 4])

    q2[is.na(aa)] <- NA


    return(q2)
  }

  if (Year_first == F) {
    a <- stringr::str_extract_all(col, "^[a-zA-Z]{1,7}\\d{1,2}\\s\\d{2,4}[/|-]\\d{2,4}\\b")


    a <- purrr::map(a, 1)
    a[sapply(a, is.null)] <- NA

    aa <- a |> unlist()
    aa[lapply(a, length) != 1] <- NA

    if (sum(is.na(a)) > 0) {
      warning("Some formats of FY Qtr may not be within accepted formats. These will be coerced to NA.")
    }




    col <- trimws(col)
    col2 <- tolower(col)
    col[is.na(aa)] <- "q1 22/23"
    col <- strsplit(col, "\\s")

    for (i in 1:length(col)) {
      if (is.na(col[[i]][1])) {
        col[[i]][1] <- NA
      }

      if (is.na(col[[i]][2])) {
        col[[i]][2] <- NA
      }
    }

    fy <- purrr::map(col, 2)

    q <- purrr::map(col, 1)
    q[is.na(nchar(fy))] <- NA


    fy_split <- stringr::str_extract_all(fy, "[[:digit:]]+")

    q <- stringr::str_extract_all(q, "[[:digit:]]+")
    q <- as.numeric(q)
    if (length(q[q != 1 & q != 2 & q != 3 & q != 4 & !is.na(q)]) > 0) {
      warning("Quarter value is not within accepted values of 1,2,3 or 4. Function will return NA for this value.")
    }

    q[q != 1 & q != 2 & q != 3 & q != 4] <- NA

    q[sapply(q, is.null)] <- NA
    q2 <- lapply(q, qtrtomonth) |> unlist()
    fy_split[is.na(fy_split)] <- NA

    fy_split1 <- purrr::map(fy_split, 1)
    fy_split1[sapply(fy_split1, is.null)] <- NA
    fy_split1 <- lapply(fy_split1, yytoyyyy) |> unlist()

    q2[q < 4 & !is.na(q)] <- paste0(fy_split1[q < 4 & !is.na(q)], q2[q < 4 & !is.na(q)])
    q2[q == 4 & !is.na(q)] <- paste0(as.character(as.numeric(fy_split1[q == 4 & !is.na(q)]) + 1), q2[q == 4 & !is.na(q)])
    q2[is.na(aa)] <- NA

    return(q2)
  }
}
