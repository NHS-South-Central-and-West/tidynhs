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
#' @return
#' @export
#'
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
