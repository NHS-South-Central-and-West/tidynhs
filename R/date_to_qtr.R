#' date_to_qtr
#'
#'given a date, this should return the NHS financial year, quarter, or financial year and quarter
#'
#' @param date
#' @param type one of 3 options:
#'
#' "FYQ" returns the financial year and quarter
#' "Q" returns the quarter only
#' "FY" returns the financial year only
#'
#' @return
#' @export
#'
#' @examples
#'date_to_qtr(as.Date('2022-04-01' ,format = "%Y-%m-%d"))
#'date_to_qtr(as.Date('2022-04-01' ,format = "%Y-%m-%d"), type="Q")
#'date_to_qtr(as.Date('2022-04-01' ,format = "%Y-%m-%d"), type="FY")
#'date_to_qtr(as.Date('04-01-2022' ,format = "%d-%m-%Y"),type="FYQ")
#'date_to_qtr(as.Date('04-01-2022' ,format = "%d-%m-%Y"),type="FYQ2")

date_to_qtr<- function(date,type="FYQ"){
  if(!inherits(date,"Date")){
    warning("Please check data type of vector/object is date. NA returned")
    return(NA)}
  if(type!="FYQ"&type!="Q"&type!="FY"){
    warning("Please check parameter type is within accepted values. NA returned")
    return(NA)}

  x<-date
  xadj<-seq(lubridate::floor_date(x,"month") ,length=2,by="-3 months")[2]


  if(lubridate::month(x)>=4){
    if(type=="Q"){
      return(substr(zoo::as.yearqtr(xadj,format = "%Y-%m-%d"),6,7))
    }
    if(type=="FY"){
      return(paste0(substr(as.character(lubridate::year(x)),1,4),"/",
                    as.character(as.numeric(
                      substr(as.character(lubridate::year(x)),3,4))+1)))
    }

    if(type=="FYQ"){
      return(
        paste0(substr(as.character(lubridate::year(x)),1,4),"/",
               as.character(as.numeric(
                 substr(as.character(lubridate::year(x)),3,4))+1),
               " ", substr(zoo::as.yearqtr(xadj,format = "%Y-%m-%d"),6,7)

        )
      )}



  }else{if(lubridate::month(x)<4){

    if(type=="Q"){return(substr(zoo::as.yearqtr(xadj,format = "%Y-%m-%d"),6,7))}
    if(type=="FY"){return(  paste0(substr(as.character(lubridate::year(xadj)),1,4),"/",
                                   as.character(as.numeric(
                                     substr(as.character(lubridate::year(xadj)),3,4))+1)))}
    if(type=="FYQ"){
      return(
        paste0(substr(as.character(lubridate::year(xadj)),1,4),"/",
               as.character(as.numeric(
                 substr(as.character(lubridate::year(xadj)),3,4))+1),
               " ", substr(zoo::as.yearqtr(xadj,format = "%Y-%m-%d"),6,7)

        ))}}}


}

