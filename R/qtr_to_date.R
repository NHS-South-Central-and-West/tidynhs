#' qtr_to_date
#'
#'Given text in one of the following formats this should return the start date corresponding to this quarter:
#'1. Financial Year, followed by financial quarter i.e 2022/23 Qtr 3 separated by a word (i.e quarter) less than or equal to 7 letters long.
#'Additionally, the delimiter between financial years must be / or -
#'2. Quarter, followed by financial year, separated by a space i.e Q3 22/23, where the quarter starts with a word (i.e quarter)
#' less than or equal to 7 letters long, followed by the quarter number with no space in between.
#' Additionally, the delimiter between financial years must be / or -
#'
#'
#' @param col vector to transform
#' @param Year_first True false flag based on formatting of text. If year appears first
#' in string; set Year_first=T, if quarter appears first set Year_first=F
#'
#' @return
#' @export
#'
#' @examples
#'
#' colyf<-c("22/23 Qtr1", "2022/2023 Quarter01",
#'          " 2022/23 Qtr4","2022/23 Q1", "22/23 QTR 3")
#' qtr_to_date(colyf,Year_first = T)
#' colqf<-c("Q322/23", "Q3 22/23","Q3 2022/2023","q3 22/23"
#'          ,"Quarter3 2022/23"
#' )
#' qtr_to_date(colqf,Year_first = F)
qtr_to_date<-function(col,Year_first=T){

  qtrtomonth<-function(x){
    if(is.na(x)){return(NA)}
    if(length(x)==0){return(NA)}
    if(x==1){
      return("-04-01")
    }
    if(x==2){
      return("-07-01")

    }
    if(x==3){
      return("-10-01")

    }
    if(x==4){
      return("-01-01")

    }
  }

  yytoyyyy<-function(x){
    if(is.na(x)){return(NA)}
    if(length(x)==0){return(NA)}
    if(nchar(x)==2){

      return(paste0("20",x))

    }

    if(nchar(x)==4){return(x)}

    if(nchar(x)!=2 & nchar(x)!=4){
      return(NA)
    }
  }



  if(Year_first==T){

    # Creates a vector aa which is NA where col does not meet REGEX requirements
    # This is used later within the if condition to ensure any values
    # not meeting REGEX requirements are returned as NA.

    a<- stringr::str_extract_all(col,"^\\d{2,4}[/|-]\\d{2,4}\\s[a-zA-Z]{1,7}\\s?\\d{1,2}\\b")
    a<-purrr::map(a,1)
    a[sapply(a,is.null)]<-NA
    aa<-a|>unlist()
    aa[lapply(a,length)!=1]<-NA

    if(sum(is.na(a))>0){
      warning("Some formats of FY Qtr may not be within accepted formats")
    }


    col2<-tolower(col)
    col2[is.na(aa)]<-"22/23 q1"# default value will be converted to NA at end
    col2<-strsplit(col2,"[a-zA-Z]{1,7}")
    fy<-purrr::map(col2, 1)
    q<-purrr::map(col2, 2)
    fy_split<-stringr::str_extract_all(fy,"[[:digit:]]+")
    q<-stringr::str_extract_all(q,"[[:digit:]]+")
    q<-as.numeric(q)
    q[q>4]<-NA

    q2<-lapply(q,qtrtomonth)|>unlist()

    fy_split1<-as.vector(purrr::map(fy_split, 1)|>unlist())
    fy_split1<-lapply(fy_split1,yytoyyyy)


    q2[q<4]<-paste0(fy_split1[q<4],q2[q<4])
    q2[q==4]<-paste0(as.character(as.numeric(fy_split1[q==4])+1),q2[q==4])

    q2[is.na(aa)]<-NA


    return(q2)

  }

  if(Year_first==F){

    a<-    stringr::str_extract_all(col,"^[a-zA-Z]{1,7}\\d{1,2}\\s\\d{2,4}[/|-]\\d{2,4}\\b")


    a<-purrr::map(a,1)
    a[sapply(a,is.null)]<-NA

    aa<-a|>unlist()
    aa[lapply(a,length)!=1]<-NA

    if(sum(is.na(a))>0){
      warning("Some formats of FY Qtr may not be within accepted formats")
    }




    col<-trimws(col)
    col2<-tolower(col)
    col[is.na(aa)]<-"q1 22/23"
    col<-strsplit(col,"\\s")

    for(i in 1:length(col)){
      if(is.na(col[[i]][1])){
        col[[i]][1]<-NA
      }

      if(is.na(col[[i]][2])){
        col[[i]][2]<-NA
      }
    }

    fy<-purrr::map(col, 2)

    q<-purrr::map(col, 1)
    q[is.na(nchar(fy))]<-NA


    fy_split<-stringr::str_extract_all(fy,"[[:digit:]]+")

    q<-stringr::str_extract_all(q,"[[:digit:]]+")
    q<-as.numeric(q)
    q[q>4]<-NA

    q[sapply(q,is.null)]<-NA
    q2<-lapply(q,qtrtomonth)|>unlist()
    fy_split[is.na(fy_split)]<-NA

    fy_split1<-purrr::map(fy_split, 1)
    fy_split1[sapply(fy_split1,is.null)]<-NA
    fy_split1<-lapply(fy_split1,yytoyyyy)|>unlist()

    q2[q<4&!is.na(q)]<-paste0(fy_split1[q<4&!is.na(q)],q2[q<4&!is.na(q)])
    q2[q==4&!is.na(q)]<-paste0(as.character(as.numeric(fy_split1[q==4&!is.na(q)])+1),q2[q==4&!is.na(q)])
    q2[is.na(aa)]<-NA

    return(q2)

  }






}


