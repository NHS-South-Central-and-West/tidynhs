#' generate_age_groups
#'
#'Prints out suggested dplyr code to generate age groups. This is intended to serve as a guide only
#'and therefore code generated should be reviewed and modified as necessary.
#'
#' @param col Name of age column to transform
#' @param age_bands Age groups to create
#' @param common_age_bands  Default selection of age bands. When this is null, age bands are generated
#' as per age_bands parameter, otherwise this overrides age_band parameter
#'
#'1 = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
#'"50-54","55-59","60-64","65+")
#'
#'2 = c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
#'"50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100+")
#'
#'3 = c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+")
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' data<-data.frame(
#'   Age = c(5L,110L,87L,108L,57L,168L,133L,62L,
#'           125L,140L,38L,168L,159L,155L,98L,37L,24L,48L,141L,
#'           34L,14L,129L,70L,121L,102L,8L,113L,113L,3L,71L,111L,
#'           95L,41L,26L,170L,160L,73L,76L,84L,106L,73L,1L,58L,
#'           190L,22L,118L,52L,79L),
#'   ID = c(2251558L,2540739L,2121923L,268210L,
#'          1695141L,103196L,485232L,2431101L,1565663L,1687369L,762799L,
#'          293488L,1476981L,1764643L,2601051L,463056L,867640L,
#'          426493L,1424699L,1166167L,1314136L,2079697L,1230127L,1055453L,
#'          2529562L,909298L,831791L,2346930L,2341395L,573434L,
#'          1669739L,1501823L,590632L,186521L,495621L,1007120L,1404587L,
#'          626912L,2593705L,2148515L,1051717L,1540477L,1337032L,
#'          1694720L,101417L,1592089L,2065567L,93434L),
#'   Value = c(3L,7L,10L,10L,6L,10L,8L,4L,1L,3L,
#'             10L,4L,2L,2L,8L,7L,1L,7L,1L,10L,3L,9L,6L,8L,4L,
#'             5L,1L,1L,10L,2L,3L,8L,4L,8L,5L,4L,5L,9L,8L,4L,8L,
#'             7L,4L,2L,7L,6L,8L,2L)
#' )
#'
#' age_bands2<- c("<5","5-9","10-15","20-99","100-119","120+")
#'
#'generate_age_groups("Age",age_bands2)
#' generate_age_groups("Age",age_bands2,2)
#'
#'
generate_age_groups<-function(col="Age",age_bands,common_age_bands=4){

  if(common_age_bands==1){age_bands<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65+")}
  if(common_age_bands==2){age_bands<-c("0-4","5-9","10-14","15-19","20-24","25-29","30-34","35-39","40-44","45-49",
                                       "50-54","55-59","60-64","65-69","70-74","75-79","80-84","85-89","90-94","95-99","100+")}

  if(common_age_bands==3){age_bands<-c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80-89","90-99","100+")}

  age_bands_formatted<-stringr::str_extract_all(age_bands,"[[:digit:]]+")

  string=paste0("mutate( \n","age_group = dplyr::case_when( \n")


  for(i in (1:length(age_bands))){

    if (length(as.vector(age_bands_formatted[[i]]))==1 & i==1){

      string<-paste0(string,paste0(col,"<", as.numeric(age_bands_formatted[[i]]),"   ~","'",age_bands[i]),"' ,","\n")

    }
    if (length(as.vector(age_bands_formatted[[i]]))==1 & i!=1){

      string<-paste0(string,paste0(col,">", as.numeric(age_bands_formatted[[i]]),"   ~","'",age_bands[i]),"'","\n")

    }

    if(length(as.vector(age_bands_formatted[[i]]))==2 &i!=length(age_bands_formatted) ){

      paste0(col,"<=", as.numeric(as.vector(age_bands_formatted[[i]][2])),"   ~","'",age_bands[i])
      string<-paste0(string,paste0(col,"<=", as.numeric(as.vector(age_bands_formatted[[i]][2])),"   ~","'",age_bands[i]),"' ,","\n")
    }
    if(length(as.vector(age_bands_formatted[[i]]))==2 &i==length(age_bands_formatted) ){

      paste0(col,"<=", as.numeric(as.vector(age_bands_formatted[[i]][2])),"   ~","'",age_bands[i])
      string<-paste0(string,paste0(col,"<=", as.numeric(as.vector(age_bands_formatted[[i]][2])),"   ~","'",age_bands[i]),"'","\n")
    }


  }

  return(cat(paste0(string,"))")))



}
