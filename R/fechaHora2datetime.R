#' Title
#'
#' @param calendardate 
#' @param hora 
#'
#' @return
#' @export
#'
#' @examples
fechaHora2datetime=function(calendardate,hora){
  ymd(calendardate,tz = "Europe/Madrid")+dhours(as.numeric(hora))
}
