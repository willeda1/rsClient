#' returns the current time with milliseconds
#'
#' Returns the current time in millisections for the UTC (GMT) time zone. Note
#' that the returned value is a character format for which order relations are
#' defined (including `max()` and `min()`) but not further arithmetic functions
#' (e.g. `mean()`).
#'
#' A final Z for `Zulu` is added to stop RDF coercing the output to a (truncated)
#' date format. Use `short=T` or `time.only=T` for a short form.
#'
#' @param spaces false, all spaces are replaced by underscores
#' @param short if true, simply reports the hours, minutes and seconds in UTC
#'    preceded by the day of the week
#' @param time.only if true, the date is suppressed and just the time printed
#'
#' @return a character string with the current time in UTC
#' @export
#' @md
#'
#' @examples
#' a1=Zulu()
#' a1
#'
#' a2=Zulu()
#' a2
#'
#' a1 < a2
#' a1 > a2
#'
#' class(a1)
#'
#' Zulu(F)
#' Zulu(short=T)
#' Zulu(time,only=T)


Zulu=function(spaces=T,short=F,time.only=F){

  if (!short & !time.only){
      time=format(as.POSIXct(Sys.time()),
              "%Y-%m-%d %H:%M:%OS6 Z", tz="UTC")
  } else {
    if (time.only){
      time=format(as.POSIXct(Sys.time()),
                  "%H:%M:%OS6 Z", tz="UTC")
    } else {
      time=format(as.POSIXct(Sys.time()),
                  "%a_%H-%M-%S", tz="UTC")
    }

  }


  if (spaces){
    time
  } else {
    gsub("\\s","_",time)
  }
}

