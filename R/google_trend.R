googletrendscrawling <- function(keyword,geo,date) 
{
  if(missing(date)) date <- paste("2015-01-01", floor_date(Sys.Date(), "mon")-days(1))
  res <- tryCatch(gtrends(keyword=keyword,geo=geo,time=date,onlyInterest = TRUE)$interest_over_time,
                  error = function(e) {})
  if(is.null(res)) {
    return(data.table(date = as.POSIXct("1900-01-02 GMT"), 
                      hits = as.character(-100), keyword=keyword,geo=geo, 
                      time = as.character("1900-01-01 1900-01-01"), gprop = as.character("NA"), 
                      category = as.integer(0)))
  }
  res$date <- as.POSIXct(res$date)
  res$hits <- as.character(res$hits)
  res$time <- as.character(res$time)
  res
}
