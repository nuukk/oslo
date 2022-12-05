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

google_trends_scapper <- function(start_date,end_date,keyword,country,new_name) {
  if(missing(start_date)) start_date <- '2017-12-01'
  if(missing(end_date)) end_date <- Sys.Date()-days(4)
  keyword <- gsub(" ","%20",keyword)
  country <- toupper(country)
  url <- paste0('https://trends.google.com/trends/explore?date=',start_date,'%20',end_date,'&geo=',country,'&q=',keyword)
  Sys.sleep(1)
  remDr$navigate(url)
  Sys.sleep(1.5)
  tryCatch({
    remDr$findElement(using='class name',value='widget-actions-item')$clickElement()
  }, error=function(e) {
    Sys.sleep(1.5)
    remDr$findElement(using='class name',value='widget-actions-item')$clickElement()
  })
  #rename
  Sys.sleep(1)
  is_finish <- length(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                 pattern='multiTimeline.csv'))
  while(is_finish==0) {
    Sys.sleep(0.5)
    is_finish <- length(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                   pattern='multiTimeline.csv'))
  }
  Sys.sleep(0.8)
  file.rename(from=map(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                  pattern='multiTimeline.csv',full.names=T), ~ data.table(file=.x,time=file.mtime(.x))) %>%
                rbindlist %>% slice_max(time,n=1L) %>% pull(file),
              to=file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads",paste0(new_name,'.csv')))
}
