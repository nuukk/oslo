googletrendscrawling <- function(keyword,geo,date) 
{
  geo <- toupper(geo)
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
  setDT(res)
  res
}

google_trends_scapper <- function(start_date,end_date,keyword,country,new_name,capture_path=NULL) {
  if(missing(start_date)) start_date <- '2017-12-01'
  if(missing(end_date)) end_date <- Sys.Date()-days(4)
  keyword <- gsub(" ","%20",keyword)
  country <- toupper(country)
  url <- paste0('https://trends.google.com/trends/explore?date=',start_date,'%20',end_date,'&geo=',country,'&q=',keyword)
  Sys.sleep(1)
  remDr$navigate(url)
  Sys.sleep(1.5)
  try({
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
                                    pattern='multiTimeline',full.names=T), ~ data.table(file=.x,time=file.mtime(.x))) %>%
                  rbindlist %>% slice_max(time,n=1L) %>% pull(file),
                to=file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads",paste0(new_name,'.csv')))
    if(!is.null(capture_path)) {
      remDr$screenshot(file=file.path(capture_path,paste0(new_name,'.jpg')))
    }
  })
}

google_trends_read <- function(file_list) {
  if(missing(file_list)) file_list <- choose.files(caption='Google Trends RAW 파일을 선택하세요')
  if(dir.exists(file_list[[1]])) file_list <- list.files(file_list,full.names=T)
  file_list <- normalizePath(file_list)
  map(file_list, ~ {
    res <- fread(.x,encoding='UTF-8',header=F)
    if(nrow(res)>=2) {
      for(i in seq_along(names(res)[-1])) {
        res[[names(res)[-1][i]]][1] <- gsub(": \\(.*","",res[[names(res)[-1][i]]][1])
      }
      res <- res %>% melt(id='V1')
      n <- data.table(start=which(str_detect(res$V1,'주'))) %>% mutate(end=lead(start,n=1L,default=nrow(res)+1)-1)
      res[,keyword:=NA]
      for(i in seq_len(nrow(n))) {
        res$keyword[n$start[i]:n$end[i]] <- res$value[n$start[i]]
      }
      res <- res[V1!='주']
      res[,`:=`(google_code=gsub("\\+.*","",gsub(".*s-","",basename(.x))),
                geo=toupper(gsub(".*=|.csv$","",basename(.x))),
                gbm=gsub("\\+.*","",basename(.x)))]
      res <- res[,.(date=V1,geo,gbm,keyword,hits=value)]
    } else {
      for(i in seq_along(names(res)[-1])) {
        res[[names(res)[-1][i]]][1] <- gsub(": \\(.*","",res[[names(res)[-1][i]]][1])
      }
      res <- bind_rows(res,data.table(NA))
      res <- res %>% melt(id='V1')
      n <- data.table(start=which(str_detect(res$V1,'주'))) %>% mutate(end=lead(start,n=1L,default=nrow(res)+1)-1)
      res[,keyword:=NA]
      for(i in seq_len(nrow(n))) {
        res$keyword[n$start[i]:n$end[i]] <- res$value[n$start[i]]
      }
      res <- res[!is.na(V1) | V1!='주']
      res[,`:=`(google_code=gsub("\\+.*","",gsub(".*s-","",basename(.x))),
                geo=toupper(gsub(".*=|.csv$","",basename(.x))),
                gbm=gsub("\\+.*","",basename(.x)))]
      res <- res[,.(date=V1,geo,gbm,keyword,hits=value)]
    }
  }) %>% rbindlist -> res
  res
}
