gsc_month_page_keyword <- function(target_list,start_date,end_date,export_directory,delay_time=2) {
  if(missing(export_directory)) export_directory <- choose.dir(caption='저장할 폴더를 선택하세요')
  stopifnot(!missing(start_date),!missing(end_date))
  if(missing(target_list)) target_list <- c('Global','US','EMEA','KR','CN','JP',
                                            'DS 웹사이트 영문','미니사이트 SSD 영문','미니사이트 Exynos 영문','미니사이트 ISOCELL 영문',
                                            'DS 웹사이트 국문','DS 웹사이트 중문','미니사이트 Exynos 중문','미니사이트 ISOCELL 중문','미니사이트 SSD 일문')
  target_all <- list(c('Global','US','EMEA','KR','CN','JP',
                       'DS 웹사이트 영문','미니사이트 SSD 영문','미니사이트 Exynos 영문','미니사이트 ISOCELL 영문',
                       'DS 웹사이트 국문','DS 웹사이트 중문','미니사이트 Exynos 중문','미니사이트 ISOCELL 중문','미니사이트 SSD 일문'),
                     c('https://semiconductor.samsung.com/','https://semiconductor.samsung.com/us/','https://semiconductor.samsung.com/emea/',
                       'https://semiconductor.samsung.com/kr/','https://semiconductor.samsung.com/cn/','https://semiconductor.samsung.com/jp/',
                       'https://www.samsung.com/semiconductor/','https://www.samsung.com/semiconductor/','https://www.samsung.com/semiconductor/','https://www.samsung.com/semiconductor/',
                       'https://www.samsung.com/semiconductor/kr/','https://www.samsung.com/semiconductor/cn/','https://www.samsung.com/semiconductor/','https://www.samsung.com/semiconductor/','https://www.samsung.com/semiconductor/'),
                     c('page !~ /us/ & page !~ /emea/ & page !~ /kr/ & page !~ /cn/ & page !~ /jp/','','','','','',
                       'page !~ /cn/ & page !~ /kr/ & page !~ /semiconductor/minisite/',
                       'page ~~ /semiconductor/minisite/ssd/',
                       'page ~~ /semiconductor/minisite/exynos/ & page !~ /semiconductor/minisite/exynos/cn/',
                       'page ~~ /semiconductor/minisite/isocell/ & page !~ /semiconductor/minisite/isocell/cn/ ','','',
                       'page ~~ /semiconductor/minisite/exynos/cn/','page ~~ /semiconductor/minisite/isocell/cn/',
                       'page ~~ /semiconductor/minisite/jp/'))
  
  ns <- which(toupper(gsub(" ","",target_all[[1]])) %chin% toupper(gsub(" ","",target_list)))
  target <- list(target_all[[1]][ns],target_all[[2]][ns],target_all[[3]][ns])
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  dates <- data.table(start=seq.Date(start_date,end_date,by='months'),end=ceiling_date(seq.Date(start_date,end_date,by='months'),'months')-days(1))
  scr_auth()
  pmap(target,
       function(type,url,filter) {
         sub_dir <- ifelse(str_detect(type,'[가-힣]')==TRUE,'P5','P6')
         if(filter=='') {
           filter <- NULL
         } else if(grepl('&',filter)) {
           filter <- c(str_split(filter,'&',simplify=T))
         }
         suppressWarnings(map(c('P5','P6'), ~ if(!dir.exists(normalizePath(paste0(export_directory,"/",.x)))) dir.create(normalizePath(paste0(export_directory,"/",.x)))))
         message(type,'추출중...')
         Sys.sleep(delay_time*2)
         for(i in seq_len(nrow(dates))) {
           tryCatch(search_analytics(
             siteURL=url,
             startDate=dates$start[[i]],
             endDate=dates$end[[i]],
             dimensions = c('page','query'),
             dimensionFilterExp=filter,
             rowLimit = 600000,
             walk_data = "byBatch"),
             error=function(e) {
               data.table(page=NA,query=NA,clicks=NA,impressions=NA,ctr=NA,position=NA)
             }) %>% save_csv(filename=paste0(type,'-',dates$start[[i]],'-',dates$end[[i]]),
                             dir=normalizePath(paste0(export_directory,"/",sub_dir)))
           Sys.sleep(delay_time)
           message(type,'추출 완료')
         }
       })
}
