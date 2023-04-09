gkp_scrapper <- function(start_date,end_date,keyword,country,lang='English',new_name,print=FALSE,capture_path=NULL) {
  lang <- match.arg(lang,choices=c('Arabic','Bengali','Bulgarian','Catalan','Chinese (simplified)','Chinese (traditional)','Croatian','Czech','Danish','Dutch','English','Estonian','Filipino','Finnish','French','German','Greek','Gujarati','Hebrew','Hindi','Hungarian','Icelandic','Indonesian','Italian','Japanese','Kannada','Korean','Latvian','Lithuanian','Malay','Malayalam','Marathi','Norwegian','Persian','Polish','Portuguese','Punjabi','Romanian','Russian','Serbian','Slovak','Slovenian','Spanish','Swedish','Tamil','Telugu','Thai','Turkish','Ukrainian','Urdu','Vietnamese'))
  Sys.sleep(1+runif(n=1,min=0.25,max=0.5)+abs(rnorm(n=1,mean=0.25,sd=0.25)))
  if(!missing(start_date)) {
    start_date <- paste0(month(as.Date(start_date),label=T,locale='US'),year(start_date))
    remDr$findElement(using='class name',value='date-popup-button')$clickElement()
    remDr$findElement(using='css',value='.start')$findChildElement(using='css',value='.baseline')$findChildElement(using='css',value='.top-section')$findChildElement(using='css',value='.input')$clearElement()
    remDr$findElement(using='css',value='.start')$sendKeysToElement(list(start_date))
    remDr$findElement(using='css',value='.start')$sendKeysToElement(list(key='enter'))
    remDr$findElement(using='css',value='.apply-bar')$findChildElement(using='css',value='.apply')$clickElement()
    Sys.sleep(0.35+runif(n=1,min=0.15,max=0.5))
  }
  if(!missing(end_date)) {
    end_date <- paste0(month(as.Date(end_date),label=T,locale='US'),year(end_date))
    remDr$findElement(using='class name',value='date-popup-button')$clickElement()
    remDr$findElement(using='css',value='.end')$clickElement()
    remDr$findElement(using='css',value='.end')$findChildElement(using='css',value='.baseline')$findChildElement(using='css',value='.top-section')$findChildElement(using='css',value='.input')$clearElement()
    remDr$findElement(using='css',value='.end')$sendKeysToElement(list(end_date))
    remDr$findElement(using='css',value='.end')$sendKeysToElement(list(key='enter'))
    remDr$findElement(using='css',value='.apply-bar')$findChildElement(using='css',value='.apply')$clickElement()
    Sys.sleep(0.35+runif(n=1,min=0.15,max=0.5))
  }
  ##location
  x <- remDr$getPageSource()[[1]]
  old_country <- ((read_html(x) %>% html_elements('.settings-bar') %>% html_children)[[1]] %>% html_children)[[2]] %>% html_text2
  # old_country <- ifelse(str_detect(gsub(" ","",tolower(old_country)),'hongkong'),'hong kong region',old_country)
  if(old_country!=country) {
    remDr$findElement(using='class name',value='location-button')$clickElement() #remDr$findElement(using='css',value='.settings-bar>.location-button')$clickElement()
    old_country0 <- map(seq_len(length(remDr$findElements(using='class name',value='target-description'))), ~ remDr$findElements(using='class name',value='target-description')[[.x]]$getElementText()[[1]]) %>% as.character %>% gsub("\ncountry","",.)
    country <- str_split(country,',')[[1]]
    if(isTRUE(all.equal(sort(gsub(" ","",country)),sort(gsub(" ","",old_country0))))) {
      remDr$findElements(using='css',value='.btn-no')[[3]]$clickElement()
    } else {
      remDr$findElement(using='css',value='[aria-label="Remove all targeted locations"]')$clickElement() #기존 설정된 모든 location 제거
      for(i in seq_along(country)) {
        remDr$findElement(using='class name',value='suggest-input')$clickElement() #enter a location to target
        remDr$findElement(using='class name',value='suggest-input')$sendKeysToElement(list(country[[i]])) #location 입력
        Sys.sleep(2)
        remDr$findElement(using='class name',value='suggestion-item')$clickElement() #enter
      }
      remDr$findElements(using='class name',value='btn-yes')[[3]]$clickElement() #save
    }
  }
  #language
  Sys.sleep(0.35)
  remDr$findElement(using='class name',value='language-button')$clickElement()
  remDr$findElement(using='css',value="[aria-label='Search languages']")$sendKeysToElement(list(lang))
  
  remDr$findElements(using='class name',value='input-container')[[7]]$sendKeysToElement(list(lang)) #input langua
  Sys.sleep(1.5)
  remDr$findElement(using='class name',value='dynamic-item')$clickElement() #enter 또는 value='list-item'
  
  #keyword
  Sys.sleep(0.35)
  remDr$findElement(using='class name',value='summary')$clickElement()
  Sys.sleep(0.5+runif(n=1,min=0.15,max=0.5))
  keyword_delete_n <- length(remDr$findElements(using='class name',value='delete-button'))
  while(keyword_delete_n>=1) {
    remDr$findElements(using='class name',value='delete-button')[[keyword_delete_n]]$clickElement() 
    keyword_delete_n <- keyword_delete_n-1
    Sys.sleep(0.15)
  } #기존 입력된 키워드 모두 삭제
  
  remDr$findElements(using='class name',value='search-input')[[2]]$sendKeysToElement(list(keyword))
  Sys.sleep(1.5)
  remDr$findElements(using='class name',value='search-input')[[2]]$sendKeysToElement(list(key='enter'))
  remDr$findElement(using='class name',value='submit-button')$clickElement() #get results button
  
  #save (1)
  Sys.sleep(runif(n=1,min=1,max=2.5))
  remDr$findElement(using='css',value='.option-container')$clickElement()
  Sys.sleep(0.75)
  remDr$findElement(using='class name',value='menu-item-label-section')$clickElement()
  
  #rename
  Sys.sleep(5)
  
  is_finish <- length(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                 pattern=paste0('Keyword Stats ',Sys.Date())))
  while(is_finish==0) {
    Sys.sleep(1.5)
    is_finish <- length(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                   pattern=paste0('Keyword Stats ',Sys.Date())))
  }
  Sys.sleep(1)
  file.rename(from=map(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                  pattern=paste0('Keyword Stats ',Sys.Date()),full.names=T), ~ data.table(file=.x,time=file.mtime(.x))) %>%
                rbindlist %>% slice_max(time,n=1L) %>% pull(file),
              to=file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads",paste0(new_name,'.csv')))
  Sys.sleep(1)
  if(print==TRUE) { print(paste0(country,' - ',start_date,'~',end_date,'(',lang,') 추출 완료')) }
  if(!is.null(capture_path)) {
    remDr$screenshot(file=file.path(capture_path,paste0(new_name,'.jpg')))
  }
}

gkp_scrapper2 <- function(start_date,end_date,keyword,country,new_name) {
  Sys.sleep(1+runif(n=1,min=0.25,max=0.5)+abs(rnorm(n=1,mean=0.25,sd=0.25)))
  remDr$navigate('https://ads.google.com/aw/keywordplanner/ideas/new?ocid=143996665&euid=556090245&__u=2335343005&uscid=769929296&__c=6457252304&authuser=1&subid=kr-ko-ha-aw-bk-a-m00%21o3~EAIaIQobChMIl7-T69_O9wIVwrWWCh2nvQGGEAAYASAAEgLjz_D_BwE~81876026093~kwd-324689985256~7918679677~434634269425')
  Sys.sleep(8)
  remDr$findElements(using='class name',value='visible-content')[[2]]$clickElement()
  remDr$findElement(using='css',value='.textarea')$sendKeysToElement(list(keyword))
  remDr$findElement(using='css',value='.submit-button')$clickElement()
  
  if(!missing(start_date)) {
    start_date <- paste0(month(as.Date(start_date),label=T,locale='US'),year(start_date))
    remDr$findElement(using='class name',value='date-popup-button')$clickElement()
    remDr$findElement(using='css',value='.start')$findChildElement(using='css',value='.baseline')$findChildElement(using='css',value='.top-section')$findChildElement(using='css',value='.input')$clearElement()
    remDr$findElement(using='css',value='.start')$sendKeysToElement(list(start_date))
    remDr$findElement(using='css',value='.start')$sendKeysToElement(list(key='enter'))
    remDr$findElement(using='css',value='.apply-bar')$findChildElement(using='css',value='.apply')$clickElement()
    Sys.sleep(0.35+runif(n=1,min=0.15,max=0.5))
  }
  if(!missing(end_date)) {
    end_date <- paste0(month(as.Date(end_date),label=T,locale='US'),year(end_date))
    remDr$findElement(using='class name',value='date-popup-button')$clickElement()
    remDr$findElement(using='css',value='.end')$clickElement()
    remDr$findElement(using='css',value='.end')$findChildElement(using='css',value='.baseline')$findChildElement(using='css',value='.top-section')$findChildElement(using='css',value='.input')$clearElement()
    remDr$findElement(using='css',value='.end')$sendKeysToElement(list(end_date))
    remDr$findElement(using='css',value='.end')$sendKeysToElement(list(key='enter'))
    remDr$findElement(using='css',value='.apply-bar')$findChildElement(using='css',value='.apply')$clickElement()
    Sys.sleep(0.35+runif(n=1,min=0.15,max=0.5))
  }
  ##location
  x <- remDr$getPageSource()[[1]]
  country <- ifelse(gsub(" ","",tolower(country))=='hongkong','hong kong region',country)
  old_country <- ((read_html(x) %>% html_elements('.settings-bar') %>% html_children)[[1]] %>% html_children)[[2]] %>% html_text2
  old_country <- ifelse(str_detect(gsub(" ","",tolower(old_country)),'hongkong'),'hong kong region',
                        ifelse(str_detect(gsub(" ","",tolower(old_country)),'korea'),'korea',old_country))
  if(old_country!=country) {
    remDr$findElement(using='class name',value='location-button')$clickElement() #remDr$findElement(using='css',value='.settings-bar>.location-button')$clickElement()
    remDr$findElement(using='class name',value='remove')$clickElement() #기존 설정된 location 제거
    remDr$findElement(using='class name',value='suggest-input')$clickElement() #enter a location to target
    remDr$findElement(using='class name',value='suggest-input')$sendKeysToElement(list(country)) #location 입력
    Sys.sleep(2)
    remDr$findElement(using='class name',value='suggestion-item')$clickElement() #enter
    remDr$findElements(using='class name',value='btn-yes')[[11]]$clickElement() #save
  }

  #save (1)
  Sys.sleep(runif(n=1,min=1,max=2.5))
  remDr$findElements(using='class name',value='action-button')[[2]]$clickElement()
  Sys.sleep(0.75)
  remDr$findElement(using='class name',value='menu-item-label-section')$clickElement()
  
  #rename
  Sys.sleep(5)
  
  is_finish <- length(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                 pattern=paste0('Keyword Stats ',Sys.Date())))
  while(is_finish==0) {
    Sys.sleep(1.5)
    is_finish <- length(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                   pattern=paste0('Keyword Stats ',Sys.Date())))
  }
  Sys.sleep(1)
  file.rename(from=map(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                  pattern=paste0('Keyword Stats ',Sys.Date()),full.names=T), ~ data.table(file=.x,time=file.mtime(.x))) %>%
                rbindlist %>% slice_max(time,n=1L) %>% pull(file),
              to=file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads",paste0(new_name,'.csv')))
  Sys.sleep(1)
  print(paste0(country,' - ',start_date,'~',end_date,' 추출 완료'))
}

gkp_simple <- function(keyword,start_date,end_date,country,new_name,print=FALSE) {
  remDr$navigate('https://ads.google.com/aw/keywordplanner/home?ocid=143996665&euid=556090245&__u=2335343005&uscid=769929296&__c=6457252304&authuser=1&subid=kr-ko-ha-aw-bk-a-m00%21o3~EAIaIQobChMIl7-T69_O9wIVwrWWCh2nvQGGEAAYASAAEgLjz_D_BwE~81876026093~kwd-324689985256~7918679677~434634269425')
  Sys.sleep(7)
  remDr$findElements(using='class name',value='secondary-text')[[2]]$clickElement()
  Sys.sleep(1.5)
  #keyword
  keyword_box <- remDr$findElement(using='css',value='[aria-label="Enter or paste your keywords, one word or phrase per line, or separated by commas"]')
  keyword_box$clickElement()
  keyword_box$sendKeysToElement(list(keyword))
  remDr$findElement(using='class name',value='submit-button')$clickElement()
  Sys.sleep(6.5)
  if(!missing(start_date)) {
    start_date <- paste0(month(as.Date(start_date),label=T,locale='US'),year(start_date))
    remDr$findElement(using='class name',value='date-popup-button')$clickElement()
    remDr$findElement(using='css',value='.start')$findChildElement(using='css',value='.baseline')$findChildElement(using='css',value='.top-section')$findChildElement(using='css',value='.input')$clearElement()
    remDr$findElement(using='css',value='.start')$sendKeysToElement(list(start_date))
    remDr$findElement(using='css',value='.start')$sendKeysToElement(list(key='enter'))
    remDr$findElement(using='css',value='.apply-bar')$findChildElement(using='css',value='.apply')$clickElement()
    Sys.sleep(0.35+runif(n=1,min=0.15,max=0.5))
  }
  if(!missing(end_date)) {
    end_date <- paste0(month(as.Date(end_date),label=T,locale='US'),year(end_date))
    remDr$findElement(using='class name',value='date-popup-button')$clickElement()
    remDr$findElement(using='css',value='.end')$clickElement()
    remDr$findElement(using='css',value='.end')$findChildElement(using='css',value='.baseline')$findChildElement(using='css',value='.top-section')$findChildElement(using='css',value='.input')$clearElement()
    remDr$findElement(using='css',value='.end')$sendKeysToElement(list(end_date))
    remDr$findElement(using='css',value='.end')$sendKeysToElement(list(key='enter'))
    remDr$findElement(using='css',value='.apply-bar')$findChildElement(using='css',value='.apply')$clickElement()
    Sys.sleep(0.35+runif(n=1,min=0.15,max=0.5))
    
    #location
    remDr$findElement(using='class name',value='location-button')$clickElement()
    remDr$findElement(using='class name',value='remove')$clickElement()
    remDr$findElement(using='class name',value='suggest-input')$clickElement()
    remDr$findElement(using='class name',value='suggest-input')$sendKeysToElement(list(country)) #location 입력
    Sys.sleep(2)
    remDr$findElement(using='class name',value='suggestion-item')$clickElement() #enter
    remDr$findElements(using='class name',value='btn-yes')[[11]]$clickElement() #save
    
    
    #save (1)
    Sys.sleep(runif(n=1,min=1,max=2.5))
    remDr$findElements(using='class name',value='action-button')[[2]]$clickElement()
    Sys.sleep(0.75)
    remDr$findElements(using='class name',value='menu-item-label-section')[[3]]$clickElement()
    
    #rename
    Sys.sleep(5)
    
    is_finish <- length(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                   pattern=paste0('Keyword Stats ',Sys.Date())))
    while(is_finish==0) {
      Sys.sleep(1.5)
      is_finish <- length(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                     pattern=paste0('Keyword Stats ',Sys.Date())))
    }
    Sys.sleep(1)
    file.rename(from=map(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                    pattern=paste0('Keyword Stats ',Sys.Date()),full.names=T), ~ data.table(file=.x,time=file.mtime(.x))) %>%
                  rbindlist %>% slice_max(time,n=1L) %>% pull(file),
                to=file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads",paste0(new_name,'.csv')))
    Sys.sleep(1)
    if(print==TRUE) { print(paste0(country,' - ',start_date,'~',end_date,'(',lang,') 추출 완료')) }
  }
}

gkp_read <- function(file_list,country,gbm=NA,product_type=NA,type,save_name=NULL,export_dir) {
  if(missing(file_list)) file_list <- choose.files(caption='전처리할 GKP RAW DATA를 선택하세요')
  type <- match.arg(type,choices=c('extended','base'))
  ans <- map(file_list, ~ {
    gkp <- read.csv(.x,skip=2,fileEncoding='UTF-16LE',sep="\t",header=T)
    res <- gkp %>% select(Keyword,starts_with('Search')) %>% data.table %>% melt(id='Keyword')
    names(res)[str_detect(names(res),'Searches')] <- gsub("Searches..","",names(res)[str_detect(names(res),'Searches')])
    country <- gsub(country,"",basename(.x))
    if(!is.na(gbm)) gbm <- gsub(gbm,"",basename(.x))
    if(!is.na(product_type)) product_type <- gsub(product_type,"",basename(.x))
    if(type=='extended') {
      gkp_res <- res %>% transmute(country,gbm,product_type,Keyword=Keyword,
                                   Is_extended=case_when(Keyword==res$Keyword[1] ~ 'Base',TRUE ~ 'Extended'),
                                   Main_keyword=res$Keyword[1],
                                   Date=my(variable),value) %>% 
        group_by(country,gbm,product_type,Main_keyword,Date,Is_extended) %>% summarize(value=sum(value,na.rm=T)) %>% suppressMessages %>% data.table %>% melt(measure='value') %>% dcast(country+gbm+product_type+Main_keyword+Date~Is_extended,)
      gkp_res$Base[is.na(gkp_res$Base)] <- 0
      gkp_res$Extended[is.na(gkp_res$Extended)] <- 0
      gkp_res <- gkp_res %>% rowwise %>% mutate(Extended=sum(Base,Extended,na.rm=T))
    } else {
      gkp_res <- res %>% transmute(country,gbm,product_type,keyword=Keyword,month=my(variable),searches_GKP=value)
      gkp_res$searches_GKP[is.na(gkp_res$searches_GKP)] <- 0
    }
    if(is.na(gbm)) { gkp_res <- gkp_res %>% select(-gbm) }
    if(is.na(product_type)) { gkp_res <- gkp_res %>% select(-product_type) }
    gkp_res
  }, .progress=TRUE) %>% rbindlist
  ans <- ans %>% filter(get(names(ans)[str_detect(tolower(names(ans)),'keyword')])!="")
  if(!is.null(save_name)) {
    if(missing(export_dir)) { export_dir <- choose.dir(caption='전처리된 자료를 저장할 폴더를 선택하세요' )}
    save_csv(ans,filename=save_name,dir=export_dir)
  }
  ans
}

gkp_read2 <- function(file_list,country,com_code=NA,gbm=NA,product_type=NA,save_name=NULL,export_dir) {
  if(missing(file_list)) file_list <- choose.files(caption='전처리할 GKP RAW DATA를 선택하세요')
  ans <- map(file_list, ~ {
    gkp <- read.csv(.x,skip=2,fileEncoding='UTF-16LE',sep="\t",header=T)
    res <- gkp %>% select(Keyword,starts_with('Search')) %>% data.table %>% melt(id='Keyword')
    res[,`:=`(variable=my(gsub("Searches..","",variable)),
              Is_extended=fcase(rowid(variable)==1,'Base',
                                default='Extended'),
              Seed_Keyword=res$Keyword[[1]])]
    country <- gsub(country,"",basename(.x))
    if(!is.na(com_code)) com_code <- gsub(com_code,"",basename(.x))
    if(!is.na(gbm)) gbm <- gsub(gbm,"",basename(.x))
    if(!is.na(product_type)) product_type <- gsub(product_type,"",basename(.x))
    
    res <- res %>% group_by(Region=country,Com_code=com_code,GBM=gbm,Product_type=product_type,
                            Seed_Keyword,Keyword,Seed_Related=Is_extended,Year=year(variable)) %>%
      summarize(Search_Volume=sum(value,na.rm=T)) %>% arrange(Seed_Related) %>% ungroup %>% 
      suppressMessages
    if(is.na(com_code)) { res <- res %>% select(-Com_code) }
    if(is.na(gbm)) { res <- res %>% select(-GBM) }
    if(is.na(product_type)) { res <- res %>% select(-Product_type)}
  }, .progress=TRUE) %>% rbindlist
  if(!is.null(save_name)) {
    if(missing(export_dir)) { export_dir <- choose.dir(caption='전처리된 자료를 저장할 폴더를 선택하세요' )}
    save_csv(ans,filename=save_name,dir=export_dir)
  }
  ans
}

gkp_read3 <- function(file_list,country,com_code=NA,gbm=NA,product_type=NA,seed_kw=NULL,date_interval=c('year','month'),save_name=NULL,export_dir) {
  date_interval <- match.arg(date_interval)
  if(missing(file_list)) file_list <- choose.files(caption='전처리할 GKP RAW DATA를 선택하세요')
  ans <- map(file_list, ~ {
    gkp <- read.csv(.x,skip=2,fileEncoding='UTF-16LE',sep="\t",header=T)
    res <- gkp %>% select(Keyword,starts_with('Search')) %>% data.table %>% melt(id='Keyword')
    res[,variable:=my(gsub("Searches..","",variable))]
    if(is.null(seed_kw)) {
      res[,`:=`(Is_extended=fcase(rowid(variable)==1,'Seed',
                                  default='Related'),
                Seed_Keyword=res$Keyword[[1]])]
    } else {
      res[,`:=`(Is_extended=fcase(Keyword %chin% seed_kw, 'Seed',
                                  default='Related'),
                Seed_Keyword=paste0(seed_kw,collapse='/'))]
    }
    country <- gsub(country,"",basename(.x))
    if(!is.na(com_code)) com_code <- gsub(com_code,"",basename(.x))
    if(!is.na(gbm)) gbm <- gsub(gbm,"",basename(.x))
    if(!is.na(product_type)) product_type <- gsub(product_type,"",basename(.x))
    res <- res %>% group_by(Region=country,Com_code=com_code,GBM=gbm,Product_type=product_type,
                            Seed_Keyword,Keyword,Seed_Related=Is_extended)
    if(date_interval=='year') {
      res <- res %>% group_by(year=year(variable),.add=T)
    } else {
      res <- res %>% group_by(month=floor_date(variable,unit='months'),.add=T)
    }
    res <- res %>% summarize(Searches_GKP=sum(value,na.rm=T)) %>% arrange(Seed_Related) %>% ungroup %>% 
      suppressMessages
    if(is.na(com_code)) { res <- res %>% select(-Com_code) }
    if(is.na(gbm)) { res <- res %>% select(-GBM) }
    if(is.na(product_type)) { res <- res %>% select(-Product_type)}
  }, .progress=TRUE) %>% rbindlist
  if(!is.null(save_name)) {
    if(missing(export_dir)) { export_dir <- choose.dir(caption='전처리된 자료를 저장할 폴더를 선택하세요' )}
    save_csv(ans,filename=save_name,dir=export_dir)
  }
  ans
}

gkp_read4 <- function(file_list,country,com_code=NA,gbm=NA,product_type=NA,seed_kw=NULL,obs,date_interval=c('year','month'),save_name=NULL,export_dir) {
  date_interval <- match.arg(date_interval)
  if(missing(file_list)) file_list <- choose.files(caption='전처리할 GKP RAW DATA를 선택하세요')
  if(missing(obs)) obs <- gsub("[^0-9]","",basename(.x))
  if(!is.null(seed_kw)) { setDT(seed_kw) }
  ans <- map(file_list, ~ {
    gkp <- read.csv(.x,skip=2,fileEncoding='UTF-16LE',sep="\t",header=T)
    res <- gkp %>% select(Keyword,starts_with('Search')) %>% data.table %>% melt(id='Keyword')
    res[,variable:=my(gsub("Searches..","",variable))]
    res[,obs:=as.numeric(gsub(obs,"",basename(.x)))]
    if(is.null(seed_kw)) {
      res[,`:=`(Is_extended=fcase(rowid(variable)==1,'Seed',
                                  default='Related'),
                Seed_Keyword=res$Keyword[[1]])]
    } else {
      seed_kw <- str_split(seed_kw$kw[seed_kw$obs==res$obs[1]],',')[[1]]
      res[,`:=`(Is_extended=fcase(Keyword %chin% seed_kw, 'Seed',
                                  default='Related'))]
    }
    country <- gsub(country,"",basename(.x))
    if(!is.na(com_code)) com_code <- gsub(com_code,"",basename(.x))
    if(!is.na(gbm)) gbm <- gsub(gbm,"",basename(.x))
    if(!is.na(product_type)) product_type <- gsub(product_type,"",basename(.x))
    res <- res %>% transmute(Region=country,com_code=com_code,gbm=gbm,product_type=product_type,
                             Keyword,Seed_Related=factor(Is_extended,levels=c('Seed','Related')),date=variable,searches_gkp=value) %>% 
      arrange(Region,Seed_Related,date)
    setDT(res)
    res
  },.progress=TRUE) %>% rbindlist
  ans <- funique(ans,cols=c('Region','com_code','gbm','product_type','Keyword'))
  if(is.na(gbm)) { ans <- ans %>% select(-gbm) }
  if(is.na(product_type)) { ans <- ans %>% select(-product_type) }
  if(is.na(com_code)) { ans <- ans %>% select(-com_code) }
  if(date_interval=='year') {
    ans[,year:=year(date)]
    ans <- ans[,.(searches_gkp=sum(searches_gkp,na.rm=T)),by=c('Region','com_code','gbm','Keyword','Seed_Related','year')]
  } else {
    ans[,month:=date]
    ans <- ans[,.(searches_gkp=sum(searches_gkp,na.rm=T)),by=c('Region','com_code','gbm','Keyword','Seed_Related','month')]
  }
  if(!is.null(save_name)) {
    if(missing(export_dir)) { export_dir <- choose.dir(caption='전처리된 자료를 저장할 폴더를 선택하세요' )}
    save_csv(ans,filename=save_name,dir=export_dir)
  }
  ans
}


se_install <- function(path) {
  path <- paste0('C:/',path)
  dir.create(path)
  download.file(url='https://github.com/mozilla/geckodriver/releases/download/v0.17.0/geckodriver-v0.17.0-win64.zip',
                destfile=file.path(path,'geckodriver.zip'))
  unzip(zipfile=file.path(path,'geckodriver.zip'),exdir=path)
  file.remove(file.path(path,'geckodriver.zip'))
  download.file(url='http://selenium-release.storage.googleapis.com/4.0/selenium-server-standalone-4.0.0-alpha-1.jar',
                destfile=file.path(path,'selenium-server-standalone-4.0.0-alpha-1.jar'))
  download.file(url='https://chromedriver.storage.googleapis.com/109.0.5414.74/chromedriver_win32.zip',
                destfile=file.path(path,'chrome-driver.zip'))
  unzip(zipfile=file.path(path,'chrome-driver.zip'),files='chromedriver.exe',exdir=path)
  file.remove(file.path(path,'chrome-driver.zip'))
}
