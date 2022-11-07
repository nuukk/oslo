gkp_scrapper <- function(start_date,end_date,keyword,country,lang='English',new_name) {
  lang <- match.arg(lang,choices=c('Arabic','Bengali','Bulgarian','Catalan','Chinese (simplified)','Chinese (traditional)','Croatian','Czech','Danish','Dutch','English','Estonian','Filipino','Finnish','French','German','Greek','Gujarati','Hebrew','Hindi','Hungarian','Icelandic','Indonesian','Italian','Japanese','Kannada','Korean','Latvian','Lithuanian','Malay','Malayalam','Marathi','Norwegian','Persian','Polish','Portuguese','Punjabi','Romanian','Russian','Serbian','Slovak','Slovenian','Spanish','Swedish','Tamil','Telugu','Thai','Turkish','Ukrainian','Urdu','Vietnamese'))
  Sys.sleep(2+runif(n=1,min=0.25,max=0.8)+abs(rnorm(n=1,mean=0.5,sd=0.25)))
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
  if(old_country!=country) {
    remDr$findElement(using='class name',value='location-button')$clickElement() #remDr$findElement(using='css',value='.settings-bar>.location-button')$clickElement()
    remDr$findElement(using='class name',value='remove')$clickElement() #기존 설정된 location 제거
    remDr$findElement(using='class name',value='suggest-input')$clickElement() #enter a location to target
    remDr$findElement(using='class name',value='suggest-input')$sendKeysToElement(list(country)) #location 입력
    Sys.sleep(2)
    remDr$findElement(using='class name',value='suggestion-item')$clickElement() #enter
    remDr$findElements(using='class name',value='btn-yes')[[3]]$clickElement() #save
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
}