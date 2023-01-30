# #PCKG MPRT
# for(i in c('dplyr','doParallel','stringr','data.table','readxl','writexl','sqldf','ggplot2','ggthemes','RMySQL','purrr',
#            'lubridate','oslo','searchConsoleR','googleAuthR','adobeanalyticsr','httr','rvest','rJava','RSelenium','stringi')) {
#   tryCatch(library(i,character.only=TRUE), error=function(e) {install.packages(i); library(i,character.only=TRUE)})
# }
# 
# #SETWD
# setwd(r"(C:\wkdir22_03\2022_11_15\search_volume)")
# 
# #DIRECTORY
# keyword_list_path <- file.path(getwd(),'keyword_list')
# raw_path <- file.path(getwd(),'raw')
# modi_path <- file.path(getwd(),'raw_modi')
# final_path <- file.path(getwd(),'export')
# excel_path <- file.path(getwd(),'export_excel')
# 
# #키워드 리스트 기준자료 불러오기
# map(c('AC','CA','REF','VC'),
#     ~ {
#       res <- read_excel(list.files(keyword_list_path,pattern=.x,full.names=T),sheet=2) %>% 
#         select(obs=`No.`,date=`기간`,country=`대상 국가`,language=언어,starts_with('Seed Keyword'),isext=`Keyword 확장 여부`) %>% 
#         mutate(language=gsub("&$|/$|&$","",language))
#       assign(paste0('kw_list_',.x),value=res,.GlobalEnv)
#     })
# 
# #키워드 정리(한 행에 여러 개의 언어가 있으면 다중열로 분할 + 키워드 9개씩 분리)
# scrap_list <- map(c('AC','CA','VC','REF'),
#                   ~ {
#                     temp <- get(ls(pattern=paste0('kw_list_',.x),envir=parent.env(environment())))
#                     temp <- temp %>% mutate(ln=str_count(language,'/|&')+1) %>% rowwise %>% slice(rep(1,each=ln)) %>% ungroup
#                     obs <- temp$obs
#                     date <- temp$date
#                     country <- temp$country
#                     language <- temp$language
#                     isext <- temp$isext
#                     ln <- temp$ln
#                     lang_cols <- names(temp)[str_detect(names(temp),'Seed Keyword')]
#                     res <- data.table()
#                     
#                     
#                     for(rows in seq_len(nrow(temp))) {
#                       for(i in seq_len(temp$ln[[rows]])) {
#                         res <- bind_rows(res,
#                                          data.table(isext=isext[rows],
#                                                     obs=obs[rows],date=date[rows],country=country[rows],
#                                                     language=str_split(temp$language[[rows]],pattern='/|&')[[1]][i],
#                                                     keyword=temp[[lang_cols[i]]][rows]))
#                         
#                       }
#                     }
#                     res <- res %>% mutate(n=str_count(keyword,pattern=',')+1,cn=ceiling(n/9))
#                     res <- data.table(Cat=.x,res)
#                     res
#                   }) %>% rbindlist
# 
# 
# scrap_list <- scrap_list %>% rowwise %>% mutate(keyword1=gsub("NA|^NA|NA,|,NA","",paste0(str_split(keyword,pattern=',')[[1]][1:9],collapse=',')),
#                                                 keyword2=gsub("NA|^NA|NA,|,NA","",paste0(str_split(keyword,pattern=',')[[1]][10:18],collapse=',')),
#                                                 keyword3=gsub("NA|^NA|NA,|,NA","",paste0(str_split(keyword,pattern=',')[[1]][19:27],collapse=',')),
#                                                 keyword4=gsub("NA|^NA|NA,|,NA","",paste0(str_split(keyword,pattern=',')[[1]][28:36],collapse=','))) %>% 
#   select(-keyword) %>% ungroup %>% mutate(ln=str_count(language,'/|&')+1) %>% distinct_all %>% relocate(isext,.before=Cat) %>%  setDT
# 
# 
# 
# scrap_list <- map(1:4,~ {scrap_list %>% select(isext,Cat,obs,date,country,language,keyword=paste0('keyword',.x)) }) %>% rbindlist %>% 
#   filter(keyword!="") %>% arrange(Cat,obs,country,language) %>% mutate(start=ym(gsub("~.*","",date)),
#                                                                        end=gsub(".*~","",date),
#                                                                        end=case_when(str_detect(end,'요청 종료') ~ floor_date(Sys.Date(),'months')-days(1),
#                                                                                      TRUE ~ ym(end)),
#                                                                        end=as.Date(end)) %>% setDT
# 
# 
# scrap_list$end[scrap_list$end>=floor_date(Sys.Date(),'months')-days(1)] <- floor_date(Sys.Date(),'months')-days(1)
# scrap_list <- scrap_list[!country %chin% 'Russia']
# 
# 
# scrap_list$country[scrap_list$country %chin% c('한국','Korea')] <- 'South Korea'
# scrap_list$country[str_detect(scrap_list$country,'UK') | str_detect(scrap_list$country,'United Kingdom') | str_detect(scrap_list$country,'영국')] <- 'United Kingdom'
# scrap_list$country[scrap_list$country %chin% 'Netherland'] <- 'Netherlands'
# scrap_list$language[scrap_list$language %chin% 'Engelish'] <- 'English'
# scrap_list$language[scrap_list$language %chin% c('Deutch','Deutsch')] <- 'German'
# scrap_list$language[scrap_list$language %chin% '한국어'] <- 'Korean'
# scrap_list$language[scrap_list$language %chin% 'Chinese'] <- 'Chinese (simplified)'
# scrap_list$keyword <- gsub(",$","",scrap_list$keyword)
# scrap_list$country <- gsub("\r|\n","",scrap_list$country)
# 
# 
# scrap_list <- scrap_list %>% group_by(Cat,obs,language) %>% mutate(rn=row_number(),n=n(),
#                                                                    new_name=case_when(n==1 ~ paste0('Search_Volume_Request_',Cat,'_',obs,'_',language),
#                                                                                       TRUE ~ paste0('Search_Volume_Request_',Cat,'_',obs,'_',language,'_',rn))) %>%
#   transmute(isext,Cat,obs,country,language,start,end,keyword,new_name) %>% setDT
# 
# 
# 
# #SCRAPPING
# # CD C:/selenium & java -Dwebdriver.gecko.driver="geckodriver.exe" -jar selenium-server-standalone-4.0.0-alpha-1.jar -port 4445
# 
# cprof <- getChromeProfile(dataDir=r"(C:\Users\SI-Partner-DT\AppData\Local\Google\Chrome\User Data)",
#                           "Profile 1")
# 
# remDr <- remoteDriver(remoteServerAddr="localhost",  
#                       port=4445L,  
#                       browserName="chrome",extraCapabilities=cprof)
# 
# 
# 
# # AC
# dataset <- scrap_list[Cat=='AC' & isext=='O' & year(start)<=2022]
# 
# remDr$open()
# remDr$navigate('https://ads.google.com/aw/keywordplanner/ideas/new?ocid=143996665&euid=556090245&__u=2335343005&uscid=769929296&__c=6457252304&authuser=1&subid=kr-ko-ha-aw-bk-a-m00%21o3~EAIaIQobChMIl7-T69_O9wIVwrWWCh2nvQGGEAAYASAAEgLjz_D_BwE~81876026093~kwd-324689985256~7918679677~434634269425')
# Sys.sleep(5)
# remDr$findElements(using='class name',value='visible-content')[[1]]$clickElement()
# remDr$findElement(using='css',value='[aria-label="Search input"]')$clickElement()
# remDr$findElement(using='css',value='[aria-label="Search input"]')$sendKeysToElement(list('test'))
# remDr$findElement(using='class name',value='submit-button')$clickElement()
# 
# 
# for(i in seq_len(nrow(dataset))) {
#   if(i%%10==0) {
#     remDr$close()
#     remDr$open()
#     remDr$navigate('https://ads.google.com/aw/keywordplanner/ideas/new?ocid=143996665&euid=556090245&__u=2335343005&uscid=769929296&__c=6457252304&authuser=1&subid=kr-ko-ha-aw-bk-a-m00%21o3~EAIaIQobChMIl7-T69_O9wIVwrWWCh2nvQGGEAAYASAAEgLjz_D_BwE~81876026093~kwd-324689985256~7918679677~434634269425')
#     Sys.sleep(5)
#     remDr$findElements(using='class name',value='visible-content')[[1]]$clickElement()
#     remDr$findElement(using='css',value='[aria-label="Search input"]')$clickElement()
#     remDr$findElement(using='css',value='[aria-label="Search input"]')$sendKeysToElement(list('test'))
#     remDr$findElement(using='class name',value='submit-button')$clickElement()
#   }
#   oslo::gkp_scrapper(start_date=dataset$start[[i]],
#                end_date=dataset$end[[i]],
#                keyword=dataset$keyword[[i]],
#                country=dataset$country[[i]],
#                lang=dataset$lang[[i]],
#                new_name=dataset$new_name[[i]])
#   message(paste0(i,"번째 완료"))
# }
# 
# 
# ### CA
# dataset <- scrap_list[Cat=='CA' & isext=='O' & year(start)<=2022]
# 
# 
# for(i in seq_len(nrow(dataset))) {
#   if(i%%10==0) {
#     remDr$close()
#     remDr$open()
#     remDr$navigate('https://ads.google.com/aw/keywordplanner/ideas/new?ocid=143996665&euid=556090245&__u=2335343005&uscid=769929296&__c=6457252304&authuser=1&subid=kr-ko-ha-aw-bk-a-m00%21o3~EAIaIQobChMIl7-T69_O9wIVwrWWCh2nvQGGEAAYASAAEgLjz_D_BwE~81876026093~kwd-324689985256~7918679677~434634269425')
#     Sys.sleep(5)
#     remDr$findElements(using='class name',value='visible-content')[[1]]$clickElement()
#     remDr$findElement(using='css',value='[aria-label="Search input"]')$clickElement()
#     remDr$findElement(using='css',value='[aria-label="Search input"]')$sendKeysToElement(list('test'))
#     remDr$findElement(using='class name',value='submit-button')$clickElement()
#   }
#   oslo::gkp_scrapper(start_date=dataset$start[[i]],
#                      end_date=dataset$end[[i]],
#                      keyword=dataset$keyword[[i]],
#                      country=dataset$country[[i]],
#                      lang=dataset$lang[[i]],
#                      new_name=dataset$new_name[[i]])
#   message(paste0(i,"번째 완료"))
# }
# 
# 
# for(i in 51:nrow(dataset)) {
#   if(i%%10==0) {
#     remDr$close()
#     remDr$open()
#     remDr$navigate('https://ads.google.com/aw/keywordplanner/ideas/new?ocid=143996665&euid=556090245&__u=2335343005&uscid=769929296&__c=6457252304&authuser=1&subid=kr-ko-ha-aw-bk-a-m00%21o3~EAIaIQobChMIl7-T69_O9wIVwrWWCh2nvQGGEAAYASAAEgLjz_D_BwE~81876026093~kwd-324689985256~7918679677~434634269425')
#     Sys.sleep(5)
#     remDr$findElements(using='class name',value='visible-content')[[1]]$clickElement()
#     remDr$findElement(using='css',value='[aria-label="Search input"]')$clickElement()
#     remDr$findElement(using='css',value='[aria-label="Search input"]')$sendKeysToElement(list('test'))
#     remDr$findElement(using='class name',value='submit-button')$clickElement()
#   }
#   oslo::gkp_scrapper(start_date=dataset$start[[i]],
#                      end_date=dataset$end[[i]],
#                      keyword=dataset$keyword[[i]],
#                      country=dataset$country[[i]],
#                      lang=dataset$lang[[i]],
#                      new_name=dataset$new_name[[i]])
#   message(paste0(i,"번째 완료"))
# }
# 
# 
# 
# ### REF
# dataset <- scrap_list[Cat=='REF' & isext=='O' & year(start)<=2022]
# 
# 
# for(i in seq_len(nrow(dataset))) {
#   if(i%%10==0) {
#     remDr$close()
#     remDr$open()
#     remDr$navigate('https://ads.google.com/aw/keywordplanner/ideas/new?ocid=143996665&euid=556090245&__u=2335343005&uscid=769929296&__c=6457252304&authuser=1&subid=kr-ko-ha-aw-bk-a-m00%21o3~EAIaIQobChMIl7-T69_O9wIVwrWWCh2nvQGGEAAYASAAEgLjz_D_BwE~81876026093~kwd-324689985256~7918679677~434634269425')
#     Sys.sleep(5)
#     remDr$findElements(using='class name',value='visible-content')[[1]]$clickElement()
#     remDr$findElement(using='css',value='[aria-label="Search input"]')$clickElement()
#     remDr$findElement(using='css',value='[aria-label="Search input"]')$sendKeysToElement(list('test'))
#     remDr$findElement(using='class name',value='submit-button')$clickElement()
#   }
#   oslo::gkp_scrapper(start_date=dataset$start[[i]],
#                      end_date=dataset$end[[i]],
#                      keyword=dataset$keyword[[i]],
#                      country=dataset$country[[i]],
#                      lang=dataset$lang[[i]],
#                      new_name=dataset$new_name[[i]])
#   message(paste0(i,"번째 완료"))
# }
# 
# 
# ### VC
# dataset <- scrap_list[Cat=='VC' & isext=='O' & year(start)<=2022]
# 
# 
# for(i in seq_len(nrow(dataset))) {
#   if(i%%10==0) {
#     remDr$close()
#     remDr$open()
#     remDr$navigate('https://ads.google.com/aw/keywordplanner/ideas/new?ocid=143996665&euid=556090245&__u=2335343005&uscid=769929296&__c=6457252304&authuser=1&subid=kr-ko-ha-aw-bk-a-m00%21o3~EAIaIQobChMIl7-T69_O9wIVwrWWCh2nvQGGEAAYASAAEgLjz_D_BwE~81876026093~kwd-324689985256~7918679677~434634269425')
#     Sys.sleep(5)
#     remDr$findElements(using='class name',value='visible-content')[[1]]$clickElement()
#     remDr$findElement(using='css',value='[aria-label="Search input"]')$clickElement()
#     remDr$findElement(using='css',value='[aria-label="Search input"]')$sendKeysToElement(list('test'))
#     remDr$findElement(using='class name',value='submit-button')$clickElement()
#   }
#   oslo::gkp_scrapper(start_date=dataset$start[[i]],
#                      end_date=dataset$end[[i]],
#                      keyword=dataset$keyword[[i]],
#                      country=dataset$country[[i]],
#                      lang=dataset$lang[[i]],
#                      new_name=dataset$new_name[[i]])
#   message(paste0(i,"번째 완료"))
# }
# 
# ###################################################################
# lapply(c('AC','CA','VC','REF'),
#        function(x) {
#          file_list <- list.files(file.path(raw_path),full.names=T,pattern=paste0('Search_Volume_Request_',x,'_'))
#          ##재저장
#          map(file_list, ~ {
#            if(grepl('.csv$',basename(.x))) {
#              temp0 <- read.csv(.x,skip=2,fileEncoding='UTF-16',sep="\t",header=T)[-(2:14)]
#            } else {
#              temp0 <- read_excel(.x)
#              names(temp0) <- temp0[2,]
#              temp0 <- temp0 %>% select(Keyword,starts_with('Search'))
#              temp0 <- temp0[-(1:2),]
#            }
#            try(save_csv(temp0,filename=gsub(".csv","",basename(.x)),
#                         dir=normalizePath(file.path(modi_path))))
#          })
#        })
# 
# 
# 
# 
# lapply(c('AC','CA','VC','REF'),
#        function(x) {
#          new_file_list <- list.files(file.path(modi_path),pattern=paste0('Search_Volume_Request_',x,'_'),full.names=T)
#          
#          temp <- map(new_file_list, ~ {
#            data.table(name=.x,
#                       obs=gsub("[^0-9|_]","",basename(.x)) %>% gsub("____","",.) %>% gsub("_[0-9]","",.) %>% gsub("_","",.),
#                       lang=gsub(paste0("Search_Volume_Request_",x),"",basename(.x)) %>% gsub("_[0-9]_","",.) %>% gsub("[0-9]|.csv$|_","",.))
#          }) %>% rbindlist %>% arrange(obs)
#          
#          temp2 <- temp %>% distinct(obs,lang) %>% mutate(y=paste0('Search_Volume_Request_',toupper(x),'_',obs,'_',lang)) %>% pull(y)
#          ##상단에 넣을 날짜 정보
#          if(x=='AC') {
#            ddate <- '2020년 1월 1일 (수) - 2022년 10월 31일 (월)'
#          } else if(x=='CA') {
#            ddate <- '2022년 1월 1일 (토) - 2022년 10월 31일 (월)'
#          } else if(x=='REF') {
#            ddate <- '2021년 1월 1일 (금) - 2022년 10월 31일 (월)'
#          } else if(x=='VC') {
#            ddate <- '2022년 4월 1일 (목) - 2022년 10월 31일 (월)'
#          }
#          
#          for(i in seq_along(temp2)) {
#            ans <- Reduce(bind_rows,map(new_file_list[str_detect(new_file_list,temp2[i])], ~ fread(.x)))
#            names_ans <- names(ans) %>% data.table %>% t %>% data.table
#            names(ans) <- paste0("V",seq_along(ans))
#            ans <- rbind(names_ans,ans)
#            first_date <- ans[1,]
#            first_date[,1] <- ddate
#            for(j in seq_along(names(first_date))[-1]) {
#              first_date[,j] <- NA
#            }
#            ans <- bind_rows(first_date,ans)
#            ans <- ans %>% filter(V1!="") %>% distinct(V1,.keep_all=T)
#            save_csv(ans,
#                     filename=temp2[[i]],
#                     dir=file.path(final_path),
#                     col_names=FALSE)
#          }
#          
#        })
# 
# 
# 
# lapply(c('AC','CA','REF','VC'),
#        function(x) {
#          file_list <- list.files(file.path(final_path),pattern=paste0('Search_Volume_Request_',x,'_'),full.names=T)
#          for(i in sort(unique(as.numeric(gsub("[^0-9]","",basename(file_list)))))) {
#            lists <- file_list[gsub("[^0-9]","",basename(file_list))==i]
#            sheet_n <- length(lists)
#            if(sheet_n==1) {
#              res <- list(fread(file_list[gsub("[^0-9]","",basename(file_list))==i]))
#              res <- setNames(res,i)
#              write_xlsx(res,
#                         path=file.path(excel_path,
#                                        paste0(gsub("_[0-9].*","",basename(lists)),'_',i,'.xlsx')),
#                         col_names=FALSE)
#              
#            } else {
#              res <- list()
#              lang_code <- c()
#              for(j in seq_len(sheet_n)) {
#                lang_code <- append(lang_code,paste0(i,'-',gsub(".*_|.csv$","",file_list[gsub("[^0-9]","",basename(file_list))==i][j])))
#                temp <- list(fread(file_list[gsub("[^0-9]","",basename(file_list))==i][j]))
#                res <- append(res,temp)
#              }
#              res <- setNames(res,lang_code)
#              write_xlsx(res,
#                         path=file.path(excel_path,
#                                        paste0(gsub("_[0-9].*","",basename(lists)),'_',i,'.xlsx')),
#                         col_names=FALSE)
#            }
#            
#          }
#        })
# 
