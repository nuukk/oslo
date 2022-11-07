frog <- function(url,frog_directory,export_directory,type=c("redirect chains","internal all","crawl overview","all"),config=NULL,drop_n=0,gbm=NULL,tmlmt=8000) {
  if(missing(url)) url <- choose.files(caption="URL 파일(들)을 선택하세요")
  if(dir.exists(url[1]) & drop_n==0) url <- list.files(url,full.names=T,pattern="csv$")
  if(dir.exists(url[1]) & drop_n>0) url <- list.files(url,full.names=T,pattern="csv$")[-seq_len(drop_n)]
  message(paste(basename(url),"이 Screaming Frog 작업에 사용됩니다"))
  if (missing(frog_directory)) {
    if(file.exists("C:\\Program Files (x86)\\Screaming Frog SEO Spider\\ScreamingFrogSEOSpiderCli.exe")) frog_directory <- "C:\\Program Files (x86)\\Screaming Frog SEO Spider"
    else frog_directory <- choose.dir(caption="SCREAMING FROG가 설치된 경로를 선택하세요")
  }
  if(missing(export_directory)) export_directory <- choose.dir(caption="파일을 저장할 경로를 선택하세요")
  if(missing(type)) type <- "all"
  type <- match.arg(type)
  if(type =="redirect chains") type <- '--save-report "Redirects:Redirect Chains"'
  else if(type =="internal all"|type=="crawl overview") type <- '--export-tabs "Internal:All"'
  else if(type =="all") type <-  '--save-report "Redirects:Redirect Chains" --export-tabs "Internal:All"'
  for (i in seq_along(url)) {
    if(is.null(config)) {
      command <- paste("CD",frog_directory,"& ScreamingFrogSEOSpiderCli.exe --crawl-list",url[[i]],"--headless --overwrite",type,"--output-folder",export_directory)
    }
    else{
      command <- paste("CD",frog_directory,"& ScreamingFrogSEOSpiderCli.exe --crawl-list",url[[i]],"--config",config,"--headless --overwrite",type,"--output-folder",export_directory)
    }
    message(command)
    system2("cmd",input=command,timeout=tmlmt)
    message("\n")
    if(file.exists(file.path(export_directory,"internal_all.csv"))) {
      internal <- fread(file.path(export_directory,"internal_all.csv"),encoding="UTF-8",fill=T)
      if(sum(names(internal) %chin% "Global P6 Page Template 2")==0) internal <- bind_cols(internal,`Global P6 Page Template 2`=NA)
      internal <- select(internal,Address,`Title 1`,
                         `Title 1 Length`,`Title 1 Pixel Width`,`Meta Description 1`,
                         `Meta Description 1 Length`,`Meta Description 1 Pixel Width`,
                         `Canonical Link Element 1`,`P6? 1`,`Global P6 pageTrack 1`,
                         `US P5 pageTrack 1`,`Global P6 Page Template 1`,
                         `Global P6 Page Template 2`)
      sitecode <- internal %>% count(temp_sitecode=gsub("/.*","",gsub(".*com/{1,}","",Address))) %>% filter(!str_detect(temp_sitecode,"[[:digit:]]")) %>% filter(n ==max(n)) %>% pull(temp_sitecode)
      file.rename(from=file.path(export_directory,"internal_all.csv"),to=file.path(export_directory,paste0(sitecode,"-internal-all.csv")))
    }
    if(file.exists(file.path(export_directory,"redirect_chains.csv"))) {
      redirect <- fread(file.path(export_directory,"redirect_chains.csv"),encoding="UTF-8",fill=T) %>%
        select(`Number of Redirects`,Address,`Final Address`,`Final Status Code`,`Status Code 1`,`Redirect URL 1`,`Status Code 2`,
               `Redirect URL 2`,`Status Code 3`,`Redirect URL 3`,`Status Code 4`,`Redirect URL 4`,`Status Code 5`,`Redirect URL 5`,
               `Status Code 6`,`Redirect URL 6`,`Status Code 7`,`Redirect URL 7`,`Status Code 8`,`Redirect URL 8`,`Status Code 9`,`Redirect URL 9`,
                `Status Code 10`,`Redirect URL 10`)
      sitecode <- redirect %>% count(temp_sitecode=gsub("/.*","",gsub(".*com/{1,}","",Address))) %>% filter(!str_detect(temp_sitecode,"[[:digit:]]")) %>% filter(n ==max(n)) %>% pull(temp_sitecode)
      file.rename(from=file.path(export_directory,"redirect_chains.csv"),to=file.path(export_directory,paste0(sitecode,"-redirect_chains.csv")))
    }
    if(exists("internal") & exists("redirect")) {
      temp <- full_join(internal,redirect,by=c("Address")) %>% mutate(`Meta Description 1`=gsub("^ |^\n| $","",`Meta Description 1`)) %>%
        filter(!str_detect(Address,"#|\\?") & str_ends(Address,"/") & str_starts(Address,paste0("https://www.samsung.com/",sitecode,"/")) &
                 !str_detect(gsub("^https://","",Address),"/{2,}") & !str_detect(Address,"%20") &
                 !str_detect(gsub("^https://www.samsung.com/","",Address),"\\.") &
                 !str_detect(Address,",|&|\\?") & !str_ends(Address,"/1/") &
                 !str_detect(Address,"/login/") & !str_detect(Address,"/us/mobile/cell-phones/") & 
                 !str_detect(Address,"/us/mobile/cell-phones-accessories/") &
                 !str_detect(Address,paste0("/",sitecode,"/common/")) &
                 !str_detect(Address,paste0("/",sitecode,"/c/p/")) &
                 !str_detect(Address,paste0("/",sitecode,"/web/")) &
                 !str_detect(Address,paste0("/",sitecode,"/lite/")) &
                 !str_detect(Address,paste0("/",sitecode,"/search/")) &
                 !str_detect(Address,paste0("/",sitecode,"/amp/")) &
                 !str_detect(Address,paste0("/",sitecode,"/appliances/")))
      temp$`Meta Description 1` <- gsub("^\n|^ | $","",temp$`Meta Description 1`)
      sitecode <- temp %>% count(temp_sitecode=gsub("/.*","",gsub(".*com/{1,}","",Address))) %>% filter(!str_detect(temp_sitecode,"[[:digit:]]")) %>% filter(n ==max(n)) %>% pull(temp_sitecode)
      temp[,`:=`(sitecode,tolower(sitecode))]
      save_csv(temp,filename=paste0(sitecode,"-frog"),dir=export_directory)
      if(is.null(gbm)) gbm <- normalizePath(choose.files(caption="사업부 분류 기준 엑셀 파일을 선택하세요"))
      gbm_table <- data.table(read_excel(gbm))
      gbm_table <- gbm_table[-seq_len(3),.(GBM=...2,SDC=...3)]
      temp[,`:=`(SDC,(gsub("^https://www.samsung.com/","",Address) %>% str_split_fixed(.,"/",3))[,2])]
      setkey(temp,SDC)
      setkey(gbm_table,SDC)
      temp <- gbm_table[temp] %>% select(names(temp),GBM)
      temp$GBM[tolower(temp$Section) =="support"] <- "Support"
      temp$GBM[tolower(temp$Section) =="b2b"] <- "B2B"
      save_csv(temp,filename=paste0(sitecode,"-frog_language"),dir=export_directory)
    }
  }
}
