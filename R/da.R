da_comb <- function(filename,table,dbname,dbuser,dbpw,dbhost,dbport,data,dnc,equal,contains,pf,export_directory) 
{
  if(str_detect(table,"-|\\.|:")) {
    message(paste0(table,"에는 - 또는 .,:이 포함될 수 없습니다. -,.,:은 _로 대체됩니다"))
    table <- gsub("-|\\.","_",table)
  }
  if(missing(data)) data <- choose.files(caption="RAW 자료(CSV)를 선택하세요")
  if(missing(dnc)) dnc <- choose.files(caption="Does Not Contains 자료를 선택하세요")
  if(missing(equal)) equal <- choose.files(caption="Equals 자료를 선택하세요")
  if(missing(contains)) contains <- choose.files(caption="Contains 자료를 선택하세요")
  if(missing(pf)) pf <- choose.files(caption="기초정보 자료를 선택하세요")
  dir_raname <- file_copy(c(data,dnc,equal,contains,pf))
  tryCatch({
    file_list <- list.files(paste0("C:/",dir_raname),full.names=T)
    dnc <- file_list[str_starts(basename(file_list),"doesnot")]
    equal <- file_list[str_starts(basename(file_list),"equal")]
    contains <- file_list[str_starts(basename(file_list),"contain")]
    pf <- file_list[str_starts(basename(file_list),"SamsungDA")]
    data <- reduce(list(file_list,dnc,equal,contains,pf),setdiff)
    if(readr::guess_encoding(file=data)$encoding[[1]]== "UTF-8") {
      raw <- fread(data,encoding="UTF-8")
    } else {
      raw <- suppressMessages(read_csv(data,locale=locale("ko",encoding="euc-kr")))
    }
    if(identical(names(raw),c("page","query","clicks","impressions","ctr","position","month","SiteCode","url"))) {
      raw <- raw %>% transmute(page,query,clicks,impressions,ctr,position,date=month,property=gsub(".*com/|/$","",url))
    }
    setDT(raw)
    raw <- raw[!str_detect(page,"\\?|#")]
    message("기준 자료들을 읽어오는 중입니다")
    map(c(contains,dnc,equal,pf),~{
      name <- case_when(str_starts(basename(.x),"does") ~ "dnc",str_starts(basename(.x),"equal") ~ "equal",
                        str_starts(basename(.x),"contain") ~ "contain",
                        TRUE ~ "pf")
      if(name != "pf") 
        assign(paste0("list_",name),value=read_excel(.x),envir=parent.env(environment()))
      else assign(paste0("list_",name),value=read_excel(.x,sheet="Page Type"),envir=parent.env(environment()))
    })
    colnames(list_pf) <- list_pf[2,]
    list_pf <- list_pf[-c(1:2),c(4:6)]
    colnames(list_pf)[3] <- "page_type"
    date_col <- which(map(names(raw),~is.Date(raw[,get(.x)]))==TRUE)
    names(raw)[date_col] <- "date"
    raw[,`:=`(date=as.Date(date),product_type=NA)]
    message("Does Not Contains 조건 분석중입니다")
    cl <- makeCluster(detectCores()-1)
    registerDoParallel(cl)
    temp1 <- foreach(i=seq_along(list_dnc$제품군),.combine=rbind,.packages=c("stringr","data.table")) %dopar% {
                       data.table(loc=which(str_detect(raw$page,list_dnc$page[i])),pd=list_dnc$제품군[i])
                     }
    stopCluster(cl)
    temp1 <- na.omit(temp1)
    raw$product_type[temp1$loc] <- temp1$pd
    df_dnc <- raw %>% filter(!is.na(product_type))
    message("Equals 조건 분석중입니다")
    df_equal <- left_join(raw %>% filter(is.na(product_type)),list_equal[,c(1,3)],by="page") %>% select(-product_type) %>% rename(product_type=제품군)
    df_contain <- df_equal %>% filter(is.na(product_type))
    df_equal <- df_equal %>% filter(!is.na(product_type))
    message("Contains 조건 분석중입니다")
    cl <- makeCluster(detectCores() - 1)
    registerDoParallel(cl)
    temp3 <- foreach(i=seq_along(list_contain$제품군),.combine=rbind,.packages=c("stringr","data.table")) %dopar% 
      {
        data.table(loc=which(str_detect(df_contain$page,list_contain$page[i])),pd=list_contain$제품군[i])
      }
    stopCluster(cl)
    temp3 <- na.omit(temp3)
    df_contain$product_type[temp3$loc] <- temp3$pd
    df_all <- bind_rows(df_equal,df_contain,df_dnc) %>% filter(!is.na(product_type))
    setDT(df_all,key="page")
    setDT(list_pf,key="URL")
    df_all <- list_pf[,.(URL,page_type)][df_all] %>% rename(page=URL) %>% select(names(df_all),page_type)
    df_all[is.na(page_type),`:=`(page_type,"PD")]
    if(sum(names(df_all) %chin% "SiteCode")== 1) {
      names(df_all)[which(names(df_all)== "SiteCode")] <- "property"
    }
    df_all[,`:=`(country=toupper(property),index_position=impressions*position,year_week=isoyear(date),year_month=year(date),week=isoweek(date),month=month(date))]
    df_all <- df_all %>% select(country,date,url=page,
                                query,clicks,impressions,ctr,position,index_position,
                                product_type,page_type,year_week,year_month,week,
                                month)
    df_all[product_type== "기타",`:=`(product_type,"Others")]
    if(missing(export_directory)) {
      export_directory <- choose.dir(caption="저장할 폴더를 선택하세요")
    }
    save_csv(df_all,filename=filename,dir=export_directory)
    message("DB에 업로드중입니다")
    db_upload(dbname=dbname,dbuser=dbuser,dbpw=dbpw,dbhost=dbhost,dbport=dbport,table=table,df_all)
    unlink(paste0("C:/",dir_raname),recursive=T)
  },error=function(e) {
    unlink(paste0("C:/",dir_raname),recursive=T)
  })
}

file_import <- function(name,dbname,dbuser,dbpw,dbhost,dbport,table,directory,export_directory) 
{
  if(missing(directory)) directory <- choose.dir(caption= "파일들이 있는 폴더를 선택하세요")
  if(missing(export_directory)) export_directory <- choose.dir(directory,caption= "전처리한 파일을 저장할 폴더를 선택하세요")
  directory <- normalizePath(directory)
  export_directory <- normalizePath(export_directory)
  name <- deparse(substitute(name))
  table <- deparse(substitute(table))
  if(str_detect(table,"-|\\.|:")) {
    message(paste0(table,"에는 - 또는 .,:이 포함될 수 없습니다. -,.,:은 _로 대체됩니다"))
    table <- gsub("-|\\.","_",table)
  }
  file_list <- list.files(directory)
  sql_temp <- file_list[menu(file_list,graphics= TRUE,title= "RAW 파일을 선택하세요")]
  while(!grepl(".csv$|.xlsx$|.xls$",sql_temp)) {
    sql_temp <- file_list[menu(file_list,graphics= TRUE,title= "RAW 파일을 다시 선택하세요")]
  }
  if(grepl(".csv$",sql_temp)) {
    sql_temp <- fread(normalizePath(file.path(directory,sql_temp)),encoding= "UTF-8")
  } else {
    sql_temp <- read_excel(normalizePath(file.path(directory,sql_temp)))
  }
  old_name <- names(sql_temp)[names(sql_temp)== "property"]
  names(sql_temp)[names(sql_temp)== "property"] <- "url"
  setDT(sql_temp)[,`:=`(country,gsub(".*com/","",url) %>% gsub("/.*","",.))]
  cat("선택하신 RAW 데이터의 국가별 행 수는 다음과 같습니다.\n")
  print(tryCatch(sql_temp[,.(Start_Date= min(date),End_Date= max(date),N= .N),by= country],
                 error= function(e) {
                   print(cat("\n선택하신 파일은 양식이 다릅니다."))
                 }))
  while(askYesNo("선택하신 RAW가 정확한가요?")!=TRUE) {
    sql_temp <- file_list[menu(file_list,graphics= TRUE,title= "RAW 파일을 선택하세요")]
    while(!grepl(".csv$|.xlsx$|.xls$",sql_temp)) {
      sql_temp <- file_list[menu(file_list,graphics= TRUE,title= "RAW 파일을 선택하세요")]
    }
    if(grepl(".csv$",sql_temp)) {
      sql_temp <- fread(normalizePath(file.path(directory,sql_temp)),data.table= FALSE,encoding= "UTF-8")
    } else {
      sql_temp <- read_excel(normalizePath(file.path(directory,sql_temp)))
    }
    old_name <- names(sql_temp)[names(sql_temp)== "property"]
    names(sql_temp)[names(sql_temp)== "property"] <- "url"
    setDT(sql_temp)[,`:=`(country,gsub(".*com/","",url) %>% gsub("/.*","",.))]
    cat("선택하신 RAW 데이터의 국가별 행 수는 다음과 같습니다.\n")
    print(tryCatch(sql_temp[,.(Start_Date= min(date),End_Date= max(date),N= .N),by= country],
                   error= function(e) {
                     print(cat("\n선택하신 파일은 양식이 다릅니다."))
                   }))
  }
  list_contain <- file_list[menu(file_list,graphics= TRUE,title= "contain.xlsx을 선택하세요")]
  while(!grepl(".xlsx",list_contain) || length(read_excel(normalizePath(file.path(directory,list_contain)))) != 4 || read_excel(normalizePath(file.path(directory,list_contain)))[4][[1]] != "contains") {
    list_contain <- file_list[menu(file_list,graphics= TRUE,title= "contain.xlsx을 다시 선택하세요")]
  }
  list_contain <- read_excel(normalizePath(file.path(directory,list_contain)))
  list_equal <- file_list[menu(file_list,graphics= TRUE, title= "equals.xlsx을 선택하세요")]
  while(!grepl(".xlsx",list_equal) || length(read_excel(normalizePath(file.path(directory,list_equal)))) != 4 || read_excel(normalizePath(file.path(directory,list_equal)))[4][[1]] != "equal") {
    list_equal <- file_list[menu(file_list,graphics= TRUE,title= "equals.xlsx을 다시 선택하세요")]
  }
  list_equal <- read_excel(normalizePath(file.path(directory,list_equal)))
  list_dnc <- file_list[menu(file_list,graphics= TRUE,title= "doesnotcontains.xlsx을 선택하세요")]
  while(!grepl(".xlsx",list_dnc) || length(read_excel(normalizePath(file.path(directory,list_dnc)))) != 4 || read_excel(normalizePath(file.path(directory,
                                                                                                                                                list_dnc)))[4][[1]] != "does not contains") {
    list_dnc <- file_list[menu(file_list,graphics= TRUE,title= "doesnotcontains.xlsx을 다시 선택하세요")]
  }
  list_dnc <- read_excel(normalizePath(file.path(directory,list_dnc)))
  list_pf <- file_list[menu(file_list,graphics= TRUE,title= "기초정보 파일을 선택하세요")]
  while(!grepl(".xlsx",list_pf) || length(read_excel(normalizePath(file.path(directory,list_pf)))) != 0 || suppressMessages(read_excel(normalizePath(enc2native(file.path(directory,list_pf))),sheet= "Page Type",col_names= FALSE)[[6]][3]) != 
        "Page Type") {
    list_pf <- file_list[menu(file_list,graphics= TRUE,title= "기초정보 파일을 다시 선택하세요")]
  }
  list_pf <- suppressMessages(read_excel(normalizePath(enc2native(file.path(directory,list_pf))),sheet= "Page Type"))
  colnames(list_pf) <- list_pf[2,]
  list_pf <- list_pf[-c(1:2),c(4:6)]
  colnames(list_pf)[3] <- "page_type"
  sql_temp[,`:=`(date= as.Date(date),product_type= NA)]
  sql_temp_b <- copy(sql_temp)
  message("does not contains 조건 평가중입니다")
  dnc_temp <- map(list_dnc$page,function(x) {
    data.table(N= which(str_detect(sql_temp$page,x)),Product_type= list_dnc$제품군[list_dnc$page==x])
  }) %>% suppressWarnings %>% rbindlist %>% filter(!is.na(N))
  sql_temp$product_type[dnc_temp$N] <- dnc_temp$Product_type
  res_dnc <- sql_temp %>% filter(!is.na(product_type))
  sql_temp <- sql_temp %>% filter(is.na(product_type))
  message("equals 조건 평가중입니다")
  equal_temp <- map(list_equal$page,function(x) {
    data.table(N= which(sql_temp$page %chin% x),Product_type= list_equal$제품군[list_equal$page==x])
  }) %>% suppressWarnings %>% rbindlist %>% filter(!is.na(N))
  sql_temp$product_type[equal_temp$N] <- equal_temp$Product_type
  res_equal <- sql_temp %>% filter(!is.na(product_type))
  sql_temp <- sql_temp %>% filter(is.na(product_type))
  message("contains 조건 평가중입니다")
  contain_temp <- map(list_contain$page,function(x) {
    data.table(N= which(str_detect(sql_temp$page,x)),Product_type= list_contain$제품군[list_contain$page==x])
  }) %>% suppressWarnings %>% rbindlist %>% filter(!is.na(N))
  sql_temp$product_type[contain_temp$N] <- contain_temp$Product_type
  res_contain <- sql_temp %>% filter(!is.na(product_type))
  sql_temp <- bind_rows(res_dnc,res_equal,res_contain)
  sql_temp <- sql_temp[!is.na(product_type)]
  message("page type 정보 평가중입니다")
  setDT(sql_temp,key= "page")
  setDT(list_pf,key= "URL")
  sql_temp <- list_pf[,.(URL,page_type)][sql_temp] %>% rename(page= URL) %>% select(names(sql_temp),page_type)
  sql_temp$page_type[is.na(sql_temp$page_type)] <- "PD"
  message("자료 정리중입니다")
  sql_temp[,`:=`(country= toupper(country),country_tier= NA,
                 index_position= impressions * position,year_week= isoyear(date),
                 year_month= year(date),week= isoweek(date),month= month(date))]
  sql_temp$country_tier[sql_temp$country %chin% c("US","UK","IN","DE","NL","RU")] <- 1
  sql_temp$country_tier[sql_temp$country %chin% c("IT","FR","ID","AU","ES","BR")] <- 2
  sql_temp$country_tier[is.na(sql_temp$country_tier)] <- 3
  sql_temp <- sql_temp %>% select(country,country_tier,date,
                                  page,clicks,impressions,ctr,position,index_position,
                                  product_type,page_type,year_week,year_month,week,
                                  month)
  sql_temp$product_type[sql_temp$product_type== "기타"] <- "Others"
  if(file.exists(file.path(export_directory,paste0(name,".csv")))) {
    write.table(sql_temp,file.path(export_directory,paste0(name,".csv")),row.names= F,append= T,col.names= F,sep= ",")
  }
  else {
    write.table(sql_temp,file.path(export_directory,paste0(name,".csv")),row.names= F,append= T,col.names= T,sep= ",")
  } 
  names(sql_temp)[names(sql_temp)== "url"] <- old_name
  names(sql_temp)[4] <- 'url'
  sql_temp[,`:=`(date,as.character(date))]
  assign(name,sql_temp,.GlobalEnv)
  db_upload(dbname=dbname,dbuser=dbuser,dbpw=dbpw,dbhost=dbhost,dbport=dbport,table=table,sql_temp)
}
