sam_sem_revamp <- function(file_list,revamp,country,p5_list,final_url,export_directory,dbname,dbuser,dbpw,dbhost,dbport) 
{
  if(missing(file_list)) file_list <- choose.files(caption="GSC CSV/Excel 자료 파일(들)을 선택하세요")
  dir_raname <- file_copy(file_list)
  tryCatch({
    gsc_csv <- list.files(paste0("C:/",dir_raname),pattern="csv$",full.names=T)
    if(length(gsc_csv) > 0) {
      gsc_csv <- lapply(gsc_csv,function(x) fread(x,encoding="UTF-8")) %>% rbindlist
      setDT(gsc_csv)[,`:=`(date=as.Date(date),clicks=as.numeric(clicks),impressions=as.numeric(impressions))]
    }
    if(is.character(gsc_csv)) {
      rm(gsc_csv)
    }
    gsc_xlsx <- list.files(paste0("C:/",dir_raname),pattern="xlsx$",full.names=T)
    if(length(gsc_xlsx) > 0) {
      gsc_xlsx <- lapply(gsc_xlsx,function(x) {
        read_excel(x,col_type=c("text","text","numeric","numeric","numeric","numeric","text"))
      }) %>% rbindlist
      gsc_xlsx[,`:=`(date,fcase(str_detect(date,"^4"),as.Date(as.numeric(date),origin="1899-12-30"),
                                !str_detect(date,"^4"),as.Date(date)))]
    }
    if(is.character(gsc_xlsx)) {
      rm(gsc_xlsx)
    }
    gsc <- bind_rows(mget(ls(pattern="^gsc")))
    setDT(gsc)[,`:=`(date=fcase(str_detect(date,"^4"),as.Date(as.numeric(date),origin="1899-12-30"),
                                !str_detect(date,"^4"),as.Date(date)))]
    gsc$page[nchar(gsc$page) < 100 & !str_ends(gsc$page,"/") & !str_ends(gsc$page,"pdf|xml|html|xlsx")] <- paste0(gsc$page[nchar(gsc$page) < 
                                                                                                                    100 & !str_ends(gsc$page,"/") & !str_ends(gsc$page,
                                                                                                                                                              "pdf|xml|html|xlsx")],"/")
    gsc[,`:=`(short_url,substr(page,1,100))]
    if(missing(revamp)) {
      message("DB에서 semi_aa_revamp_dashboard 테이블을 읽어오는 중입니다.")
      aa_revamp <- db_read(table="semi_aa_revamp_dashboard",dbname,dbuser,dbpw,dbhost,dbport)
      db_con <- dbConnect(dbDriver("MySQL"),dbname=dbname,
                          user=dbuser,password=dbpw,
                          host=dbhost,
                          port=dbport)
      aa_revamp_n <- as.numeric(dbGetQuery(db_con,"SELECT COUNT(*) FROM semi_aa_revamp_dashboard"))
      dbDisconnect(db_con)
      if(nrow(aa_revamp) < aa_revamp_n) {
        warning("DB에서 자료가 정상적으로 읽어지지 않았습니다. CSV DB 자료를 선택해주세요.")
        aa_revamp <- fread(choose.files(caption="CSV DB 자료를 선택하세요"))
      }
    }
    else {
      file_copy(revamp,dir=dir_raname)
      aa_revamp <- fread(file.path(paste0("C:/",dir_raname),paste0(gsub("[^A-z0-9|\\.]","",basename(revamp)))),encoding="UTF-8")
    }
    aa_revamp <- aa_revamp %>% filter(channel == "Organic" & year(date) >= 2021)
    Encoding(aa_revamp$entry_url_) <- "UTF-8"
    aa_revamp[,`:=`(short_url=substr(entry_url_,1,100),date=as.Date(date))]
    assign(x="aa_revamp0",value=aa_revamp,.GlobalEnv)
    message("AA 자료와 GSC 자료를 결합중입니다.")
    gsc2 <- gsc %>% group_by(date,short_url) %>% summarize(across(c("clicks","impressions","ctr","position"),sum)) %>% data.table
    aa_revamp2 <- aa_revamp %>% filter(short_url != "none") %>% 
      group_by(date,short_url) %>% summarize(across(c("entries"),sum)) %>% data.table
    setkey(gsc2,date,short_url)
    setkey(aa_revamp2,date,short_url)
    unique_key <- bind_rows(gsc2,aa_revamp2) %>% select(-c(clicks,impressions,ctr,position,entries)) %>% distinct_all
    setDT(unique_key,key=c("date","short_url"))
    comb <- gsc2[unique_key]
    comb <- aa_revamp2[comb]
    comb <- comb %>% filter(year(date) >= 2021)
    setkey(comb,short_url)
    if(missing(p5_list)) p5_list <- choose.files(caption="P5 기준 분류 리스트 파일을 선택하세요")
    file_copy(p5_list,dir=dir_raname)
    p5_list <- read_excel(file.path(paste0("C:/",dir_raname),gsub("[^A-z0-9|\\.]","",basename(p5_list))))
    setDT(p5_list,key="Entry Url")
    comb <- p5_list[comb] %>% rename(short_url="Entry Url") %>% relocate(starts_with("P5/"),.after=position)
    if(missing(final_url)) final_url <- choose.files(caption="P5-P6 매핑 리스트 파일을 선택하세요")
    file_copy(final_url,dir=dir_raname)
    final_url <- fread(file.path(paste0("C:/",dir_raname),gsub("[^A-z0-9|\\.]","",basename(final_url))))
    Encoding(final_url$Address) <- "UTF-8"
    final_url <- final_url[,`:=`(Address,substr(Address,1,100))] %>% distinct(Address,.keep_all=T)
    setkey(final_url,Address)
    setkey(comb,short_url)
    comb <- final_url[comb] %>% rename(short_url=Address) %>% relocate(`Final Address`,.after=last_col())
    aa_revamp_r2 <- aa_revamp[,.(short_url,revamp_url,section_revamp,section,product_revamp,site_code)] %>% distinct(short_url,.keep_all=T)
    setDT(aa_revamp_r2,key=c("short_url"))
    setDT(comb,key=c("short_url"))
    comb <- aa_revamp_r2[comb] %>% select(names(comb),revamp_url,section_revamp,section,product_revamp,site_code)
    comb_none <- aa_revamp[short_url == "none"] %>% mutate(clicks=NA,impressions=NA,ctr=NA,position=NA,`P5/P6매칭_다대일그룹`=NA,`P5/P6매칭_다대일상세`=NA,`Final Address`=NA) %>% 
      select(names(comb))
    comb <- bind_rows(comb,comb_none)
    if(missing(country)) {
      message("DB에서 AA 자료를 읽어오는 중입니다.")
      aa <- db_read(dbname,dbuser,dbpw,dbhost,dbport,table="semi_aa_country_demo_dashboard",year(date)>=2021 & channel=="Organic")
      db_con <- dbConnect(dbDriver("MySQL"),dbname=dbname,
                          user=dbuser,password=dbpw,
                          host=dbhost,
                          port=dbport)
      aa_n <- as.numeric(dbGetQuery(db_con,"SELECT COUNT(*) FROM semi_aa_country_demo_dashboard WHERE YEAR(date)>=2021 AND Channel='Organic'"))
      dbDisconnect(db_con)
      if(nrow(aa) < aa_n) {
        warning("DB에서 AA 자료가 정상적으로 읽어지지 않았습니다. CSV DB 자료를 선택해주세요.")
        aa <- fread(choose.files(caption="CSV DB 자료를 선택하세요"))
      }
    }
    else {
      file_copy(country,dir=dir_raname)
      aa <- fread(file.path(paste0("C:/",dir_raname),gsub("[^A-z0-9|\\.]","",basename(country))))
    }
    aa$entry_url_[str_starts(aa$entry_url_,"/semiconductor/minisite/isocell/")] <- paste0("https://www.samsung.com",aa$entry_url_[str_starts(aa$entry_url_,"/semiconductor/minisite/isocell/")])
    aa <- aa %>% filter(channel == "Organic" & year(date) >= 2021)
    aa <- aa %>% select(date,channel,site_code,entry_url_,section,product,entries)
    assign(x="aa0",value=aa,.GlobalEnv)
    aa_r2 <- aa[year(date) >= 2021 & channel == "Organic",.(short_url=substr(entry_url_,1,100),product)] %>% distinct(short_url,product) %>% arrange(desc(nchar(product))) %>% distinct(short_url,.keep_all=T)
    names(comb)[12] <- "section_revamp"
    setDT(aa_r2,key=c("short_url"))
    setDT(comb,key=c("short_url"))
    comb <- aa_r2[comb]
    comb_na_sc <- comb %>% filter(is.na(site_code))
    comb_sc <- comb %>% filter(!is.na(site_code))
    setDT(comb_na_sc)[,`:=`(site_code,map_chr(short_url,~sitecode_gsc(.x)))]
    comb <- bind_rows(comb_na_sc,comb_sc)
    if(missing(export_directory)) export_directory <- choose.dir(caption="저장할 경로를 선택해주세요")
    comb %>% save_csv(filename=paste0("gsc-aa-merge-date-page-raw-",tolower(month(Sys.Date(),label=T,locale="US")),day(Sys.Date())),dir=export_directory)
    unlink(paste0("C:/",dir_raname),recursive=T)
    comb
  },error=function(e) {
    unlink(paste0("C:/",dir_raname),recursive=T)
  })
}

sitecode_gsc <- function (x) 
{
  if (str_starts(x, "https://semiconductor.samsung.com/us/")) {
    res <- "US"
  }
  else if (str_starts(x, "https://semiconductor.samsung.com/emea/")) {
    res <- "EMEA"
  }
  else if (str_starts(x, "https://semiconductor.samsung.com/kr/") | 
           str_starts(x, "https://www.samsung.com/semiconductor/kr/")) {
    res <- "KR"
  }
  else if (str_starts(x, "https://semiconductor.samsung.com/cn/") | 
           str_starts(x, "https://www.samsung.com/semiconductor/cn/") | 
           str_starts(x, "https://www.samsung.com/semiconductor/minisite/exynos/cn/") | 
           str_starts(x, "https://www.samsung.com/semiconductor/minisite/isocell/cn/")) {
    res <- "CN"
  }
  else if (str_starts(x, "https://semiconductor.samsung.com/jp/") | 
           str_starts(x, "https://www.samsung.com/semiconductor/minisite/jp/")) {
    res <- "JP"
  }
  else if (str_starts(x, "https://semiconductor.samsung.com/") & 
           !str_detect(x, "/us/|/emea/|/kr/|/cn/|/jp/")) {
    res <- "Global"
  }
  else if (str_starts(x, "https://www.samsung.com/semiconductor/") & 
           !str_detect(x, "/cn/|/kr/|/semiconductor/minisite/")) {
    res <- "Global"
  }
  else if (str_starts(x, "https://www.samsung.com/semiconductor/minisite/ssd/")) {
    res <- "Global"
  }
  else if (str_starts(x, "https://www.samsung.com/semiconductor/minisite/exynos/") & 
           !str_detect(x, "/semiconductor/minisite/exynos/cn/")) {
    res <- "Global"
  }
  else if (str_starts(x, "https://www.samsung.com/semiconductor/minisite/isocell/") & 
           !str_detect(x, "/semiconductor/minisite/isocell/cn/")) {
    res <- "Global"
  }
  else {
    res <- NA
  }
  res
}
