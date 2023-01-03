flip_fold <- function(start_date,end_date,aa_raw,gsc_raw,old_file,export_directory,ver=4,delay_time=0.5)
{
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  if(missing(aa_raw)) {
    suppressWarnings(map(c("aa_raw","gsc_raw"),~ if(!dir.exists(normalizePath(paste0(export_directory,"/",.x)))) dir.create(normalizePath(paste0(export_directory,"/",.x)))))
    message("AA 추출을 시작합니다")
    auth_oauth()
    get_me()
    map(c("sssamsungnewus","sssamsung4ca","sssamsung4uk","sssamsung4de","sssamsung4fr","sssamsung4it","sssamsung4es",
          "sssamsung4nl","sssamsung4se","sssamsung4cz","sssamsung4pl","sssamsung4ru","sssamsung4au","sssamsung4nz","sssamsung4vn",
          "sssamsung4th","sssamsung4id","sssamsung4my","sssamsung4sg","sssamsung4in","sssamsung4ae","sssamsung4sa","sssamsung4tr",
          "sssamsung4cn","sssamsung4tw","sssamsung4br"),~
          {
            date_range <- seq.Date(as.Date(start_date),as.Date(end_date),by="days")
            if (.x != "sssamsungnewus") {
              dimen <- c("referrertype","paidsearch","entryprop39")
              if(ver==3) {
                seg <- c("s200001591_62e872fb7fb9150fea2711d2","s200001591_62e8719859c1546edead804b")
              } else if(ver==4) {
                seg <- c("s200001591_62f5b7aabdc285313218a64d","s200001591_62f5b77df3bb4c5c0cdbb97f")
              }
            }
            else {
              dimen <- c("referrertype","paidsearch","prop14")
              if(ver==3) {
                seg <- c("s200001591_62e873139dba542907c9f18b","s200001591_62e8785ffb778b120a360eb0")
              } else if(ver==4) {
                seg <- c("s200001591_62f5b740de192d32554a5eb4","s200001591_62f5b67c0ab36d0f50c01b07")
              }
            }
            for (i in seq_along(date_range)) {
              for (j in seq_along(seg)) {
                name <- paste0(ifelse(seg[[j]] %chin% c("s200001591_62e872fb7fb9150fea2711d2","s200001591_62e873139dba542907c9f18b"),"Z FLIP3",
                                      ifelse(seg[[j]] %chin% c("s200001591_62e8719859c1546edead804b","s200001591_62e8785ffb778b120a360eb0"),"Z FOLD3",
                                             ifelse(seg[[j]] %chin% c("s200001591_62f5b7aabdc285313218a64d","s200001591_62f5b740de192d32554a5eb4"),"Z FLIP4","Z FOLD4"))),"-",toupper(gsub("sssamsung|new|4","",.x)),"_",date_range[[i]])
                Sys.sleep(delay_time)+round(runif(n=1,min=0.1,max=0.5),2)
                tryCatch(aw_freeform_table(company_id=Sys.getenv("AW_COMPANY_ID"),
                                           rsid=.x,
                                           dimensions=dimen,
                                           search=c("'Search Engine'","'Natural'",""),
                                           date_range=c(date_range[[i]],date_range[[i]]),
                                           segmentId=seg[[j]],
                                           metrics="entries",
                                           prettynames=TRUE,
                                           top=c(1,1,50000)) %>% mutate(Date=date_range[[i]]),
                         error=function(e) {
                           data.table(`Referrer Type`=NA,`Paid Search`=NA,`Entry URL (p39)`=NA,Entries=NA,Date=date_range[[i]])
                         }) %>% save_csv(filename=name,dir=normalizePath(paste0(export_directory,"/aa_raw")))
                Sys.sleep(delay_time)+round(runif(n=1,min=0.1,max=0.5),2)
              }
            }
          }, .progress=TRUE)
    aa <- list.files(normalizePath(paste0(export_directory,"/aa_raw")),full.names=T)
  } else {
    aa <- list.files(aa_raw,full.names=T)
  }
  aa <- map(aa,
            ~ {
              temp <- fread(.x,encoding="UTF-8")
              names(temp)[3] <- "url"
              sitecode <- gsub("_.*","",gsub("Z FOLD.|Z FLIP.|-","",basename(.x)))
              temp <- temp %>% mutate(country=sitecode)
            }) %>% rbindlist
  if(missing(gsc_raw)) {
    message("GSC 추출을 시작합니다")
    scr_auth()
    sitecode <- c("us","ca","uk","de","fr","it","es","nl","se","cz","pl","ru","au","nz","vn","th","id","my","sg","in","ae","sa","tr","cn","tw","br")
    date <- seq.Date(start_date,end_date,by="days")
    for (i in seq_along(date)) {
      map(sitecode,~
            {
              Sys.sleep(delay_time)+round(runif(n=1,min=0.1,max=0.5),2)
              tryCatch(search_analytics(siteURL=paste0("https://www.samsung.com/",.x,"/"),
                                        startDate=date[[i]],endDate=date[[i]],
                                        dimensions=c("page"),
                                        dimensionFilterExp="page ~~ smartphones",
                                        rowLimit=6e+05,
                                        walk_data="byBatch"),error=function(e) { data.table(page=NA,clicks=NA,impressions=NA,ctr=NA,position=NA)}) %>%
                mutate(date=date[[i]]) %>% relocate(date,.before=everything()) %>%
                save_csv(filename=paste0("GSC-RAW","-",.x,"-",date[[i]]),
                         dir=normalizePath(paste0(export_directory,"/gsc_raw")))
              Sys.sleep(delay_time)+round(runif(n=1,min=0.1,max=0.5),2)
            }, .progress=TRUE)
    }
    gsc <- list.files(normalizePath(paste0(export_directory,"/gsc_raw")),full.names=T)
  } else {
    gsc <- list.files(gsc_raw,full.names=T)
  }
  gsc <- map(gsc,~
               {
                 res <- fread(.x,encoding='UTF-8',col.names=c("date","page","clicks","impressions","ctr","position"))
                 sitecode <- toupper(gsub("GSC-RAW-|[0-9]|-.*","",basename(.x)))
                 setDT(res)[,country:=sitecode]
                 res
               }) %>% rbindlist
  setDT(gsc)
  gsc <- gsc[str_detect(page,'flip') | str_detect(page,'fold')]
  names(aa)[c(5,6)] <- c("date","country")
  aa <- aa[(str_detect(url,"smartphones") & str_detect(url,paste0("z-fold",ver))) | (str_detect(url,"smartphones") & str_detect(url,paste0("z-flip",ver)))]
  gsc <- gsc[(str_detect(page,"smartphones") & str_detect(page,paste0("z-fold",ver))) | (str_detect(page,"smartphones") & str_detect(page,paste0("z-flip",ver)))]
  gsc[,`:=`(url=substr(page,1,100))]
  aa[,`:=`(date,as.Date(date))]
  gsc <- gsc[,.(clicks=sum(clicks,na.rm=T),impressions=sum(impressions,na.rm=T),ctr=sum(clicks,na.rm=T)/sum(impressions,na.rm=T),position=mean(position,.na.rm=T)),by=c("date","url","country")]
  aa <- aa[,.(Entries=sum(Entries,na.rm=T)),by=c("url","date","country")]
  unique_kys <- bind_rows(distinct(aa,date,url,country),distinct(gsc,date,url,country)) %>% distinct_all
  setDT(unique_kys,key=c("date","url","country"))
  setDT(aa,key=c("date","url","country"))
  gsc$country[is.na(gsc$country)] <- "rest"
  setDT(gsc,key=c("date","url","country"))
  raw <- gsc[aa[,.(date,url,country,Entries)][unique_kys]] %>% setDT
  raw$url_4th <- str_split_fixed(gsub(".*com/{1,}","",raw$url),"/",n=6)[,4]
  raw$url_5th <- str_split_fixed(gsub(".*com/{1,}","",raw$url),"/",n=6)[,5]
  raw <- raw %>% mutate(page_type = case_when(url_4th == "buy" & url_5th == "" ~ "buy",
                                              url_4th == "buy" & (str_detect(url_5th,paste0("z-fold",ver)) | str_detect(url_5th,paste0("z-flip",ver))) ~ "buy",
                                              url_4th == "" ~ "mkt",
                                              url_4th %chin% c("showroom","accessories","design","experience","camera","durability","offers","compare","bespoke","spec","reviews","edition","samsung-credit","t-mobile-store","att-store","verizon-store","us-celluar-store") ~ "mkt",
                                              str_detect(url_4th,"my") ~ "mkt",
                                              url_4th == "specs" & url_5th == "support" ~ "mkt",
                                              url_4th=='buy' & str_starts(url_5th,'?modelCode') ~ 'mkt',
                                              url_4th=='page=home' & url_5th=="" ~ 'home')) %>% setDT
  raw[,`:=`(model,fcase(str_detect(url,paste0("z-fold",ver)),paste0("Fold ",ver),str_detect(url,paste0("z-flip",ver)),paste0("Flip ",ver)))]
  raw[,`:=`(launch_date,fcase(model=="Fold 3",as.Date("2021-08-11"),
                              model=="Flip 3",as.Date("2021-08-11"),
                              model=='Flip 4',as.Date('2022-08-10'),
                              model=='Fold 4',as.Date('2022-08-10')))]
  raw[,`:=`(d_plus,as.numeric(as.Date(date) - launch_date))]
  raw <- raw[(str_detect(url,"smartphones") & str_detect(url,paste0("z-fold",ver))) | (str_detect(url,"smartphones") & str_detect(url,paste0("z-flip",ver)))]
  raw <- select(raw,url,`4th_url`=url_4th,`5th_url`=url_5th,page_type,site_code=country,model,date,launch_date,d_plus,seo_traffic=Entries,clicks,impressions,ctr,avg_position=position)
  if(missing(export_directory)) export_directory <- choose.dir(caption="저장할 폴더를 선택하세요")
  raw %>% group_by(date2=date) %>% group_walk(~save_csv(.,filename=paste0("flip-fold-",.y$date2),dir=export_directory))
  if(missing(old_file)) old_file <- choose.files(caption='기존 전처리된 자료를 선택하세요')
  old_file <- fread(old_file,encoding='UTF-8')
  old_file[,ctr:=as.numeric(gsub("%","",ctr))]
  raw[,ctr:=as.numeric(ctr)] 
  raw <- bind_rows(old_file,raw)
  raw %>% save_csv(filename='flip-fold-total',dir=export_directory)
}
