obc_monthly <- function(gdc,ref_file_list,gsc_file_list,filename,export_directory) 
{
  if(missing(gdc)) gdc <- enc2native(choose.files(caption="GDC 기준 파일을 선택하세요"))
  if(sum(excel_sheets(gdc)=="OBC-URL List")==1) {
    gdc <- data.table(read_excel(gdc,sheet="OBC-URL List"))
  }
  else {
    gdc <- data.table(read_excel(gdc,sheet=select.list(excel_sheets(gdc),graphics=TRUE,title="기준 Sheet를 선택하세요")))
  }
  gdc <- gdc[-c(1:4),c(2,5,6)]
  colnames(gdc) <- c("Country","Model","URL")
  gdc[,`:=`(URL,tolower(URL))]
  aa4gdc <- distinct(mutate(gdc,URL=substr(URL,1,100)),URL,.keep_all=TRUE)
  setDT(aa4gdc,key="URL")
  if(!missing(ref_file_list)) ref_file_list <- list.files(ref_file_list,full.names=T)
  if(missing(ref_file_list))  ref_file_list <- choose.files(caption="REF RAW 파일을 선택하세요")
  cl <- makeCluster(detectCores() - 1)
  registerDoParallel(cl)
  ref <- foreach(i=seq_along(ref_file_list),.combine=rbind,.packages="readxl") %dopar% {
                   read_excel(ref_file_list[[i]],col_types=c("text","text","date","text","numeric"))
                 }
  stopCluster(cl)
  setDT(ref)
  ref[,`:=`(day,as.Date(day))]
  if(sum(ref$type == "marketing_channel") > 0) stop("MKT 자료가 포함되어 있습니다. 종료합니다!")
  ref[,`:=`(old_url=url,url=tryCatch(tolower(url),error=function(e) { tolower(iconv(url,"WINDOWS-1252","UTF-8")) }))]
  setDT(ref,key="url")
  aa_ref <- select(filter_all(aa4gdc[ref],all_vars(!is.na(.))),Country,Model,Entry_URL=old_url,Date=day,Entries=`natural traffic`) %>% 
    arrange(Country,Model,Date,desc(Entries)) %>% distinct_all
  if(missing(export_directory)) export_directory <- choose.dir(caption="저장할 위치를 선택하세요")
  save_csv(filename=paste0(filename,"_ref"),aa_ref,dir=export_directory)
  if(!missing(gsc_file_list)) gsc_file_list <- list.files(gsc_file_list,full.names=T)
  if(missing(gsc_file_list)) gsc_file_list <- choose.files(caption="GSC RAW 파일(들)을 선택하세요")
  cl <- makeCluster(detectCores()-1)
  registerDoParallel(cl)
  gsc <- foreach(i=seq_along(gsc_file_list),.combine=rbind,.packages=c("readxl","dplyr")) %dopar% {
                   temp <- read_excel(gsc_file_list[[i]],col_types=c("text","text","numeric","numeric","numeric","numeric","text"),range="A1:G2")
                   if(which(names(temp) == "date") == 2) {
                     temp <- read_excel(gsc_file_list[[i]],col_types=c("text","date","numeric","numeric","numeric","numeric","text"))
                   }
                   else if(which(names(temp) == "date") == 1) {
                     temp <- read_excel(gsc_file_list[[i]],col_types=c("date","text","numeric","numeric","numeric","numeric","text"))
                     temp <- relocate(temp,date,.after=page)
                   }
                   temp
                 }
  stopCluster(cl)
  gsc <- distinct_all(gsc)
  gsc <- setDT(gsc)
  gsc[,`:=`(page2,tolower(page))]
  setkey(gsc,page2)
  setDT(gdc,key="URL")
  ggdc <- data.table(select(filter_all(gdc[gsc],all_vars(!is.na(.))),Model,Country,Page=URL,date,clicks,impressions,ctr,position,url)) %>% arrange(Model,Country,date,desc(clicks))
  save_csv(filename=paste0(filename,"_gdc"),ggdc,dir=export_directory)
}
