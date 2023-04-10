gmo_preprocessor <- function(file_list,excel_file,old_file=NULL,dbname=NULL,dbuser,dbpow,dbhost,dbport) {
  series <- read_excel(excel_file,sheet="series_R", skip=2) %>% transmute(model=분류, cond=`3rd_4th_조건`)
  
  read_excel(excel_file,sheet='사업부별',skip=2) %>%
    transmute(GBM=`GBM Detail`,second_dir=`2nd-directory 기준`,second_cond=`조건...4`,third_dir=`3rd-directory 기준`,third_cond=`조건...6`) %>% setDT %>% 
    group_by(g1=second_cond,g2=third_cond) %>%
    group_walk(~ assign(x=paste0('table_',.x$second_cond[[1]],'_',.x$third_cond[[1]]),value=.x,.GlobalEnv))
  
  res <- map(file_list, ~ {
    raw <- setDT(read_excel(.x))
    raw <- raw[!str_detect(url,'/support|/business/')]
    if(sum(names(raw)=='page')>=1) {
      raw <- raw %>% mutate(sitecode=gsub(".*com/|/$","",url),total_position=impressions*position,
                            month=ym(gsub("[^0-9]","",basename(.x)))) %>% 
        select(-url) %>% rename(url=page)
    }
    raw[,`:=`(`2nd`=str_split_fixed(url,'/',7)[,5],
              `3rd`=str_split_fixed(url,'/',7)[,6],
              `4th`=str_split_fixed(url,'/',7)[,7])]
    raw[`2nd` %chin% table_equal_NA$second_dir & !`2nd` %chin% table_equal_equal$second_dir & !`2nd` %chin% table_equal_contain$second_dir,
        Division:=as.character(future_map(`2nd`, ~ table_equal_NA$GBM[table_equal_NA$second_dir==.x]))]
    raw[str_detect(`2nd`,paste0(table_contain_NA$second_dir,collapse='|')) & (is.na(Division) | Division=='character(0)'),
        Division:=as.character(future_map(`2nd`, ~ table_contain_NA$GBM[table_contain_NA$second_dir==.x]))]
    raw[`2nd` %chin% table_equal_equal$second_dir & `3rd` %chin% table_equal_equal$third_dir,
        Division:=as.character(future_map2(`2nd`,`3rd`, function(x,y) { table_equal_equal$GBM[table_equal_equal$second_dir==x & table_equal_equal$third_dir==y] }  ))]
    raw[`2nd` %chin% table_equal_contain$second_dir & str_detect(`3rd`,paste0(table_equal_contain$third_dir,collapse='|')),
        Division:=as.character(future_map2(`2nd`,`3rd`,function(x,y) { table_equal_contain$GBM[table_equal_contain$second_dir==x &
                                                                                                 str_detect(y,table_equal_contain$third_dir)]  }        ))]
    raw[is.na(Division) | Division %chin% 'character(0)',Division:='Common']
    raw[,`:=`(GBM=Division,
              GBM_Detail=Division)]
    raw[!str_detect(url,'www.samsung.com'),`:=`(Division='Subdomain',GBM_Detail='Subdomain')]
  },.progress=TRUE) %>% rbindlist
  ## Series
  future_map(series$cond, ~ {
    res[`2nd` %chin% c('mobile','smartphones','microsite') & str_detect(`3rd`,.x) | str_detect(`4th`,.x),
        Series:=series$model[series$cond==.x]]
  })
  
  res[,Series:=as.character(Series)]
  res$Series[is.na(res$Series)] <- ""
  res[,Series_Detail:=Series]
  res[Series_Detail %chin% 'Z' & str_detect(url,'z-fold'),Series_Detail:='z-fold']
  res[Series_Detail %chin% 'Z' & str_detect(url,'z-flip'),Series_Detail:='z-flip']
  
  ####[smartphones]
  res[,smartphones:=fcase(`2nd` %chin% c('mobile','smartphones'),'(MX)smartphones',
                          Division %chin% 'MX', '(MX)others',
                          default="")]
  
  if(sum(names(res)=='total_position')==1) {
    #GSC RAW
      res <- res[,.(sitecode,month,page=url,GBM=Division,GBM_Detail,`2nd`,`3rd`,series=Series,series_Detail=Series_Detail,
                    smartphones,clicks,impressions,position,total_position,Division,`4th`)]
  } else {
    #AA RAW
    res <- res[,.(sitecode=country,month,url,GBM=Division,GBM_Detail,`2nd`,`3rd`,
                  series=Series,series_Detail=Series_Detail,smartphones,`natural traffic`,Division,`4th`)]
  }
  rm(list=ls(pattern='^table_contain_|^table_equal_',envir=.GlobalEnv),envir=.GlobalEnv)
  if(!is.null(old_file)) {
    file.copy(from=old_file,
              to=file.path(dirname(old_file),paste0(basename(old_file),"의 백업자료")))
    fwrite(res,file=old_file,append=TRUE)
  }
  if(!is.null(dbname)) {
    if(sum(names(res)=='total_position')==1) {
      res <- res[,.(sitecode,month,page=url,GBM=Division,GBM_Detail,URL_2nd=`2nd`,URL_3rd=`3rd`,series=Series,series_Detail=Series_Detail,
                    smartphones,clicks,impressions,position,total_position,Division,URL_4th=`4th`)]
      table <- 'GMC_B2C_month_page_GSC'
    } else {
      res[,.(sitecode=country,month,url,GBM=Division,GBM_Detail,URL_2nd=`2nd`,url_3rd=`3rd`,
             series=Series,series_Detail=Series_Detail,smartphones,`natural traffic`,Division,url_4th=`4th`)]
      table <- 'GMC_B2C_month_page_AA'
    }
    db_upload(dbname=dbname,dbuser=dbuser,dbpw=dbpw,dbhost=dbhost,dbport=dbport,table=table,res)
  }
  res
}

gmo_aggregate <- function(df,old_file=NULL) {
  raw <- setDT(df)
  raw$smartphones[is.na(raw$smartphones)] <- ""
  raw$`2nd`[is.na(raw$`2nd`)] <- ""
  raw$series[is.na(raw$series)] <- ""
  raw$series_Detail[is.na(raw$series_Detail)] <- ""
  if(sum(names(raw)=='natural traffic')>0) {
    res <- raw[,.(`natural traffic`=sum(`natural traffic`,na.rm=T)),by=c('sitecode','month','Division','GBM_Detail')]
  } else {
    res <- raw[,.(clicks=sum(clicks,na.rm=T),
                  impressions=sum(impressions,na.rm=T),
                  total_position=sum(total_position,na.rm=T)
    ),by=c('sitecode','month','Division','GBM_Detail')]
  }
  if(!is.null(old_file)) {
    file.copy(from=old_file,
              to=file.path(dirname(old_file),paste0(basename(old_file),"의 백업자료")))
    fwrite(res,file=old_file,append=TRUE)
  }
  res
}
