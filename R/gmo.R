gmo_preprocessor <- function(file_list,excel_file,old_file=NULL,dbname=NULL,dbuser,dbpow,dbhost,dbport) {
  if(missing(file_list)) file_list <- choose.files(caption='전처리에 사용될 RAW XSLX 파일(들)을 선택해주세요')
  file_list <- normalizePath(file_list)
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

gmo_aggregate <- function(dfname,old_file=NULL) {
  raw <- setDT(dfname)
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

frog_pagetype <- function(file_list) {
  if(missing(file_list)) file_list <- choose.files(caption='전처리에 사용될 RAW XSLX 파일(들)을 선택해주세요')
  file_list <- normalizePath(file_list)
  res <- future_map(file_list, \(x) {
    raw <- fread(x,select=c('Address','Global P6 pageTrack 1','US P5 pageTrack 1','Global P6 Page Template 1','Status Code 1'))
    raw <- raw[!str_detect(Address,'/business/|/support/') & str_starts(Address,'https://www.samsung.com/') & !is.na(Address) &
                 `Status Code 1`==200] %>% distinct(Address,.keep_all=T)
    
    ## GBM-Division 분류
    raw[,`:=`(URL_2nd=str_split_fixed(Address,"/",8)[,5],
              URL_3rd=str_split_fixed(Address,"/",8)[,6],
              URL_4th=str_split_fixed(Address,"/",8)[,7],
              URL_5th=str_split_fixed(Address,"/",8)[,8])]
    
    raw[URL_2nd %chin% 'computing' & URL_3rd %chin% 'monitors',Division:='Division']
    raw[URL_2nd %chin% 'de' & URL_3rd %chin% 'mobile-content-page',Division:='Division']
    raw[URL_2nd %chin% 'MKT' & str_detect(URL_3rd,'galaxy-s'),Division:='Division']
    raw[URL_2nd %chin% c('tvs','televisions-home-theater','lifestyle-tvs','serif-tvs','qled','commercial-tvs','business-tvs','tv-accessories','monitors','business-monitors',
                         'smart-monitor','gaming-monitor','video-players','led-signage','smart-signage','displayes','audio-devices','audio-video','home-theater','soundbar',
                         'sound-bar','tv-audio','all-tv-accessories','display-accessories','lifestyle-tv','tv-upgrade','innovation-tv','tv-premium-collection',
                         'frame-tv-art-store-promotion','big-screen-tv','qled-tv-and-soundbar-deal','projector-accessories','audio-accessories','smart-tv','video',
                         'the-freestyle','the-wall','firstlook','samsung-flip','dynamic-interactive-screen','qled2020reviews','win-a-soundbar','smartphones','mobile',
                         'enterprise-edition','galaxya','tablets','computing','watches','wearables','smartwatches-and-fitness-bands','computers','pc','galaxy-book',
                         'chromebooks','laptops','galaxybooks','notebooks','audio-sound','mobile-audio','audio','akg','galaxy-buds','mobile-accessories',
                         'computer-accessories','pc-accessories','smartthings','mobile-iot','hub','mobile-phone-buying-guide','cameras','smartthings-accessories',
                         'trackers','hubs','computing-accessories','power-of-10','precommande-note20-shop','grandjeu-smartphonez-fr','smart-switch','s20-offer',
                         's8-registration','s9-offer','tabs7-offer','note20-care-plus','air-care','air-purifier','air-purifiers','home-appliances','air-conditioners',
                         'cooking-appliances','wall-ovens','ranges','dishwashers','microwave-ovens','microwaves','refrigerators','built-in','system-air-conditioners',
                         'vacuum-cleaners','handstick','air-dresser','washers-and-dryers','washing-machines','dryers','laundry','washers','home-appliance-accessories',
                         'home-appliances-accessories','cooktops','dualcookflex','range-hoods','familyhub','bespoke','air-conditioner-customer-enquiry','air-solutions',
                         'bespoke-jet','infinite-line','addwash','chef-collection','fiveyearwarranty','home-appliances1','what-size-washing-machines-a','airdresser',
                         'built-in-refrigerators','cooktops-and-hoods','dishwasher','family-hub','front-loader','ha-accessories','hoods','microwave','ovens','quickdrive',
                         'vacuum-cleaner','vacuums','waschmaschine'),Division:='Division']
    raw[str_detect(URL_2nd,paste0(c('lifestyle','projector','audio-video','tablets','galaxytabs','gear','galaxy-watch-active',
                                    'galaxybook','pc','harman-','mobile','galaxy','phones','wearables','laptop-buying-guide','laptop'),collapse='|')),
        Division:='Division']
    
    ####Page Track
    raw[,`:=`(g_pagetrack=fcase(str_detect(`Global P6 pageTrack 1`,'marketing|rketing'),'MKT',
                                str_detect(`Global P6 pageTrack 1`,'product detail'),'PD',
                                str_detect(`Global P6 pageTrack 1`,'flagship pdp'),'Flagship PDP',
                                str_detect(`Global P6 pageTrack 1`,'product category detail'),'PCD',
                                str_detect(`Global P6 pageTrack 1`,'product family showcase'),'PFS',
                                str_detect(`Global P6 pageTrack 1`,'product finder'),'PF',
                                str_detect(`US P5 pageTrack 1`,'product category detail'),'PCD'),
              g_template=fcase(str_detect(`Global P6 Page Template 1`,'standard-pd'),'PD-standard',
                               str_detect(`Global P6 Page Template 1`,'bc-pd|buying-pd'),'PD-buying',
                               str_detect(`Global P6 Page Template 1`,'feature-pd'),'PD-feature',
                               str_detect(`Global P6 Page Template 1`,'flagship'),'Flagship PDP',
                               str_detect(`Global P6 Page Template 1`,'pcd'),'PCD',
                               str_detect(`Global P6 Page Template 1`,'pfs'),'PFS',
                               str_detect(`Global P6 Page Template 1`,'pf'),'PF',
                               str_detect(`Global P6 Page Template 1`,'marketing'),'MKT',
                               str_detect(`Global P6 Page Template 1`,'faq'),'FAQ',
                               str_detect(`Global P6 Page Template 1`,'article'),'Article'))]
    
    ###
    raw[,pagetype_detail:=fcase(
      !is.na(g_template),g_template,
      !is.na(g_pagetrack),g_pagetrack,
      URL_2nd %chin% 'explore', 'Explore',
      URL_2nd %chin% 'offer','Offer',
      URL_2nd %chin% 'news','Article',
      URL_2nd %chin% 'shop-faq','FAQ',
      Division=='Division' & str_detect(URL_5th, "[a-z][a-z]-[a-z,1-9][a-z,0-9][0-9]"),'PD',
      Division=='Division' & str_detect(URL_4th, "[A-Z][-,_,A-Z][-,_,0-9,A-Z][-,0-9,A-Z]"),'PD',
      Division=='Division' & str_detect(URL_4th, "[a-z][a-z]-[a-z][a-z,1-9][a-z,0-9][0-9]"),'PD',
      Division=='Division' & str_detect(URL_3rd, "hdtv-[a-z][1-9][0-9][0-9]"),'PD',
      Division=='Division' & str_detect(URL_3rd, "[a-z][a-z]-[a-z][a-z,1-9][a-z,0-9][0-9]"),'PD',
      Address %chin% c("https://www.samsung.com/us/bespoke/","https://www.samsung.com/us/laundry/",
                       "https://www.samsung.com/us/healthy-haven/","https://www.samsung.com/us/explore/family-hub-refrigerator/features/","https://www.samsung.com/us/refrigerators/bespoke/design-studio/addons/",
                       "https://www.samsung.com/us/tvs/gaming-tv/","https://www.samsung.com/us/tvs/qled-tv/smart-tv/"),'MKT',
      Address %chin% c("https://www.samsung.com/us/dryers/","https://www.samsung.com/us/ranges/","https://www.samsung.com/us/vacuums/","https://www.samsung.com/us/washers/","https://www.samsung.com/us/cooktops/",
                       "https://www.samsung.com/us/microwaves/","https://www.samsung.com/us/wall-ovens/","https://www.samsung.com/us/air-dresser/","https://www.samsung.com/us/dishwashers/","https://www.samsung.com/us/air-purifiers/",
                       "https://www.samsung.com/us/refrigerators/","https://www.samsung.com/us/cooktops-and-hoods/","https://www.samsung.com/us/home-appliances-accessories/",
                       "https://www.samsung.com/us/tvs/","https://www.samsung.com/us/hubs/","https://www.samsung.com/us/mobile/","https://www.samsung.com/us/cameras/","https://www.samsung.com/us/tablets/",
                       "https://www.samsung.com/us/watches/","https://www.samsung.com/us/monitors/","https://www.samsung.com/us/trackers/","https://www.samsung.com/us/tv-audio/","https://www.samsung.com/us/notebooks/","https://www.samsung.com/us/smart-home/","https://www.samsung.com/us/chromebooks/",
                       "https://www.samsung.com/us/galaxybooks/","https://www.samsung.com/us/range-hoods/","https://www.samsung.com/us/smartphones/","https://www.samsung.com/us/smartthings/","https://www.samsung.com/us/memory-storage/",
                       "https://www.samsung.com/us/tv-accessories/","https://www.samsung.com/us/mobile-accessories/","https://www.samsung.com/us/computing-accessories/"),'PCD',
      Address %chin% c("https://www.samsung.com/africa_en/","https://www.samsung.com/africa_fr/","https://www.samsung.com/africa_pt/","https://www.samsung.com/al/","https://www.samsung.com/ar/","https://www.samsung.com/au/","https://www.samsung.com/at/","https://www.samsung.com/be/","https://www.samsung.com/be_fr/","https://www.samsung.com/br/","https://www.samsung.com/bg/","https://www.samsung.com/ca/","https://www.samsung.com/ca_fr/","https://www.samsung.com/cl/","https://www.samsung.com/cn/","https://www.samsung.com/co/","https://www.samsung.com/hr/","https://www.samsung.com/cz/","https://www.samsung.com/dk/","https://www.samsung.com/eg/","https://www.samsung.com/ee/","https://www.samsung.com/fi/","https://www.samsung.com/fr/","https://www.samsung.com/de/","https://www.samsung.com/gr/","https://www.samsung.com/hk/","https://www.samsung.com/hk_en/","https://www.samsung.com/hu/","https://www.samsung.com/in/","https://www.samsung.com/id/","https://www.samsung.com/iran/","https://www.samsung.com/ie/","https://www.samsung.com/il/","https://www.samsung.com/it/","https://www.samsung.com/kz_kz/","https://www.samsung.com/kz_ru/","https://www.samsung.com/sec/","https://www.samsung.com/lv/","https://www.samsung.com/levant/","https://www.samsung.com/levant_ar/","https://www.samsung.com/lt/","https://www.samsung.com/mk/","https://www.samsung.com/my/","https://www.samsung.com/mx/","https://www.samsung.com/mm/","https://www.samsung.com/nl/","https://www.samsung.com/nz/","https://www.samsung.com/n_africa/","https://www.samsung.com/no/","https://www.samsung.com/pk/","https://www.samsung.com/latin/","https://www.samsung.com/latin_en/","https://www.samsung.com/py/","https://www.samsung.com/pe/","https://www.samsung.com/ph/","https://www.samsung.com/pl/","https://www.samsung.com/pt/","https://www.samsung.com/ro/","https://www.samsung.com/ru/","https://www.samsung.com/sa/","https://www.samsung.com/sa_en/","https://www.samsung.com/rs/","https://www.samsung.com/sg/","https://www.samsung.com/sk/","https://www.samsung.com/si/","https://www.samsung.com/za/","https://www.samsung.com/es/","https://www.samsung.com/se/","https://www.samsung.com/ch/","https://www.samsung.com/ch_fr/","https://www.samsung.com/tw/","https://www.samsung.com/th/","https://www.samsung.com/tr/","https://www.samsung.com/ae/","https://www.samsung.com/ae_ar/","https://www.samsung.com/uk/","https://www.samsung.com/ua/","https://www.samsung.com/uy/","https://www.samsung.com/uz_uz/","https://www.samsung.com/uz_ru/","https://www.samsung.com/us/","https://www.samsung.com/vn/","https://www.samsung.com/ba/","https://www.samsung.com/ps/","https://www.samsung.com/bd/","https://www.samsung.com/az/") ,'Home'
      
    )]
    raw <- raw[!is.na(pagetype_detail),.(Address,pagetype_detail)]
   raw
  },.progress=TRUE) %>% rbindlist
  save_csv(res,filename='page-type-byFrog')
  res
}
