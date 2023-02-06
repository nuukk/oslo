googletrendscrawling <- function(keyword,geo,date) 
{
  geo <- toupper(geo)
  if(missing(date)) date <- paste("2015-01-01", floor_date(Sys.Date(), "mon")-days(1))
  res <- tryCatch(gtrends(keyword=keyword,geo=geo,time=date,onlyInterest = TRUE)$interest_over_time,
                  error = function(e) {})
  if(is.null(res)) {
    return(data.table(date = as.POSIXct("1900-01-02 GMT"), 
                      hits = as.character(-100), keyword=keyword,geo=geo, 
                      time = as.character("1900-01-01 1900-01-01"), gprop = as.character("NA"), 
                      category = as.integer(0)))
  }
  res$date <- as.POSIXct(res$date)
  res$hits <- as.character(res$hits)
  res$time <- as.character(res$time)
  setDT(res)
  res
}

google_trends_scapper <- function(start_date,end_date,keyword,country,new_name,capture_path=NULL) {
  if(missing(start_date)) start_date <- '2017-12-01'
  if(missing(end_date)) end_date <- Sys.Date()-days(4)
  keyword <- gsub(" ","%20",keyword)
  country <- toupper(country)
  url <- paste0('https://trends.google.com/trends/explore?date=',start_date,'%20',end_date,'&geo=',country,'&q=',keyword)
  Sys.sleep(1)
  remDr$navigate(url)
  Sys.sleep(1.5)
  try({
    tryCatch({
      remDr$findElement(using='class name',value='widget-actions-item')$clickElement()
    }, error=function(e) {
      Sys.sleep(1.5)
      remDr$findElement(using='class name',value='widget-actions-item')$clickElement()
    })
    #rename
    Sys.sleep(1)
    is_finish <- length(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                   pattern='multiTimeline.csv'))
    while(is_finish==0) {
      Sys.sleep(0.5)
      is_finish <- length(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                     pattern='multiTimeline.csv'))
    }
    Sys.sleep(0.8)
    file.rename(from=map(list.files((file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads")),
                                    pattern='multiTimeline',full.names=T), ~ data.table(file=.x,time=file.mtime(.x))) %>%
                  rbindlist %>% slice_max(time,n=1L) %>% pull(file),
                to=file.path("C:","Users",Sys.getenv("USERNAME"),"Downloads",paste0(new_name,'.csv')))
    if(!is.null(capture_path)) {
      remDr$screenshot(file=file.path(capture_path,paste0(new_name,'.jpg')))
    }
  })
}

google_trends_read <- function(file_list,gbm=NULL,product_type=NULL) {
  if(missing(file_list)) file_list <- choose.files(caption='Google Trends RAW 파일을 선택하세요')
  if(dir.exists(file_list[[1]])) file_list <- list.files(file_list,full.names=T)
  file_list <- normalizePath(file_list)
  google_country_code <- data.table(kr_country_name=c('가나','가봉','가이아나','감비아','건지','과들루프','과테말라','괌','그레나다','그리스','그린란드','기니','기니비사우','나미비아','나우루','나이지리아','남극 대륙','남수단','남아프리카','네덜란드','네덜란드령 카리브','네팔','노르웨이','노퍽섬','뉴질랜드','뉴칼레도니아','니우에','니제르','니카라과','대만','대한민국','덴마크','도미니카','도미니카 공화국','독일','동티모르','라오스','라이베리아','라트비아','러시아','레바논','레소토','레위니옹','루마니아','룩셈부르크','르완다','리비아','리투아니아','리히텐슈타인','마다가스카르','마르티니크','마셜 제도','마요트','마카오','말라위','말레이시아','말리','맨섬','멕시코','모나코','모로코','모리셔스','모리타니','모잠비크','몬테네그로','몬트세라트','몰도바','몰디브','몰타','몽골','미국','미국령 버진아일랜드','미국령 해외 제도','미얀마','미크로네시아','바누아투','바레인','바베이도스','바티칸 시국','바하마','방글라데시','버뮤다','베냉','베네수엘라','베트남','벨기에','벨라루스','벨리즈','보스니아 헤르체고비나','보츠와나','볼리비아','부룬디','부르키나파소','부베섬','부탄','북마리아나제도','북마케도니아','북한','불가리아','브라질','브루나이','사모아','사우디아라비아','사우스조지아 사우스샌드위치 제도','산마리노','상투메 프린시페','생마르탱','생바르텔레미','생피에르 미클롱','서사하라','세네갈','세르비아','세이셸','세인트루시아','세인트빈센트그레나딘','세인트키츠 네비스','세인트헬레나','소말리아','솔로몬 제도','수단','수리남','스리랑카','스발바르제도-얀마웬섬','스웨덴','스위스','스페인','슬로바키아','슬로베니아','시리아','시에라리온','신트마르턴','싱가포르','아랍에미리트','아루바','아르메니아','아르헨티나','아메리칸 사모아','아이슬란드','아이티','아일랜드','아제르바이잔','아프가니스탄','안도라','알바니아','알제리','앙골라','앤티가 바부다','앵귈라','에리트리아','에스와티니','에스토니아','에콰도르','에티오피아','엘살바도르','영국','영국령 버진아일랜드','영국령 인도양 식민지','예멘','오만','오스트레일리아','오스트리아','온두라스','올란드 제도','왈리스-푸투나 제도','요르단','우간다','우루과이','우즈베키스탄','우크라이나','이라크','이란','이스라엘','이집트','이탈리아','인도','인도네시아','일본','자메이카','잠비아','저지','적도 기니','조지아','중국','중앙 아프리카 공화국','지부티','지브롤터','짐바브웨','차드','체코','칠레','카메룬','카보베르데','카자흐스탄','카타르','캄보디아','캐나다','케냐','케이맨 제도','코모로','코소보','코스타리카','코코스 제도','코트디부아르','콜롬비아','콩고-브라자빌','콩고-킨샤사','쿠바','쿠웨이트','쿡 제도','퀴라소','크로아티아','크리스마스섬','키르기스스탄','키리바시','키프로스','타지키스탄','탄자니아','태국','터크스 케이커스 제도','터키','토고','토켈라우','통가','투르크메니스탄','투발루','튀니지','트리니다드 토바고','파나마','파라과이','파키스탄','파푸아뉴기니','팔라우','팔레스타인','페로 제도','페루','포르투갈','포클랜드 제도(말비나스 군도)','폴란드','푸에르토리코','프랑스','프랑스령 기아나','프랑스령 남방 지역','프랑스령 폴리네시아','피지','핀란드','필리핀','핏케언 제도','허드 맥도널드 제도','헝가리','홍콩'),
                                    google_code=c('GH','GA','GY','GM','GG','GP','GT','GU','GD','GR','GL','GN','GW','NA','NR','NG','AQ','SS','ZA','NL','BQ','NP','NO','NF','NZ','NC','NU','NE','NI','TW','KR','DK','DM','DO','DE','TL','LA','LR','LV','RU','LB','LS','RE','RO','LU','RW','LY','LT','LI','MG','MQ','MH','YT','MO','MW','MY','ML','IM','MX','MC','MA','MU','MR','MZ','ME','MS','MD','MV','MT','MN','US','VI','UM','MM','FM','VU','BH','BB','VA','BS','BD','BM','BJ','VE','VN','BE','BY','BZ','BA','BW','BO','BI','BF','BV','BT','MP','MK','KP','BG','BR','BN','WS','SA','GS','SM','ST','MF','BL','PM','EH','SN','RS','SC','LC','VC','KN','SH','SO','SB','SD','SR','LK','SJ','SE','CH','ES','SK','SI','SY','SL','SX','SG','AE','AW','AM','AR','AS','IS','HT','IE','AZ','AF','AD','AL','DZ','AO','AG','AI','ER','SZ','EE','EC','ET','SV','GB','VG','IO','YE','OM','AU','AT','HN','AX','WF','JO','UG','UY','UZ','UA','IQ','IR','IL','EG','IT','IN','ID','JP','JM','ZM','JE','GQ','GE','CN','CF','DJ','GI','ZW','TD','CZ','CL','CM','CV','KZ','QA','KH','CA','KE','KY','KM','XK','CR','CC','CI','CO','CG','CD','CU','KW','CK','CW','HR','CX','KG','KI','CY','TJ','TZ','TH','TC','TR','TG','TK','TO','TM','TV','TN','TT','PA','PY','PK','PG','PW','PS','FO','PE','PT','FK','PL','PR','FR','GF','TF','PF','FJ','FI','PH','PN','HM','HU','HK'))
  map(file_list, ~ {
    res <- fread(.x,encoding='UTF-8',header=F)
    geo <- google_country_code[kr_country_name==gsub(".* \\(|\\)","",res$V2[1]),google_code]
    if(nrow(res)>=2) {
      for(i in seq_along(names(res)[-1])) {
        res[[names(res)[-1][i]]][1] <- gsub(": \\(.*","",res[[names(res)[-1][i]]][1])
      }
      res <- res %>% melt(id='V1')
      n <- data.table(start=which(str_detect(res$V1,'주|월|일'))) %>% mutate(end=lead(start,n=1L,default=nrow(res)+1)-1)
      res[,keyword:=NA]
      for(i in seq_len(nrow(n))) {
        res$keyword[n$start[i]:n$end[i]] <- res$value[n$start[i]]
      }
      res <- res[!V1 %chin% c('월','주','일')]
      res[,geo:=geo]
      res <- res[,.(date=V1,geo,keyword,hits=value)]
    } else {
      for(i in seq_along(names(res)[-1])) {
        res[[names(res)[-1][i]]][1] <- gsub(": \\(.*","",res[[names(res)[-1][i]]][1])
      }
      res <- bind_rows(res,data.table(NA))
      res <- res %>% melt(id='V1')
      n <- data.table(start=which(str_detect(res$V1,'주|월|일'))) %>% mutate(end=lead(start,n=1L,default=nrow(res)+1)-1)
      res[,keyword:=NA]
      for(i in seq_len(nrow(n))) {
        res$keyword[n$start[i]:n$end[i]] <- res$value[n$start[i]]
      }
      res <- res[!V1 %chin% c('월','주','일')]
      res[,geo:=geo]
      res <- res[,.(date=V1,geo,keyword,hits=value)]
    }
    if(!is.null(gbm)) { res <- res %>% mutate(GBM=gsub(gbm,"",basename(.x))) %>% relocate(GBM, .before=keyword) }
    if(!is.null(product_type)) { res <- res %>% mutate(Product_type=gsub(product_type,"",basename(.x))) %>% relocate(Product_Type, .before=keyword)}
    res
  }) %>% rbindlist -> res
  res
}
