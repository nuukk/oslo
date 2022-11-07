lan_detect_key <- function (...,keyfile,save=c("Yes","No")) 
{
  keyword <- as.character(...)
  if (!missing(keyfile)) 
    tryCatch(gl_auth(keyfile),error=function(e) {
      gl_auth(choose.files("구글 서비스 계정 키 정보가 기록된 JSON 파일을 선택하세요"))
    })
  if (missing(keyfile)) 
    gl_auth(choose.files(caption="구글 서비스 계정 키 정보가 기록된 JSON 파일을 선택하세요"))
  if(missing(save)) save <- "Yes"
  save <- match.arg(save)
  temp <- gl_translate_detect(keyword)
  if (save == "Yes") {
    save_csv(temp,filename="lan-detect")
  }
  temp
}

lan_detect_apikey <- function(...,apikey,time=0,save=c("Yes","No")) 
{
  if(missing(save)) save <- "Yes"
  save <- match.arg(save)
  keyword <- as.character(...)
  result <- data.table()
  for(i in seq_along(keyword)) {
    temp <- tryCatch(data.table(Keyword=keyword[[i]],
                                Language=parse_json(POST(paste0("https://translation.googleapis.com/language/translate/v2/detect?key=",apikey),body=list(q=keyword[[i]]),encode="json"))$data$detections[[1]][[1]]$language),
                     error=function(e) {
                       data.table(Keyword=keyword[[i]],Language=NA)
                     })
    result <- bind_rows(result,temp)
    Sys.sleep(time)
  }
  if(save == "Yes") {
    save_csv(result,filename="lan-detect")
  }
  result
}
