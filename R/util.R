save_csv <- function(...,filename="",dir,col_names=TRUE) 
{
  if (missing(filename)) filename <- gsub(" |:","-",paste0(Sys.time()))
  data <- data.table(...)
  names(data) <- enc2utf8(names(data))
  cn <- which(map_lgl(names(data),~is.character(data[,get(.x)])))
  dn <- which(map_lgl(names(data),~is.Date(data[,get(.x)]) | is.POSIXct(data[,get(.x)])))
  if(length(cn) >= 1) { data[,`:=`((names(data)[cn]),lapply(.SD,enc2utf8)),.SDcol=names(data)[cn]] }
  if(length(dn) >= 1) { data[,`:=`((names(data)[dn]),lapply(.SD,as.Date)),.SDcol=names(data)[dn]] }
  if(missing(dir)) { dir <- choose.dir(default=getwd(),caption="저장할 위치를 선택하세요") }
  file_dir_name <- enc2native(file.path(dir,paste0(filename,".csv")))
  if (file.exists(file_dir_name)) {
    message("같은 이름의 파일이 존재해요. 파일 이름에 생성 시간이 추가됩니다!")
    file_dir_name <- enc2native(file.path(dir,paste0(filename,"_",gsub(" |:","-",paste0(Sys.time())),".csv")))
  }
  fwrite(data,file_dir_name,bom=TRUE,col.names=col_names)
}

file_copy <- function(...,dir) 
{
  file_list <- normalizePath(...)
  Encoding(file_list) <- "UTF-8"
  if (missing(dir)) {
    ran_chr <- paste0(sample(letters,size=7),collapse="")
    ran_num <- round(runif(n=1,min=0,max=9)*1e+06)
    dir_raname <- ifelse(rbinom(n=1,size=1,prob=0.5)>=0.5,paste0(ran_chr,ran_num),paste0(ran_num,ran_chr))
  }
  else {
    dir_raname <- dir
  }
  if (dir.exists(paste0("C:/",dir_raname))) {
    unlink(paste0("C:/",dir_raname),recursive=T)
  }
  dir.create(paste0("C:/",dir_raname))
  map(file_list,~ {
    file.copy(from=.x,to=file.path(paste0("C:/",dir_raname),gsub("[^A-z0-9|\\.]","",basename(.x))))
  }, .progress=TRUE)
  dir_raname
}

db_upload <- function(dbname,dbuser,dbpw,dbhost,dbport,table,...) 
{
  raw <- data.table(...)
  conn <- dbConnect(dbDriver("MySQL"),dbname=dbname,
                    user=dbuser,password=dbpw,host=dbhost,
                    port=dbport)
  # dbSendQuery(a,"set character set \"euckr\"")
  if(sum(dbListTables(conn)==table)>0) {
    if (askYesNo(paste(table,"이라는 테이블이 이미 존재해요! 기존 테이블의 마지막 부분부터 현재 자료를 추가하시겠습니까? (아니오를 선택하면 데이터가 덮어 씌워집니다)"))==TRUE) {
      dbWriteTable(conn,table,raw,row.names=F,append=TRUE)
    }
    else {
      dbWriteTable(conn,table,raw,row.names=F,overwrite=TRUE)
    }
  }
  else {
    dbWriteTable(conn,table,raw,row.names=F)
  }
  dbDisconnect(conn)
}

db_read <- function(dbname,dbuser,dbpw,dbhost,dbport,table,...) 
{
  conn <- dbConnect(dbDriver("MySQL"),dbname=dbname,
                    user=dbuser,password=dbpw,host=dbhost,
                    port=dbport)
  if(missing(table)) { table <- select.list(dbListTables(conn),graphics=TRUE,title="가져올 테이블을 선택하세요") }
  if(sum(dbListTables(conn)==table)==0) {
    message(paste(table,"이라는 테이블이 존재하지 않아요! 리스트에서 골라주세요."))
    table <- select.list(dbListTables(conn),graphics=TRUE,title="가져올 테이블을 선택하세요")
  }
  condition <- rlang::enexprs(...)
  temp <- data.table(suppressWarnings(collect(filter(tbl(conn,table),condition))))
  dbDisconnect(conn)
  assign(x="db_read_table_name",value=table,.GlobalEnv)
  temp
}

db_drop <- function(dbname,dbuser,dbpw,dbhost,dbport,table) 
{
  conn <- dbConnect(dbDriver("MySQL"),dbname=dbname,
                    user=dbuser,password=dbpw,host=dbhost,
                    port=dbport)
  if (!missing(table)) {
    dbSendQuery(conn,paste0("drop table ",table,""))
    message(paste(table,"테이블이 삭제되었습니다."))
  }
  else {
    drop_list <- select.list(dbListTables(conn),graphics=TRUE,multiple=TRUE,title="삭제할 테이블을 선택하세요(다중선택 가능)")
    lapply(drop_list,function(x) {
      dbSendQuery(conn,paste0("drop table ",x,""))
      message(paste0(x,"테이블이 삭제되었습니다."))
    })
  }
  dbDisconnect(conn)
}
