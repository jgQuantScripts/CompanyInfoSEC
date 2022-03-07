# https://www.sec.gov/edgar/sec-api-documentation
# https://www.osha.gov/data/sic-manual
# https://www.sec.gov/corpfin/division-of-corporation-finance-standard-industrial-classification-sic-code-list
# load packages
require("jsonlite"); require("data.table");require("httr");require("pbapply");require("stringr");require("plyr")
# assign user agent
PASS <- new.env()
assign("usrAgent","companyname.com email@companyName.com",env=PASS)
# ****************************************************************************************************************
#                                        Read in CIK Codes : 
# ****************************************************************************************************************
# read in list of CIK codes
INFO <- read_json("https://www.sec.gov/files/company_tickers.json")
INFO <- rbindlist(INFO)
# CIK numbers are 10 digits - we have to fill in with zeros
INFO$CIK = do.call(rbind, lapply(as.list(1:nrow(INFO)), function(ii){
  ZEROS = 10-as.numeric(str_count(INFO$cik_str[ii]))
  paste0(c(rep(0,ZEROS),INFO$cik_str[ii]), collapse = "")
}))
INFO <- as.data.frame(INFO)
# *****************************************************************************************************************
# *****************************************************************************************************************
# function to lookup CIK number by ticker
getCIK = function(symbol){
  subset(INFO, INFO$ticker == paste(symbol))$CIK
}
# *****************************************************************************************************************
# *****************************************************************************************************************
sicLookUp  = read.csv("sic.csv",sep=",")
getMG= function(sic){subset(sicLookUp, sicLookUp$SIC.Code == sic)$MAJOR.GROUP}
getDIV= function(sic){subset(sicLookUp, sicLookUp$SIC.Code == sic)$DIVISION}
# *****************************************************************************************************************
# *****************************************************************************************************************
# gets ticker sector by ticker
getTickerSIC= function(ticker)
{
  # get CIK # for ticker
  CIK = getCIK(ticker)
  # get data by passing in url & headers
  pg <- GET(url = paste0("https://data.sec.gov/submissions/CIK",CIK,".json"),
            config = httr::add_headers(`User-Agent` = PASS$usrAgent,
                                       `Accept-Encoding` = 'gzip, deflate'))
  
  # raw data
  data_raw <- try(content(pg, as="text", encoding="UTF-8") %>% fromJSON(pg, flatten=FALSE),silent = TRUE)
  # ********************************************************************************************************
  #                                     EXTRACT COMPANY INFO
  # ********************************************************************************************************
  MG <- getMG(data_raw$sic)
  DIV <- getDIV(data_raw$sic)
  ALL <- as.data.frame(cbind(paste(ticker),str_to_title(data_raw$name),CIK,data_raw$entityType,
                             data_raw$exchanges[[1]],data_raw$tickers,data_raw$sic, MG, DIV,
                             data_raw$sicDescription))
 
  colnames(ALL) <- c("currentTicker","companyName","cik", "entityType", "exhanges", "tickers", 
                     "sic","majorGroup","division","industry")
  
  # return data frame
  ALL
}
# *****************************************************************************************************************
# *****************************************************************************************************************
# all tickers from CIK table
tickers = unique(INFO$ticker)


ALL = pblapply(as.list(tickers), function(x){
  tmp <- try(getTickerSIC(ticker=x), silent = TRUE)
  if(!inherits(tmp,'try-error')) tmp
})

ALL <- rbindlist(ALL, use.names = TRUE, fill = TRUE) %>% as.data.frame()

saveRDS(ALL,"sectorIndustryList.rds")
