setwd("C:\\Research\\Bank_Data\\Bank_Full_Data_Small_Firms - 5_1000_Eurozone\\Data")

load(file = "bank.df.raw.RData")
load(file = "germany.raw.RData")
load(file = "austria.raw.RData")
load(file = "finland.raw.RData")
load(file = "france.raw.RData")
load(file = "ireland.raw.RData")
load(file = "italy.raw.RData")
load(file = "slovenia.raw.RData")
load(file = "spain.raw.RData")
load(file = "belgium.raw.RData")
load(file = "croatia.raw.RData")
load(file = "cyprus.raw.RData")
load(file = "estonia.raw.RData")
load(file = "greece.raw.RData")
load(file = "latvia.raw.RData")
load(file = "lithuania.raw.RData")
load(file = "luxemburg.raw.RData")
load(file = "malta.raw.RData")
load(file = "netherlands.raw.RData")
load(file = "portugal.raw.RData")


# load library
library(rlang)
library(lubridate)
library(plyr) 
library(dplyr)
library(tibble)
library(tidyverse)
library(tidyr)

# bank data 
bank.df.raw$SELLER_CountryCode <- bank.df.raw$SELLER_COUNTRY
bank.df.raw$BUYER_CountryCode <- bank.df.raw$BUYER_COUNTRY
bank.df.raw <- rename(bank.df.raw, c("posterior.terms" = "LOAN_TENOR"))

# change buyer and seller country names - have a uniformity for all data set
bank.df.raw$SELLER_COUNTRY  <- gsub("AT", "Austria", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("BE", "Belgium", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("BG", "Bulgaria", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("CH", "Switzerland", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("CN", "China", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("CZ", "Czech Republic", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("DE", "Germany", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("DK", "Denmark", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("EE", "Estonia", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("ES", "Spain", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("FI", "Finland", bank.df.raw$SELLER_COUNTRY ) 
bank.df.raw$SELLER_COUNTRY  <- gsub("FR", "France", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("GB", "United Kingdom", bank.df.raw$SELLER_COUNTRY )#
bank.df.raw$SELLER_COUNTRY  <- gsub("HK", "Hong Kong", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("HR", "Croatia", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("HU", "Hungary", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("IE", "Ireland", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("IT", "Italy", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("KR", "South Korea", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("MX", "Mexico", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("NL", "Netherlands", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("NO", "Norway", bank.df.raw$SELLER_COUNTRY ) #
bank.df.raw$SELLER_COUNTRY  <- gsub("PL", "Poland", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("RO", "Romania", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("SE", "Sweden", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("SI", "Slovenia", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("SK", "Slovak Republic", bank.df.raw$SELLER_COUNTRY )
bank.df.raw$SELLER_COUNTRY  <- gsub("TW", "Taiwan", bank.df.raw$SELLER_COUNTRY ) #

# rename Buyer Country
bank.df.raw$BUYER_COUNTRY <- gsub("AT", "Austria", bank.df.raw$BUYER_COUNTRY) 
bank.df.raw$BUYER_COUNTRY <- gsub("BE", "Belgium", bank.df.raw$BUYER_COUNTRY) 
bank.df.raw$BUYER_COUNTRY <- gsub("CH", "Switzerland", bank.df.raw$BUYER_COUNTRY) #
bank.df.raw$BUYER_COUNTRY <- gsub("DE", "Germany", bank.df.raw$BUYER_COUNTRY) 
bank.df.raw$BUYER_COUNTRY <- gsub("ES", "Spain", bank.df.raw$BUYER_COUNTRY) 
bank.df.raw$BUYER_COUNTRY <- gsub("GB", "United Kingdom", bank.df.raw$BUYER_COUNTRY) #
bank.df.raw$BUYER_COUNTRY <- gsub("HU", "Hungary", bank.df.raw$BUYER_COUNTRY)
bank.df.raw$BUYER_COUNTRY <- gsub("IT", "Italy", bank.df.raw$BUYER_COUNTRY)
bank.df.raw$BUYER_COUNTRY <- gsub("NL", "Netherlands", bank.df.raw$BUYER_COUNTRY)
bank.df.raw$BUYER_COUNTRY <- gsub("RO", "Romania", bank.df.raw$BUYER_COUNTRY) 
bank.df.raw$BUYER_COUNTRY <- gsub("PL", "Poland", bank.df.raw$BUYER_COUNTRY)
bank.df.raw$BUYER_COUNTRY <- gsub("SE", "Sweden", bank.df.raw$BUYER_COUNTRY)
bank.df.raw$BUYER_COUNTRY <- gsub("SI", "Slovenia", bank.df.raw$BUYER_COUNTRY)
bank.df.raw$BUYER_COUNTRY <- gsub("SK", "Slovak Republic", bank.df.raw$BUYER_COUNTRY)
bank.df.raw$BUYER_COUNTRY <- gsub("US", "United States", bank.df.raw$BUYER_COUNTRY) #

# subset only Euro currency transactions
bank.df.raw <- subset(bank.df.raw[bank.df.raw$LOAN_CCY == "EUR",])

# remove countries without EURO currency
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "Bulgaria",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "China",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "Czech Republic",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "Denmark",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "Hong Kong",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "Hungary",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "Norway",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "Poland",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "Romania",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "Sweden",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "Switzerland",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "Taiwan",]
bank.df.raw <- bank.df.raw[!bank.df.raw$SELLER_COUNTRY == "United Kingdom",]

# remove countries from the Buyer country  
bank.df.raw <- bank.df.raw[!bank.df.raw$BUYER_COUNTRY == "United States",]
bank.df.raw <- bank.df.raw[!bank.df.raw$BUYER_COUNTRY == "United Kingdom",]
bank.df.raw <- bank.df.raw[!bank.df.raw$BUYER_COUNTRY == "Switzerland",]
bank.df.raw <- bank.df.raw[!bank.df.raw$BUYER_COUNTRY == "Sweden",]
bank.df.raw <- bank.df.raw[!bank.df.raw$BUYER_COUNTRY == "Poland",]

# change dates to as.Dates
bank.df.raw$LOAN_MATURITYDATE <- as.Date(bank.df.raw$LOAN_MATURITYDATE)
bank.df.raw$LOAN_EFFECTIVEDATE <- as.Date(bank.df.raw$LOAN_EFFECTIVEDATE)
bank.df.raw$Date <- as.Date(bank.df.raw$LOAN_EFFECTIVEDATE)

# add new variables 
bank.df.raw$SID <- as.character(as.numeric(substr(bank.df.raw$SELLER,7,10)))
bank.df.raw$BID <- as.character(as.numeric(substr(bank.df.raw$BUYER,6,9)))
bank.df.raw$TID <- as.character(as.numeric(bank.df.raw$ASSET_ID))
bank.df.raw$DID <- paste(as.character(bank.df.raw$SID),"_",as.character(bank.df.raw$BID),sep="")
bank.df.raw$log.num.invoices <- log(bank.df.raw$NUM_INVOICES)
bank.df.raw$log.amount <- log(bank.df.raw$LOAN_AMT)

# new Date variables
bank.df.raw$year <- lubridate::year(bank.df.raw$LOAN_EFFECTIVEDATE)
bank.df.raw$week <- lubridate::week(bank.df.raw$LOAN_EFFECTIVEDATE)
bank.df.raw$quarter <- zoo::as.yearqtr(bank.df.raw$LOAN_EFFECTIVEDATE, format = "%Y-%m-%d")

# create column for first transaction of each supplier
bank.df.raw <- merge(bank.df.raw, bank.df.raw %>% dplyr::group_by(SID) %>% dplyr::summarise(s.first.transaction = as.Date(min(LOAN_EFFECTIVEDATE))))
bank.df.raw <- merge(bank.df.raw, bank.df.raw %>% group_by(SID) %>% summarise(s.last.transaction = as.Date(max(LOAN_EFFECTIVEDATE))))
bank.df.raw$s.daysInSystem <- as.numeric(as.Date(bank.df.raw$s.last.transaction) - as.Date(bank.df.raw$s.first.transaction))

# create column for first transaction of each buyer
bank.df.raw <- merge(bank.df.raw, bank.df.raw %>% group_by(BID) %>% summarise(b.first.transaction = as.Date(min(LOAN_EFFECTIVEDATE))))
bank.df.raw <- merge(bank.df.raw, bank.df.raw %>% group_by(BID) %>% summarise(b.last.transaction = as.Date(max(LOAN_EFFECTIVEDATE))))
bank.df.raw$b.daysInSystem <- as.numeric(as.Date(bank.df.raw$b.last.transaction) - as.Date(bank.df.raw$b.first.transaction))

# create column for number of buyers per supplier and number of suppliers by buyer
bank.df.raw <- bank.df.raw %>% merge(.,bank.df.raw %>% group_by(SID) %>% dplyr::count(BID) %>% dplyr::count(SID) %>% rename(.,c("buyers.cnt" = "n")))
bank.df.raw <- bank.df.raw %>% merge(.,bank.df.raw %>% group_by(BID) %>% dplyr::count(SID) %>% dplyr::count(BID) %>% rename(.,c("suppliers.cnt" = "n")))

bank.df.raw <- merge(bank.df.raw, bank.df.raw %>% group_by(DID) %>% summarise(d.first.transaction = as.Date(min(LOAN_EFFECTIVEDATE))))
bank.df.raw <- merge(bank.df.raw, bank.df.raw %>% group_by(DID) %>% summarise(d.last.transaction = as.Date(max(LOAN_EFFECTIVEDATE))))
bank.df.raw$d.daysInSystem <- as.numeric(as.Date(bank.df.raw$d.last.transaction) - as.Date(bank.df.raw$d.first.transaction))

#remove columns
bank.df.raw$ASSET_ID <- NULL

# create a variable for transactions year for dyads
bank.df.raw$year.s.first.transaction <- lubridate::year(bank.df.raw$s.first.transaction)

#subset data where the first transaction year is 2018 and 2019
bank.df.raw <- bank.df.raw %>% subset(year.s.first.transaction < 2020)

# remove missing data 
bank.df.raw <- drop_na(bank.df.raw)

# copy data
bank.df <- rlang::duplicate(bank.df.raw, shallow = FALSE)

### 
# clean small firms data 
germany.raw$SELLER_COUNTRY <- "Germany"
austria.raw$SELLER_COUNTRY <- "Austria"
finland.raw$SELLER_COUNTRY <- "Finland"
france.raw$SELLER_COUNTRY <- "France"
ireland.raw$SELLER_COUNTRY <- "Ireland"
italy.raw$SELLER_COUNTRY <- "Italy"
slovenia.raw$SELLER_COUNTRY <- "Slovenia"
spain.raw$SELLER_COUNTRY <- "Spain"
belgium.raw$SELLER_COUNTRY <- "Belgium"
croatia.raw$SELLER_COUNTRY <- "Croatia"
cyprus.raw$SELLER_COUNTRY <- "Cyprus"
estonia.raw$SELLER_COUNTRY <- "Estonia"
greece.raw$SELLER_COUNTRY <- "Greece"
latvia.raw$SELLER_COUNTRY <- "Latvia"
lithuania.raw$SELLER_COUNTRY <- "Lithuania"
malta.raw$SELLER_COUNTRY <- "Malta"
netherlands.raw$SELLER_COUNTRY <- "Netherlands"
portugal.raw$SELLER_COUNTRY <- "Portugal"
luxemburg.raw$SELLER_COUNTRY <- "Luxemburg"

# join data frames
small.firms.raw <- full_join(germany.raw, austria.raw) 
small.firms.raw <- full_join(small.firms.raw, finland.raw) 
small.firms.raw <- full_join(small.firms.raw, france.raw) 
small.firms.raw <- full_join(small.firms.raw, ireland.raw) 
small.firms.raw <- full_join(small.firms.raw, italy.raw) 
small.firms.raw <- full_join(small.firms.raw, slovenia.raw) 
small.firms.raw <- full_join(small.firms.raw, spain.raw) 
small.firms.raw <- full_join(small.firms.raw, belgium.raw) 
small.firms.raw <- full_join(small.firms.raw, croatia.raw) 
small.firms.raw <- full_join(small.firms.raw, cyprus.raw) 
small.firms.raw <- full_join(small.firms.raw, estonia.raw) 
small.firms.raw <- full_join(small.firms.raw, greece.raw) 
small.firms.raw <- full_join(small.firms.raw, latvia.raw) 
small.firms.raw <- full_join(small.firms.raw, lithuania.raw) 
small.firms.raw <- full_join(small.firms.raw, malta.raw) 
small.firms.raw <- full_join(small.firms.raw, netherlands.raw) 
small.firms.raw <- full_join(small.firms.raw, portugal.raw) 
small.firms.raw <- full_join(small.firms.raw, luxemburg.raw) 

# change names 
small.firms.raw <- rename(small.firms.raw, c("SID" = "Name"))
small.firms.raw <- rename(small.firms.raw, c("SELLER_INDUSTRY" = "GICS Ind Grp Name"))

# subset only needed quarters 
small.firms.raw <- subset(small.firms.raw, select = c(SID, SELLER_INDUSTRY, SELLER_COUNTRY,
                                                      Revenue.2019.Q1, Revenue.2019.Q2, Revenue.2019.Q3, Revenue.2019.Q4,
                                                      Revenue.2020.Q1, Revenue.2020.Q2, Revenue.2020.Q3, Revenue.2020.Q4,
                                                      AR.2019.Q1, AR.2019.Q2, AR.2019.Q3, AR.2019.Q4))

small.firms.raw[small.firms.raw == 0] <- NA

# only complete cases for Q4 2019
small.firms.raw <- small.firms.raw[complete.cases(small.firms.raw[,"Revenue.2019.Q4"]),]

# create payment terms variable 
small.firms.raw$payment.terms.2019.Q1 <- (small.firms.raw$AR.2019.Q1/small.firms.raw$Revenue.2019.Q1)*(365/4)
small.firms.raw$payment.terms.2019.Q2 <- (small.firms.raw$AR.2019.Q2/small.firms.raw$Revenue.2019.Q2)*(365/4)
small.firms.raw$payment.terms.2019.Q3 <- (small.firms.raw$AR.2019.Q3/small.firms.raw$Revenue.2019.Q3)*(365/4)
small.firms.raw$payment.terms.2019.Q4 <- (small.firms.raw$AR.2019.Q4/small.firms.raw$Revenue.2019.Q4)*(365/4)

# put previous payment terms
small.firms.raw$payment.terms.2019.Q4[is.na(small.firms.raw$payment.terms.2019.Q4)] <- 
  small.firms.raw$payment.terms.2019.Q3[is.na(small.firms.raw$payment.terms.2019.Q4)]

small.firms.raw$payment.terms.2019.Q4[is.na(small.firms.raw$payment.terms.2019.Q4)] <- 
  small.firms.raw$payment.terms.2019.Q2[is.na(small.firms.raw$payment.terms.2019.Q4)]

small.firms.raw$payment.terms.2019.Q4[is.na(small.firms.raw$payment.terms.2019.Q4)] <- 
  small.firms.raw$payment.terms.2019.Q1[is.na(small.firms.raw$payment.terms.2019.Q4)]

# subset needed columns
small.firms.raw <- subset(small.firms.raw, select = c(SID, SELLER_INDUSTRY, SELLER_COUNTRY,Revenue.2019.Q4,
                                                      Revenue.2020.Q1, Revenue.2020.Q2, Revenue.2020.Q3, Revenue.2020.Q4,
                                                      payment.terms.2019.Q4))
# 
small.firms.raw <- small.firms.raw[complete.cases(small.firms.raw[,"payment.terms.2019.Q4"]),]

small.firms.raw[is.na(small.firms.raw)] <- 0

small.firms.raw <- drop_na(small.firms.raw)
small.firms <- rlang::duplicate(small.firms.raw, shallow = FALSE)

# save all data 
save(bank.df, file = "bank.df.RData")
save(small.firms, file = "small.firms.RData")