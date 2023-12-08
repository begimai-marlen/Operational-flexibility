setwd("C:\\Research\\Bank_Data\\Bank_Full_Data_Small_Firms - 5_1000_Eurozone\\Data")

#load data
load(file="bank.df.RData")
load(file="small.firms.RData")

#load libraries
library(lubridate)
library(plyr) 
library(dplyr)
library(zoo)
library(tibble)
library(utils)
library(rio)
library(naniar)
library(tidyverse)
library(tidyr)
library(car)

#restore order
bank.df <- bank.df[order(bank.df$LOAN_EFFECTIVEDATE),]

# create dyads dataframe
dyads.df <- unique(bank.df %>% select(starts_with(c("DID","d.daysInSystem", "d.first.transaction","d.last.transaction",
                                                    "SID","BID","BUYER_COUNTRY", "BUYER_INDUSTRY", 
                                                    "SELLER_COUNTRY", "SELLER_INDUSTRY"))))

# add annual spend for each dyad
dyads.df <- unique(merge(dyads.df,(bank.df %>% group_by(DID,year) %>% dplyr::summarize(annualSpend = sum(LOAN_AMT)) %>% 
                                     transmute(year = paste("annualSpend.",year,sep=""),
                                               annualSpend=annualSpend) %>% spread(year,annualSpend))))

# remove annual spends for 2018, 2021, 2022
dyads.df$annualSpend.2022 <- NULL
dyads.df$annualSpend.2021 <- NULL
dyads.df$annualSpend.2018 <- NULL

# remove NA values for the annual spend in 2019 and 2020
dyads.df <- drop_na(dyads.df)

# only leave suppliers with their largest buyer
df <- dyads.df %>% group_by(SID) %>% summarise(annualSpend.2019 = max(annualSpend.2019))

dyads.df <- dyads.df[dyads.df$annualSpend.2019 %in% df$annualSpend.2019,]

# bank data frame will be required for payment terms 

# cut unnecessary dyads in bank.merged
bank.merged <- merge(bank.df, dyads.df, by=(c("SELLER_INDUSTRY", "SID", "SELLER_COUNTRY")), all.x = T)
bank.merged <- drop_na(bank.merged)

# cutting the data Q4 2019 - Q4 2020
bank.merged <- bank.merged[bank.merged$Date >= as.Date("2019-10-01"),]
bank.merged <- bank.merged[bank.merged$Date < as.Date("2021-01-01"),]

# remove duplicates
dyads.df <- unique(dyads.df)

# save the file 
save(dyads.df, file = "dyads.df.RData")
save(bank.merged, file = "bank.merged.RData")