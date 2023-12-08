setwd("C:\\Research\\Bank_Data\\Bank_Full_Data_Small_Firms - 5_1000_Eurozone\\Data")

#load data
load(file="bank.merged.RData")
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

# create three years period aggregate the date 
bank.merged.cox <- unique(bank.merged %>% group_by(SID,quarter) %>% dplyr::summarize(quarterSpend. = sum(LOAN_AMT)) %>% 
                transmute(quarter = paste("quarterSpend.",quarter,sep=""),
                          quarterSpend.=quarterSpend.) %>% spread(quarter,quarterSpend.))

# only leave complete cases for Q4 2019 
bank.merged.cox <- bank.merged.cox[complete.cases(bank.merged.cox[,2]),]

# make NA values in Q1 2020- Q4 2020 zeros
bank.merged.cox[is.na(bank.merged.cox)] <- 0

# add Seller industry and country
df <- bank.merged %>% select(SID, SELLER_INDUSTRY, SELLER_COUNTRY) 
df <- unique(df)

# merged with the cox dataframe
bank.merged.cox <- merge(bank.merged.cox, df, by ="SID", all.x=T)

# rename columns 
bank.merged.cox <- rename(bank.merged.cox, "1" = `quarterSpend.2019 Q4`)
bank.merged.cox <- rename(bank.merged.cox, "2" = `quarterSpend.2020 Q1`)
bank.merged.cox <- rename(bank.merged.cox, "3" = `quarterSpend.2020 Q2`)
bank.merged.cox <- rename(bank.merged.cox, "4" = `quarterSpend.2020 Q3`)
bank.merged.cox <- rename(bank.merged.cox, "5" = `quarterSpend.2020 Q4`)

# create a treatment variable 
bank.merged.cox$treatment <- 1

## add payment terms to the main dataframe 
bank.merged.cox.PT <- unique(bank.merged %>% group_by(SID,quarter) %>% dplyr::summarize(quarterPT. = mean(posterior.terms)) %>% 
                               transmute(quarter = paste("quarterPT.",quarter,sep=""), quarterPT.=quarterPT.) %>% spread(quarter,quarterPT.))

# subset only Q4 2019 
bank.merged.cox.PT <- subset(bank.merged.cox.PT, select = c(SID, `quarterPT.2019 Q4`))

# drop NA values for Q4 2019
bank.merged.cox.PT <- drop_na(bank.merged.cox.PT)

#rename the variable
bank.merged.cox.PT <- rename(bank.merged.cox.PT, "payment.terms" = "quarterPT.2019 Q4")

# merge with the main dataframe
bank.merged.cox <- merge(bank.merged.cox, bank.merged.cox.PT, by = c("SID"), all = T)

## small firms in the Eurozone
small.firms.revenue <- small.firms

# rename columns 
small.firms.revenue <- rename(small.firms.revenue, "1" = Revenue.2019.Q4)
small.firms.revenue <- rename(small.firms.revenue, "2" = Revenue.2020.Q1)
small.firms.revenue <- rename(small.firms.revenue, "3" = Revenue.2020.Q2)
small.firms.revenue <- rename(small.firms.revenue, "4" = Revenue.2020.Q3)
small.firms.revenue <- rename(small.firms.revenue, "5" = Revenue.2020.Q4)

# rename payment terms
small.firms.revenue <- rename(small.firms.revenue, "payment.terms" = "payment.terms.2019.Q4")

# create treatment variable 
small.firms.revenue$treatment <- 0

# merge small firms and bank data 
cox.revenue <- full_join(bank.merged.cox, small.firms.revenue)

# create True False variable for to assess whether the transaction volume was lower or bigger 
cox.revenue$Q1TF <- ifelse(cox.revenue$`2` > cox.revenue$`1`, T,F)
cox.revenue$Q2TF <- ifelse(cox.revenue$`3` > cox.revenue$`1`, T,F)
cox.revenue$Q3TF <- ifelse(cox.revenue$`4` > cox.revenue$`1`, T,F)
cox.revenue$Q4TF <- ifelse(cox.revenue$`5` > cox.revenue$`1`, T,F)

### which quarter it got back to normal variable
cox.revenue$which.quarter.1 <- ifelse(cox.revenue$Q1TF * 1, 1, NA)
cox.revenue$which.quarter.2 <- ifelse(cox.revenue$Q2TF * 2, 2, NA)
cox.revenue$which.quarter.3 <- ifelse(cox.revenue$Q3TF * 3, 3, NA)
cox.revenue$which.quarter.4 <- ifelse(cox.revenue$Q4TF * 4, 4, NA)

# create minimum variable 
my_min <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA)

cox.revenue$time.to.normal = apply(cox.revenue[c("which.quarter.1", "which.quarter.2", "which.quarter.3", "which.quarter.4")], 1, my_min)

# create status variable: reached normal or not
cox.revenue$status <- ifelse(cox.revenue$time.to.normal >= 1, 1,0)
cox.revenue[is.na(cox.revenue$time.to.normal),]$time.to.normal <- 5

#if never reached "normal" - set status to 0
cox.revenue[is.na(cox.revenue$status),]$status <- 0

# add adverse impact variable
cox.revenue$adverse.impact <- (-(cox.revenue$`2` - cox.revenue$`1`)/cox.revenue$`1`)*100
cox.revenue$scaled.adverse.impact <- scale(cox.revenue$adverse.impact)

# subset only needed columns 
cox.revenue.final.pt <- subset(cox.revenue, select = c(SID, SELLER_INDUSTRY, SELLER_COUNTRY, time.to.normal, status, adverse.impact, scaled.adverse.impact, treatment, payment.terms))

# scale and log of payment terms
cox.revenue.final.pt$scaled.pt <- scale(cox.revenue.final.pt$payment.terms)
cox.revenue.final.pt$log.pt <- log(cox.revenue.final.pt$payment.terms)

#save the file 
save(cox.revenue.final.pt, file = "cox.revenue.final.pt.RData")