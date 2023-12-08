setwd("C:\\Research\\Bank_Data\\Bank_Full_Data_Small_Firms - 5_1000_Eurozone\\Data")

load(file="cox.revenue.final.pt.RData")

library(Rcpp)
library(lubridate)
library(plyr) 
library(dplyr)
library(survival)
library(sandwich)
library(utils)
library(rio)
library(naniar)
library(tidyverse)
library(lmtest)
library(tableone)
library(MatchIt)
library(optmatch)
library(coxrobust)
library(coxphw)
library(stargazer)

#drop industries in small firms dataframe 
cox.revenue.final.pt <- cox.revenue.final.pt[!cox.revenue.final.pt$SELLER_INDUSTRY == "N/A",]
cox.revenue.final.pt <- cox.revenue.final.pt[!cox.revenue.final.pt$SELLER_INDUSTRY == "Real Estate", ]
cox.revenue.final.pt <- cox.revenue.final.pt[!cox.revenue.final.pt$SELLER_INDUSTRY == "Retailing", ]
cox.revenue.final.pt <- cox.revenue.final.pt[!cox.revenue.final.pt$SELLER_INDUSTRY == "Utilities", ]
cox.revenue.final.pt <- cox.revenue.final.pt[!cox.revenue.final.pt$SELLER_INDUSTRY == "Diversified Financials",]
cox.revenue.final.pt <- cox.revenue.final.pt[!cox.revenue.final.pt$SELLER_INDUSTRY == "Household & Personal Products",]
cox.revenue.final.pt <- cox.revenue.final.pt[!cox.revenue.final.pt$SELLER_INDUSTRY == "Financial institutions (incl. foreign sovereigns)",]

# change industry names from bank industries to bloomberg industries 
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Automotive",]$SELLER_INDUSTRY <- "Automobiles & Components"
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Chemicals, pharma, healthcare",]$SELLER_INDUSTRY <- "Pharmaceuticals, Biotechnology & Life Sciences"
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Construction, building materials",]$SELLER_INDUSTRY <- "Capital Goods" 
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Consumer goods, textile industry",]$SELLER_INDUSTRY <- "Consumer Durables & Apparel"
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Electronics",]$SELLER_INDUSTRY <- "Technology Hardware & Equipment"
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Machinery",]$SELLER_INDUSTRY <- "Capital Goods"
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Media, paper",]$SELLER_INDUSTRY <- "Media & Entertainment" 
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Metals",]$SELLER_INDUSTRY <- "Materials" 
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Services",]$SELLER_INDUSTRY <- "Commercial & Professional Services"
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Telecommunication, IT",]$SELLER_INDUSTRY <- "Telecommunication Services"
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Transport, travel",]$SELLER_INDUSTRY <- "Transportation"
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Food, beverages, agriculture",]$SELLER_INDUSTRY <- "Food, Beverage & Tobacco"
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Food & Staples Retailing",]$SELLER_INDUSTRY <- "Food, Beverage & Tobacco"

### WORK on this industries 
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Software & Services",]$SELLER_INDUSTRY <- "Commercial & Professional Services" #stay
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Consumer Services",]$SELLER_INDUSTRY <- "Commercial & Professional Services" #stay
cox.revenue.final.pt[cox.revenue.final.pt$SELLER_INDUSTRY == "Private customers",]$SELLER_INDUSTRY <- "Commercial & Professional Services" #stay

cox.revenue.final.pt <- cox.revenue.final.pt[!cox.revenue.final.pt$SELLER_INDUSTRY == "Health Care Equipment & Services",]
cox.revenue.final.pt <- cox.revenue.final.pt[!cox.revenue.final.pt$SELLER_INDUSTRY == "Semiconductors & Semiconductor Equipment",]

# create PSM 
treatment <- subset(cox.revenue.final.pt[cox.revenue.final.pt$treatment == 1,])
control <- subset(cox.revenue.final.pt[cox.revenue.final.pt$treatment == 0,])

# test the distribution
t.test(treatment$scaled.adverse.impact, control$scaled.adverse.impact, paired = F)

plot(cox.revenue.final.pt$scaled.adverse.impact ~ cox.revenue.final.pt$treatment)

cox.revenue.final.pt$SELLER_INDUSTRY <- as.factor(cox.revenue.final.pt$SELLER_INDUSTRY)
cox.revenue.final.pt$SELLER_COUNTRY <- as.factor(cox.revenue.final.pt$SELLER_COUNTRY)

# matching
match.df <- matchit(treatment ~ SELLER_INDUSTRY, data=cox.revenue.final.pt, method = "optimal", distance = "logit", ratio = 1)

matched.data <- match.data(match.df)

save(matched.data, file="matched.data.RData")

res <- lm(scaled.adverse.impact ~ treatment + SELLER_INDUSTRY, data = matched.data, weights = weights)

coeftest(res, vcov. = vcovCL, cluster = ~subclass)

plot(matched.data$scaled.adverse.impact, matched.data$distance)

treatment <- subset(matched.data[matched.data$treatment == 1,])
control <- subset(matched.data[matched.data$treatment == 0,])

t.test(treatment$scaled.adverse.impact, control$scaled.adverse.impact)

plot(matched.data$scaled.adverse.impact ~ matched.data$treatment)

#### Cox Proportional Hazard Rate Model 
# Controls only 
summary(quarter.cox <- coxph(Surv(time.to.normal, status) ~ scaled.adverse.impact + SELLER_INDUSTRY + SELLER_COUNTRY, 
                     data = matched.data, robust = T))

# Treatment 
summary(quarter.cox <- coxph(Surv(time.to.normal, status) ~ scaled.adverse.impact + SELLER_INDUSTRY + treatment, 
                             data = matched.data, robust = T))

# Payment terms 
summary(quarter.cox <- coxph(Surv(time.to.normal, status) ~ scaled.adverse.impact + SELLER_INDUSTRY + log.pt, 
                             data = matched.data, robust = T))

# Main effect 
summary(quarter.cox <- coxph(Surv(time.to.normal, status) ~ scaled.adverse.impact + SELLER_INDUSTRY + treatment + log.pt, 
                             data = matched.data, robust = T))

# Treatment & Payment terms  
summary(quarter.cox <- coxph(Surv(time.to.normal, status) ~ scaled.adverse.impact + SELLER_INDUSTRY + treatment + log.pt
                             + treatment*log.pt, 
                             data = matched.data, robust = T))

# Treatment & Adverse impact 
summary(quarter.cox <- coxph(Surv(time.to.normal, status) ~ scaled.adverse.impact + SELLER_INDUSTRY + treatment + log.pt
                             + treatment*scaled.adverse.impact, 
                             data = matched.data, robust = T))

# Full model 
summary(quarter.cox <- coxph(Surv(time.to.normal, status) ~ scaled.adverse.impact + SELLER_INDUSTRY + treatment + log.pt
                             + treatment*scaled.adverse.impact + treatment*log.pt, 
                             data = matched.data, robust = T))
