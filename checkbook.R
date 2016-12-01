

setwd("U:/OpenGov/Unique Reports/Checkbook")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("reshape")
library("reshape2")
library("stringr")
library("zoo")
library("lubridate")

#read in checkbook data 
checkbook  <-  read.csv(file="Pulled_12.01.2016.csv", header=FALSE,  stringsAsFactors = FALSE, quote = "") ## had to disable quoting
checkbook$V10 <- NULL
checkbook <- rename(checkbook, c("V1"="Account String", "V2"="Amount", "V3"="Check Date", "V4"="Check No.", "V5"="Fiscal Period",
                                 "V6"="Fiscal Year", "V7"="Description", "V8"="Vendor No.", "V9"="Account Description"))

#Split account string into separate fields
checkbook$Fund <- str_sub(checkbook$`Account String`, 1, 4)
checkbook$Dept. <- str_sub(checkbook$`Account String`, 6, 9)
checkbook$Account <- str_sub(checkbook$`Account String`, 11, 19)

#Load Vendor File
vendor <- read.csv(file="Vendor12.01.16.csv", header=FALSE, stringsAsFactors = FALSE)
vendor <- rename(vendor, c("V1"="Vendor Address", "V2"="Vendor City", "V3"="Vendor Name", "V4"="Vendor State", "V5"="Vendor Number"))

#Merge all checkbook data with vendor directory file by vendor number
checkbook <- merge(checkbook, vendor, by.x="Vendor No.", by.y="Vendor Number", all.x=TRUE)
checkbook$`Vendor Address` <- NULL
checkbook <- checkbook[c("Account String", "Fund", "Dept.", "Account", "Amount", "Check Date", "Check No.", "Fiscal Period", "Fiscal Year",
                         "Description", "Vendor No.", "Account Description", "Vendor City", "Vendor Name", "Vendor State")]
checkbook$`Vendor City`[checkbook$`Vendor City` == ""] <- "None"
checkbook$`Vendor State`[checkbook$`Vendor State` == ""] <- "None"

write.csv(checkbook,"U:/OpenGov/Unique Reports/Checkbook/Pulled12.01.2016UpdatedVendor.csv", row.names=FALSE)
