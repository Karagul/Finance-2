
setwd("U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData")

library("xlsx")
library("plyr")
library("dplyr")
library("tidyr")
library("reshape")
library("reshape2")
library("stringr")
library("zoo")
library("lubridate")
library("RSQLite")
library("ggmap")

###Payroll Tax15######################################################
setwd("U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData")

#### Payroll Tax15 ######
kvs15 <- read.csv(file='KVS15.csv', header=FALSE, skip=65, nrows = 102573,  na.strings =c("", NA), stringsAsFactors = FALSE)

## Have to fill the NA at the bottom of the dataframe for na.locf to work.  Can't start with an NA
kvs15[c(102572, 102573), c(4:6)] <- "Anything here"

### Adding columns based on last ovservation carry forward, Remove "000" from column.
kvs15 <- within(kvs15,{
  V5 <- na.locf(V5, fromLast = TRUE)
  V6 <- na.locf(V6, fromLast = TRUE)
  V8 <- ifelse(grepl("000", V2), as.character(V2), NA)
  })

#Run na.locf on account number and remove "-"
kvs15 <- within (kvs15, {
  V8 <- na.locf(V8, fromLast = TRUE)
  V8 <- gsub("[[:punct:]]", "", V8)
  })

#kvs$V2 <- trimws(kvs$V2, "both")
kvs15 <- kvs15[which(kvs15$V3=="A      WAGE RET."),]

kvs15$V4 <- as.numeric(gsub(",","", kvs15$V4))
#kvs15$Name <- str_sub(kvs$V1, 9, 100)

kvs15 <- kvs15[c("V2", "V5", "V4", "V8")]
names(kvs15) <- c("Name", "Date", "Amount", "Account Number")
kvs15$Name <- trimws(kvs15$Name, "both")

kvs15 <-unique(kvs15)
write.csv(kvs15, file="U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData/KVS15_Output.csv", row.names = FALSE)

#### Payroll Tax16 ######
kvs16 <- read.csv(file='KVS16.csv', header=FALSE, skip=65, nrows = 65697,  na.strings =c("", NA), stringsAsFactors = FALSE)

## Have to fill the NA at the bottom of the dataframe for na.locf to work.  Can't start with an NA
kvs16[c(65696, 65697), c(3:5)] <- "Anything here"

kvs16 <- within(kvs16, {
  V4 <- na.locf(V4, fromLast = TRUE)
  V5 <- na.locf(V5, fromLast = TRUE)
  V6 <- ifelse(grepl("-000", V1), as.character(V1), NA)
  })

#Run na.locf on account number and remove "-"
kvs16 <- within (kvs16, {
  V6 <- na.locf(V6, fromLast = TRUE)
  V6 <- gsub("[[:punct:]]", "", V6)
  V6 <- str_sub(V6, 9, 17)
  })

### Keep only row with wage ret.
kvs16 <- kvs16[which(kvs16$V2=="A      WAGE RET."),]

kvs16$V3 <- as.numeric(gsub(",","", kvs16$V3))
kvs16$Name <- str_sub(kvs16$V1, 9, 100)

kvs16 <- kvs16[c("Name", "V4", "V3", "V6")]
names(kvs16) <- c("Name", "Date", "Amount", "Account Number")
kvs16$Name <- trimws(kvs16$Name, "both")

kvs16 <-unique(kvs16)

### Created function to speed up fixing missing account numbers
fix_missing <- function(a, b, c = kvs16$`Account Number`, d = kvs16$Name){
  c[d == a] <- b 
  c
  }

kvs16$`Account Number` <- fix_missing("AMERICAN POSTAL", "303225000") 
kvs16$`Account Number` <- fix_missing("A R ALLEN COMPANY INC", "454823000") 
kvs16$`Account Number` <- fix_missing("ARAGON EMPLOYEE LEASING", "455945000")
kvs16$`Account Number` <- fix_missing("BLUEGRASS FIRE PROTECTION", "457889000")
kvs16$`Account Number` <- fix_missing("CITIZENS FEDERAL S L", "322800000")
kvs16$`Account Number` <- fix_missing("FIRST BAPTIST CHURCH", "342450000")
kvs16$`Account Number` <- fix_missing("FIRST CHRISTIAN CHURCH", "342500000")
kvs16$`Account Number` <- fix_missing("FIRST CHURCH OF GOD", "342510000")
kvs16$`Account Number` <- fix_missing("KENTON COUNTY PUBLIC LIBRARY", "367800000")
kvs16$`Account Number` <- fix_missing("NO KY WATER SERVICE DIST", "367855000")
kvs16$`Account Number` <- fix_missing("LATONIA BAPTIST CHURCH", "373950000")
kvs16$`Account Number` <- fix_missing("MASONIC TEMPLE", "380600000")
kvs16$`Account Number` <- fix_missing("MONUMENTAL LIFE INSURANCE CO", "386785000")
kvs16$`Account Number` <- fix_missing("N KY CONVENTION CENTER CORP", "391448000")
kvs16$`Account Number` <- fix_missing("NO KY CONVENTION CENTER CORP", "442703000")
kvs16$`Account Number` <- fix_missing("OMNI FIREPROOFING COMPANY INC", "392900000")
kvs16$`Account Number` <- fix_missing("PNC BANK NA", "394202000")
kvs16$`Account Number` <- fix_missing("PAPAS CHRIS & SON", "394550000")
kvs16$`Account Number` <- fix_missing("QUEEN CITY AWNING", "399312000")
kvs16$`Account Number` <- fix_missing("ROSEDALE MANOR INC", "406500000")
kvs16$`Account Number` <- fix_missing("RUNYAN MEMORIAL", "407306000")
kvs16$`Account Number` <- fix_missing("SMOKEY INC", "417610000")
kvs16$`Account Number` <- fix_missing("SOUTHSIDE BAPT CH", "418100000")
kvs16$`Account Number` <- fix_missing("THELEN ASSOCIATES INC", "426300000")
kvs16$`Account Number` <- fix_missing("T A N K", "407306000")
kvs16$`Account Number` <- fix_missing("TRINITY EPISCOPAL CHURCH", "426875000")
kvs16$`Account Number` <- fix_missing("WAYNE ELECTRICAL SERVICE LLC", "443091000")
kvs16$`Account Number` <- fix_missing("WHPC INC", "430905000")
kvs16$`Account Number` <- fix_missing("LUCKHARDT LAWN SERVICE INC", "447461000")
kvs16$`Account Number` <- fix_missing("COMPLETE PEST CONTROL INC", "449007000")
kvs16$`Account Number` <- fix_missing("PRICKEL ELECTRIC LLC", "449791000")
kvs16$`Account Number` <- fix_missing("FOCUS INVESTIGATIONS LLC", "452043000")
kvs16$`Account Number` <- fix_missing("ROBINSON & BRANDT", "452102000")
kvs16$`Account Number` <- fix_missing("KENTUCKIANS FOR COMMONWEALTH", "452629000")
kvs16$`Account Number` <- fix_missing("DECK ROBERT", "453100000")
kvs16$`Account Number` <- fix_missing("Z W TELECOM INC", "455403000")
kvs16$`Account Number` <- fix_missing("STAR DRYWALL OF LOUISVILLE INC", "455673000")
kvs16$`Account Number` <- fix_missing("EME FENCE CO INC", "456208000")
kvs16$`Account Number` <- fix_missing("STRANGE DONALD JR", "456402000")
kvs16$`Account Number` <- fix_missing("PHIPPS REPROGRAPHICS INC", "456425000")
kvs16$`Account Number` <- fix_missing("TASICO CINCINNATI LP", "456716000")
kvs16$`Account Number` <- fix_missing("QUINTILES COMMERCIAL US INC", "457735000")
kvs16$`Account Number` <- fix_missing("FIRST GREEN BUILDING SERVICES", "457743000")
kvs16$`Account Number` <- fix_missing("NOAH'S ART INC", "446889000")

write.csv(kvs16, file="U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData/KVS16_Output.csv", row.names = FALSE)


#### Payroll Tax16 ######
ptax16  <-  read.csv(file="Springbrook16.csv", header=FALSE, skip = 1, na.strings = c("", NA, "Payment", NA), stringsAsFactors = FALSE)

ptax16$V1[ptax16$V1 =="CreditCash"] <- NA

####Fill down row from most recent row.
ptax16 <- within(ptax16, {
  V1 <- na.locf(V1)
  V19 <- as.numeric(gsub(",","",V19))
  Name <- str_sub(V1, 24, 100)
  Name <- trimws(Name, "both")   ##trim white space at both ends.
  `Account Number` <- str_sub(V1, 12, 20)
  })

ptax16 <- ptax16[c(-2:-4, -6:-12, -14:-18, -20:-25)]
ptax16 <- na.omit(ptax16)

ptax16 <- ptax16[c("Name", "V13", "V19", "Account Number")]
names(ptax16) <- c("Name", "Date", "Amount", "Account Number")

ptax16 <- unique(ptax16)
ptax16 <- ptax16[!(ptax16$Name == ""),]

write.csv(kvs16, file="U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData/SpringBrook16_Output.csv", row.names = FALSE)

###  Bind Files Together ###
payroll_tax <- do.call("rbind", list(kvs15, kvs16, ptax16))
payroll_tax <- unique(payroll_tax)

write.csv(payroll_tax, file="U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData/Combined15-16.csv", row.names = FALSE)

###Load Business Infomration and join with payroll tax dataframe
businessData <- read.xlsx2("BusinessInformation.xlsx", as.data.frame = TRUE, sheetIndex = 1)

payroll_tax_merge <- merge(payroll_tax, businessData, by.x="Account Number", by.y="bt_master_business_no", all.x=TRUE, all.y=FALSE)

payroll_tax_merge$customer_city[payroll_tax_merge$customer_address == "2203 FOWLER ST"] <- "CINCINNATI OH"
payroll_tax_merge$customer_city[payroll_tax_merge$Name == "UNIVERSITY OF KENTUCKY"] <- "LEXINGTON KY"

## Create new columns in merged file
payroll_tax_merge <- within(payroll_tax_merge, {
  City <- str_sub(customer_city, -50, -4) 
  State <- str_sub(customer_city, -2, -1)
  FullAddress <- paste(customer_address, City, State, customer_zip, sep = " ")
})



payroll_tax_merge$FullAddress <- gsub("ATTN PAYROLL", " ", payroll_tax_merge$FullAddress)
payroll_tax_merge$FullAddress <- gsub("ATTN: JAMIE BAUMAN", " ", payroll_tax_merge$FullAddress)

### Create function to fix city names ####
fix_city <- function(a, b, c = payroll_tax_merge$City){
  c <- gsub(a, b, c)
  c
}

payroll_tax_merge$City <- fix_city("ALEXADNRIA", "ALEXANDRIA")
payroll_tax_merge$City <- fix_city("ALEXANDIA", "ALEXANDRIA")
payroll_tax_merge$City <- fix_city("\\CINCINNAT\\>", "CINCINNATI")
payroll_tax_merge$City <- fix_city("\\CINCINNTI\\>", "CINCINNATI")
payroll_tax_merge$City <- fix_city("\\CINTI\\>", "CINCINNATI")
payroll_tax_merge$City <- fix_city("SPGS", "SPRINGS")
payroll_tax_merge$City <- fix_city("\\<CRES\\>", "CRESCENT")
payroll_tax_merge$City <- fix_city("\\<CRESCNT\\>", "CRESCENT")
payroll_tax_merge$City <- fix_city("\\<HIGHLND HGTS\\>", "HEIGHTS")
payroll_tax_merge$City <- fix_city("LATONIA", "COVINGTON")
payroll_tax_merge$City <- fix_city("CINCINNAT", "CINCINNATI")
payroll_tax_merge$City <- fix_city("\\<MAYFILED HTS\\>", "MAYFIELD HEIGHTS")
payroll_tax_merge$City <- fix_city("\\<PITSSBURGH\\>", "PITTSBURGH")
payroll_tax_merge$City <- fix_city("\\<MORNINGVIEW\\>", "MORNING VIEW")
payroll_tax_merge$City <- fix_city("\\<HEIGHTS\\>", "HIGHLAND HEIGHTS")
payroll_tax_merge$City <- fix_city("\\<HIGHLAND HIGHLAND HEIGHTS\\>", "HIGHLAND HEIGHTS")##Previous gsub created this need
payroll_tax_merge$City <- fix_city("\\<RYLAND HIGHLAND HEIGHTS\\>", "RYLAND HEIGHTS")##Previous gsub created this need
payroll_tax_merge$City <- fix_city("\\<MAYFIELD HIGHLAND HEIGHTS\\>", "MAYFIELD HEIGHTS")##Previous gsub created this need
payroll_tax_merge$City <- fix_city("\\<STERLING HIGHLAND HEIGHTS\\>", "STERLING HEIGHTS")##Previous gsub created this need
payroll_tax_merge$City <- fix_city("\\COLD SPRING\\>", "COLD SPRINGS")
payroll_tax_merge$City <- fix_city("\\COVNIGTON\\>", "COVINGTON")
payroll_tax_merge$City <- fix_city("\\DEVOSSVILLE\\>", "DEMOSSVILLE")
payroll_tax_merge$City <- fix_city("\\E PARKER CITY\\>", "PARKER CITY")
payroll_tax_merge$City <- fix_city("\\HAMITLON\\>", "HAMILTON")
payroll_tax_merge$City <- fix_city("\\<WILLIAMSVLLE\\>", "WILLIAMSVILLE")


## Remove punctuation from city column 
payroll_tax_merge$City <- gsub("[[:punct:]]", "", payroll_tax_merge$City)

payroll_tax_merge$City_State <- with(payroll_tax_merge, paste(City, State, sep = " "))


payroll_count <- payroll_tax_merge
payroll_count$Count <- 1
payroll_count1 <- aggregate(Count ~ City_State + City, payroll_count, sum)
#payroll_count1 <- payroll_count1[!(payroll_count1$City_State == " "),]

payroll_city_coordinatesDSK <- geocode(payroll_count1$City_State, source = "dsk")
payroll_count1 <- cbind(payroll_count1, payroll_city_coordinatesDSK)

payroll_tax_merge <- merge(payroll_tax_merge, payroll_count1, by.x="City_State", by.y="City_State", all=TRUE)

## Write file to CSV
write.csv(payroll_tax_merge, file="U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData/BusinessTaxes.csv", row.names = FALSE)


###   SQLite storage ####
payroll <- dbDriver("SQLite")
cons.payroll <- dbConnect(payroll, dbname="O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/PayrollTax.db")
dbWriteTable(cons.payroll, "PayrollTax", payroll_tax_merge, overwrite = TRUE)
dbDisconnect(cons.payroll)

### Subsetting by Date ###
#test <- port.ins[port.ins$NewLeaseDate > as.Date("1899-12-31"),]

#as.Date(x, origin = "1970-01-01")


location <- geocode("Symmes TWP OH", source = "dsk")

