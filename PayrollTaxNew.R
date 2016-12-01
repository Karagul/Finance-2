
##################################
### Payroll Tax15 from KVS #######
##################################
# Load Payroll Tax15 File from KVS
kvs15 <- read.csv(file='KVS15.csv', header=FALSE, skip=65, nrows = 102573,  na.strings =c("", NA), stringsAsFactors = FALSE)

# Have to fill the NA at the bottom of the dataframe for na.locf to work.  Can't start with an NA
kvs15[c(102572, 102573), c(4:6)] <- "Anything here"

#Adding columns based on last ovservation carry forward, Remove "000" from column.
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

#Keep only rows with wage retrun record.  Other rows are junk
kvs15 <- kvs15[which(kvs15$V3=="A      WAGE RET."),]

#Remove commas from amount column and make it numeric class
kvs15$V4 <- as.numeric(gsub(",","", kvs15$V4))

#Keep and order only columns 2, 5, 4, and 8
kvs15 <- kvs15[c("V2", "V5", "V4", "V8")]
names(kvs15) <- c("Name", "Date", "Amount", "Account Number") #change column names
kvs15$Name <- trimws(kvs15$Name, "both") #trim whitespace from boths ends of column
kvs15 <-unique(kvs15)#remove duplicates if they exist
#write file for FY 2015
write.csv(kvs15, file="U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData/KVS15_Output.csv", row.names = FALSE)

##################################
### Payroll Tax16 from KVS #######
##################################
#Load Payroll tax16 File from KVS
kvs16 <- read.csv(file='KVS16.csv', header=FALSE, skip=65, nrows = 65697,  na.strings =c("", NA), stringsAsFactors = FALSE)

## Have to fill the NA at the bottom of the dataframe for na.locf to work.  Can't start with an NA
kvs16[c(65696, 65697), c(3:5)] <- "Anything here"

#Adding columns based on last ovservation carry forward, Remove "000" from column.
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

#Remove commas from amount column and make it numeric class
kvs16$V3 <- as.numeric(gsub(",","", kvs16$V3))
kvs16$Name <- str_sub(kvs16$V1, 9, 100) #extract name of business and assign to new column

#Keep and order columns Name, 4, 3, and 6
kvs16 <- kvs16[c("Name", "V4", "V3", "V6")]
names(kvs16) <- c("Name", "Date", "Amount", "Account Number") #rename columns
kvs16$Name <- trimws(kvs16$Name, "both") #trim whitespace from both ends
kvs16 <-unique(kvs16)#remove duplicates if they exist

### Created function to speed up fixing missing account numbers
fix_missing <- function(a, b, c = kvs16$`Account Number`, d = kvs16$Name){
  c[d == a] <- b 
  c
  }
kvs16$`Account Number` <- fix_missing("Name", "Account Number") ###removed all code here as it contains names and account numbers

write.csv(kvs16, file="U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData/KVS16_Output.csv", row.names = FALSE)

###################################
### Payroll Tax16 from Spingbrook #
###################################
ptax16  <-  read.csv(file="Springbrook16.csv", header=FALSE, skip = 1, na.strings = c("", NA, "Payment", NA), stringsAsFactors = FALSE)

#remove unneccessary rows with creditcash. 
ptax16$V1[ptax16$V1 =="CreditCash"] <- NA

#Adding columns based on last ovservation carry forward, Remove "000" from column.
ptax16 <- within(ptax16, {
  V1 <- na.locf(V1)
  V19 <- as.numeric(gsub(",","",V19))
  Name <- str_sub(V1, 24, 100)
  Name <- trimws(Name, "both")#trim white space at both ends.
  `Account Number` <- str_sub(V1, 12, 20)
  })

#Remove unneccessary colums and rows
ptax16 <- ptax16[c(-2:-4, -6:-12, -14:-18, -20:-25)]
ptax16 <- na.omit(ptax16)

#Keep only needed columns and change names as needed
ptax16 <- ptax16[c("Name", "V13", "V19", "Account Number")]
names(ptax16) <- c("Name", "Date", "Amount", "Account Number")
ptax16 <- unique(ptax16) #remove duplicates if they exists
ptax16 <- ptax16[!(ptax16$Name == ""),] #keep rows where name field is not blank

write.csv(kvs16, file="U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData/SpringBrook16_Output.csv", row.names = FALSE)

############################
###  Bind Files Together ###
############################
payroll_tax <- do.call("rbind", list(kvs15, kvs16, ptax16))
payroll_tax <- unique(payroll_tax) #check again for any duplication of rows

write.csv(payroll_tax, file="U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData/Combined15-16.csv", row.names = FALSE)

#Load Business Infomration and join with payroll tax dataframe
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

#Remove unnecessary information form address fields
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

#Remove punctuation from city column 
payroll_tax_merge$City <- gsub("[[:punct:]]", "", payroll_tax_merge$City)

#Combine city and state for goecoding
payroll_tax_merge$City_State <- with(payroll_tax_merge, paste(City, State, sep = " "))

#create new dataframe that mirrors combined file for geocoding
payroll_count <- payroll_tax_merge
payroll_count$Count <- 1 #assign a count of 1 for each row to wrap up rows by city/state combinations
payroll_count1 <- aggregate(Count ~ City_State + City, payroll_count, sum) #perform the aggregation

#Geocode city_state column using data science toolkit api and bind results back with aggregated file
payroll_city_coordinatesDSK <- geocode(payroll_count1$City_State, source = "dsk")
payroll_count1 <- cbind(payroll_count1, payroll_city_coordinatesDSK)

#Join geocoded dataframe with original dataframe
payroll_tax_merge <- merge(payroll_tax_merge, payroll_count1, by.x="City_State", by.y="City_State", all=TRUE)

#Write file to CSV
write.csv(payroll_tax_merge, file="U:/OpenGov/Unique Reports/Finance/PayrollTax/NewData/BusinessTaxes.csv", row.names = FALSE)

#SQLite storage #
payroll <- dbDriver("SQLite")
cons.payroll <- dbConnect(payroll, dbname="O:/AllUsers/CovStat/Data Portal/repository/Data/Database Files/PayrollTax.db")
dbWriteTable(cons.payroll, "PayrollTax", payroll_tax_merge, overwrite = TRUE)
dbDisconnect(cons.payroll)

### Subsetting by Date ###
#test <- port.ins[port.ins$NewLeaseDate > as.Date("1899-12-31"),]

#as.Date(x, origin = "1970-01-01")


