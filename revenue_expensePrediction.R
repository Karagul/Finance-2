
#Download General Fund data for fiscal years from OpenGov platform and reshape it 
prediction14  <- read.csv("PredictionFY2014.csv", stringsAsFactors = FALSE, na.strings = c("", NA), header=TRUE)
prediction14 <- prediction14%>%
    melt(id=c("Category", "Object.Type"))%>%
    na.omit(prediction14)
prediction15  <- read.csv("PredictionFY2015.csv", stringsAsFactors = FALSE, na.strings = c("", NA), header=TRUE)
prediction15 <- prediction15 %>%
    melt(id=c("Category", "Object.Type"))%>%
    na.omit(prediction15)
prediction16  <- read.csv("PredictionFY2016.csv", stringsAsFactors = FALSE, na.strings = c("", NA), header=TRUE)
prediction16 <- prediction16 %>%
    melt(id=c("Category", "Object.Type"))%>%
    na.omit(prediction16)

predictions <- do.call("rbind", list(prediction14, prediction15, prediction16))
predictions <- rename(predictions, c("variable"= "Date", "value"="Amount"))


predictions <- within(predictions, {
  FiscalMonth <- NA
  FiscalMonth [Date == "July.FY.2014.Actual"] <- "7/31/13"
  FiscalMonth [Date == "August.FY.2014.Actual"] <- "8/31/13"
  FiscalMonth [Date == "September.FY.2014.Actual"] <- "9/30/13"
  FiscalMonth [Date == "October.FY.2014.Actual"] <- "10/31/13"
  FiscalMonth [Date == "November.FY.2014.Actual"] <- "11/30/13"
  FiscalMonth [Date == "December.FY.2014.Actual"] <- "12/31/13"
  FiscalMonth [Date == "January.FY.2014.Actual"] <- "1/31/14"
  FiscalMonth [Date == "February.FY.2014.Actual"] <- "2/28/14"
  FiscalMonth [Date == "March.FY.2014.Actual"] <- "3/31/14"
  FiscalMonth [Date == "April.FY.2014.Actual"] <- "4/30/14"
  FiscalMonth [Date == "May.FY.2014.Actual"] <- "5/31/14"
  FiscalMonth [Date == "June.FY.2014.Actual"] <- "6/30/14"})
  
predictions <- within(predictions, {
  FiscalMonth [Date == "July.FY.2015.Actual"] <- "7/31/14"
  FiscalMonth [Date == "August.FY.2015.Actual"] <- "8/31/14"
  FiscalMonth [Date == "September.FY.2015.Actual"] <- "9/30/14"
  FiscalMonth [Date == "October.FY.2015.Actual"] <- "10/31/14"
  FiscalMonth [Date == "November.FY.2015.Actual"] <- "11/30/14"
  FiscalMonth [Date == "December.FY.2015.Actual"] <- "12/31/14"
  FiscalMonth [Date == "January.FY.2015.Actual"] <- "1/31/15"
  FiscalMonth [Date == "February.FY.2015.Actual"] <- "2/28/15"
  FiscalMonth [Date == "March.FY.2015.Actual"] <- "3/31/15"
  FiscalMonth [Date == "April.FY.2015.Actual"] <- "4/30/15"
  FiscalMonth [Date == "May.FY.2015.Actual"] <- "5/31/15"
  FiscalMonth [Date == "June.FY.2015.Actual"] <- "6/30/15"})

predictions <- within(predictions, {
  FiscalMonth [Date == "July.FY.2016.Actual"] <- "7/31/15"
  FiscalMonth [Date == "August.FY.2016.Actual"] <- "8/31/15"
  FiscalMonth [Date == "September.FY.2016.Actual"] <- "9/30/15"
  FiscalMonth [Date == "October.FY.2016.Actual"] <- "10/31/15"
  FiscalMonth [Date == "November.FY.2016.Actual"] <- "11/30/15"
  FiscalMonth [Date == "December.FY.2016.Actual"] <- "12/31/15"
  FiscalMonth [Date == "January.FY.2016.Actual"] <- "1/31/16"
  FiscalMonth [Date == "February.FY.2016.Actual"] <- "2/29/16"
  FiscalMonth [Date == "March.FY.2016.Actual"] <- "3/31/16"
  FiscalMonth [Date == "April.FY.2016.Actual"] <- "4/30/16"
  FiscalMonth [Date == "May.FY.2016.Actual"] <- "5/31/16"
  FiscalMonth [Date == "June.FY.2016.Actual"] <- "6/30/16"})

predictions$Date <- NULL

write.csv(predictions, "predictions.csv", row.names = FALSE)



