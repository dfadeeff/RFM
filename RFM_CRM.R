getwd()
setwd("/home/dima/Automation/Reports/CRM")

rm(list = ls(all=TRUE))

####################################################################################
#Learn how to copy files from one to another folder
####################################################################################

#Set the folders
from_folder <- "/home/dima/sisense_share/Exchange_rate"
to_folder <- "/home/dima/Automation/Reports/CRM/"

#Identify files
list_files <- list.files(from_folder,"exchange_rates.csv",full.names = T)

#Copy the files
file.copy(list_files,to=to_folder,overwrite = T)

exchange_rate <- read.csv("exchange_rates.csv",header = T,sep = ";")
exchange_rate <- subset(exchange_rate, exchange_rate$currency_code=="GBP")
exchange_rate <- exchange_rate[,c(1:3)]
exchange_rate$exchange_rate_date <- as.Date(exchange_rate$exchange_rate_date, "%Y-%m-%d")


####################################################################################
#Load the collections
####################################################################################

#Load library
library(lubridate)
library(zoo)
library(dplyr)
library(mongolite)
library(jsonlite)
library(ggplot2)
library(data.table)
library(stringr)


#Open connection
collection = c("intwash_orders", "intwash_customers","intwash_invoices","intwash_voucher_redemptions")

orders <- mongo(collection = collection[1], db = "uk_live", 
                url = "mongodb://172.31.51.215:27017",verbose = TRUE)
customers <- mongo(collection = collection[2], db="uk_live",
                   url = "mongodb://172.31.51.215:27017",verbose = TRUE )
invoices <- mongo(collection = collection[3], db="uk_live",
                  url = "mongodb://172.31.51.215:27017",verbose = TRUE )
#vouchers <- mongo(collection = collection[4], db="uk_live",
#                  url = "mongodb://172.31.51.215:27017",verbose = TRUE )

#Parse thru collection and find the keys 
orders_table <- orders$find(fields = '{"_id":1,"invoice":1,"reference":1,"createdAt":1,
                           "customer":1,"voucherCode":1,"state":1,"locationIdentifier":1,"chosenServiceClass.reference":1}')

customers_table <- customers$find(fields = '{"reference":1,"_id":1,
                        "internalData.segmentation":1,"person.name":1,"userAccount.email":1,
                        "internalData.gender":1,"internalData.basketGender":1,
                        "addresses.addressLine":1,"addresses.addressLine2":1,"phones.number":1,"phones.type":1}')

invoices <- invoices$find(fields = '{"_id":1,"grossTotal":1,"grossTotalWithoutDiscounts":1,"netTotal":1,"netTotalWithoutDiscounts":1}')

#vouchers <- vouchers$find(fields = '{"_id":1, "campaignId":1, "blockId":1,
#                          "modelId":1, "code":1, "customerReference":1, "reference":1}')



#Flatten customers table
customers_table <- flatten(customers_table)

#Subset and rename customers
customers <- customers_table[,c(1,8,3)]
colnames(customers) <- c("customer","segmentation","reference")
#Subset orders
orders_data <- orders_table[,c(1:6,8)]
orders_data <- merge(x=orders_data,y=customers,by="customer",all.x = TRUE)

names(orders_data)
orders_data <- orders_data[,c(9,2:8)]
colnames(orders_data) <- c("customer","id","state","order","createdAt","location","invoice","segmentation")

#Rename the field to merge
colnames(invoices) <- c("invoice","NetTotalAfterDiscount","NetTotalBeforeDiscount",
                        "GrossTotalAfterDiscount","GrossTotalBeforeDiscount")

orders_data <- merge(x=orders_data,y=invoices,by="invoice",all.x = T)

####################################################################################
#Merging with the X rates
####################################################################################
orders_data$exchange_rate_date <- as.Date(orders_data$createdAt,"%Y-%m-%d") 
orders_data <- merge(x=orders_data,y=exchange_rate,by="exchange_rate_date",all.x = T)

####################################################################################
#Optimize the data frame
####################################################################################
unique(orders_data$location)
orders_data$NetTotalAfterDiscount <- ifelse(orders_data$location=="gb_london",
                                            orders_data$NetTotalAfterDiscount/orders_data$exchange_rate_value,
                                            orders_data$NetTotalAfterDiscount)
orders_data$NetTotalBeforeDiscount <- ifelse(orders_data$location=="gb_london",
                                            orders_data$NetTotalBeforeDiscount/orders_data$exchange_rate_value,
                                            orders_data$NetTotalBeforeDiscount)
orders_data$GrossTotalAfterDiscount <- ifelse(orders_data$location=="gb_london",
                                            orders_data$GrossTotalAfterDiscount/orders_data$exchange_rate_value,
                                            orders_data$GrossTotalAfterDiscount)
orders_data$GrossTotalBeforeDiscount <- ifelse(orders_data$location=="gb_london",
                                            orders_data$GrossTotalBeforeDiscount/orders_data$exchange_rate_value,
                                            orders_data$GrossTotalBeforeDiscount)

#Add isvalid column (first, initialize default value, then, adjust by particular state)
orders_data$isvalid = 1 #initialize isvalid
orders_data$isvalid[which(orders_data$state %in% c("new","payment_authorisation_error",
                                 "canceled","reserved"))] = 0 


#Now get the subset
df <- orders_data[,c(3,5:8,13,16)]

df <- data.table(df)
df <- df[isvalid==1 & !is.na(GrossTotalBeforeDiscount)]


#Subset data.table
mydata <- df[,.(df$customer,df$GrossTotalBeforeDiscount, df$createdAt,df$isvalid)]

colnames(mydata) <- c('customer_id','purchase_amount','date_of_purchase','isvalid')
mydata$date_of_purchase = as.Date(mydata$date_of_purchase,format='%Y-%m-%d')


#Here the dataset grouped by customer is created
MinMax <- mydata[,.(min(date_of_purchase),max(date_of_purchase),sum(isvalid),mean(purchase_amount)),by=.(customer_id)] 

colnames(MinMax) <- c("customer_id","firstOrder","lastOrder","frequency","amount")


MinMax$recency = as.numeric(difftime(time1 = Sys.Date(),
                                        time2 = MinMax$lastOrder,
                                        units = 'days'))

MinMax <- MinMax[,.(customer_id,recency,frequency,amount)]

#Check the overall numbers
sum(MinMax$frequency)
length(unique(MinMax$customer_id))


####################################################################################
#Assign the ratings to the data table
####################################################################################

hist(MinMax$recency)
hist(MinMax$frequency, xlim = c(0,30),breaks = 90)
hist(MinMax$amount, xlim = c(0,100), breaks = 200)


#Create new data frame, copying customers
new_data <- MinMax

#Now assign points
RecencyPoints <- seq(100,20,-20)
RecencyWeight <- 0.4

FrequencyPoints <- seq(20,100,20)
FrequencyWeight <- 0.3

MonetaryPoints <- seq(20,100,20)
MonetaryWeight <- 0.3





#Method to cut the new_date table

#rankR 1 is very recent while rankR 5 is least recent
breaksRec = c(quantile(new_data$recency, probs = c(0,0.1,0.2,0.3,0.5,1)))
labelsRecencyOneFive <- factor(5:1)
new_data$rankR <- labelsRecencyOneFive[findInterval(new_data$recency,breaksRec,rightmost.closed = T)]

labelsRecencyPoints <- factor(RecencyPoints*RecencyWeight)
new_data$rankRecencyPoints = as.numeric(paste(labelsRecencyPoints[findInterval(new_data$recency, breaksRec,rightmost.closed = T)]))
#Check thresholds
c(quantile(new_data$recency, probs = c(0,0.1,0.2,0.3,0.5,1)))

#rankF 1 is least frequent while rankF 5 is most frequent
breaksFreq = c(quantile(new_data$frequency, probs = c(0,0.55,0.7,0.85,0.95,1)))
labelsFrequencyOneFive <- factor(1:5)
new_data$rankF <- labelsFrequencyOneFive[findInterval(new_data$frequency,breaksFreq,rightmost.closed = T)]

labelsFrequencyPoints <- factor(FrequencyPoints*FrequencyWeight)
new_data$rankFrequencyPoints = as.numeric(paste(labelsFrequencyPoints[findInterval(new_data$frequency,breaksFreq,rightmost.closed = T)]))
#Check thresholds
c(quantile(new_data$frequency, probs = c(0,0.55,0.7,0.85,0.95,1)))

#rankM 1 is lowest sales while rankM 5 is highest sales
#new_data$rankM = cut(new_data$amount, 5, labels = F)
breaksMon = c(quantile(new_data$amount, probs = c(0,0.3,0.5,0.7,0.9,1)))
labelsMonetaryOneFice <- factor(1:5)
new_data$rankM = labelsMonetaryOneFice[findInterval(new_data$amount,breaksMon,rightmost.closed = T)]

labelsMonetaryPoints <- factor(MonetaryPoints*MonetaryWeight)
new_data$rankMonetaryPoints = as.numeric(paste(labelsMonetaryPoints[findInterval(new_data$amount,breaksMon,rightmost.closed = T)]))
#Check thresholds
c(quantile(new_data$amount, probs = c(0,0.3,0.5,0.7,0.9,1)))


attach(new_data)
new_data$sum <- rankMonetaryPoints + rankRecencyPoints + rankFrequencyPoints
new_data <- new_data[order(new_data$sum,decreasing=TRUE),]

#Now codify the customers
finalbreakpoints <- seq(min(new_data$sum),max(new_data$sum),length.out = 6)
finalbreakpoints
new_data$class = findInterval(new_data$sum,finalbreakpoints,rightmost.closed = T)


#Assign names
library(plyr)
new_data$classfactor <- as.factor(new_data$class)
new_data$names <- revalue(new_data$classfactor, c("5"="Loyal","4"="Active","3"="Prospective","2"="Dormant","1"="Churned"))
table(new_data$names)



#Transform the data
#new_data = transform(new_data,score=interaction(new_data$rankR,new_data$rankF,new_data$rankM, sep = ''))


#Write to a specific folder that is shared with another machine/server
write.csv(new_data, file="/home/dima/sisense_share/Cohorts/RFM.csv",row.names = FALSE)




########################################################################################
#Now perform CRM analysis
########################################################################################

str(customers_table)

#####################################################
#Work with phones
#####################################################
#Check list inside the dataframe
phones <- customers_table[,c(1,2)]
colnames(phones) <- c("id","ph") 
phones <- data.table(phones)

#Expands the data frame
#phones_temp1 <- as.data.table(phones)[,unlist(ph),by=id] #expands without types, aka names
phones_temp <- phones[, .(phnames = names(unlist(ph)), phvalues = unlist(ph)), by = id] #expands with names
#Aggregate by values, transform from long to wide format
phones_temp <- dcast(phones_temp,id~phnames, value.var = 'phvalues')

#Replace with 2 number if applicable
phones_temp$number <- ifelse(is.na(phones_temp$number),phones_temp$number1,phones_temp$number)

#Check whether vector has NAs
sum(is.na(phones_temp$number)) > 0

#Left join
phones <- merge(x = phones[,1,with=F],y = phones_temp, by="id", all.x = T )

#Subset phones
phones <- phones[,.(id,number,type)]


#####################################################
#Work with phones
#####################################################
#Check another list inside the dataframe 
addresses <- customers_table[,c(1,4)]
colnames(addresses) <- c("id","adr")
addresses <- data.table(addresses)

#Here I removed all empty rows
#addresses <- addresses[first_non_null_index,] 

first_non_null_index <- which(!unlist(lapply(addresses$adr,is.null)))[1]
first_null_index <- which(unlist(lapply(addresses$adr,is.null)))[1]

intermediate <- addresses[first_null_index,] 
addresses[first_null_index,] <- addresses[first_non_null_index,]
addresses[first_non_null_index,] <- intermediate

#Expands the data frame
adr_temp <- addresses[, .(adrnames = names(unlist(adr)), adrvalues = as.character(unlist(adr))), by = id]

#Aggregate by values, transform from long to wide format
adr_temp <- dcast(adr_temp,id~adrnames, value.var = 'adrvalues')
#Replace with 2 number if applicable
adr_temp$number <- ifelse(is.na(adr_temp$addressLine),adr_temp$addressLine1,adr_temp$addressLine)
#Check whether vector has NAs
sum(is.na(adr_temp$addressLine)) > 0

#Merge
addresses <- merge(x = addresses[,1,with=F],y = adr_temp, by="id", all.x = T )


#Subset phones
addresses <- addresses[,.(id,addressLine,addressLine2)]


#Here I was trying to find first non null element in a row 
#lapply(adr_temp[6,1+which(!is.na(adr_temp[6,2:ncol(adr_temp)]))[1]])


crm <- customers_table[,c(1,3,5:9)]
colnames(crm) <- c('id','reference','user_email','person_name','gender','segmentation','basket_gender')

#Merge with phones and addresses
crm <- merge(x = crm, y = phones, by="id", all.x = T)
crm <- merge(x = crm, y = addresses, by="id", all.x = T)

crm <- crm[,c(2:9)]
colnames(crm)[1] <- c("customer_id")

crm <- merge(x = crm, y=MinMax, by="customer_id", all.x = T)
colnames(orders_table)[4] <- c("order")

#Merge with specific columns from initial orders table
orders_subset <- orders_data[,c("customer","order","createdAt","location","GrossTotalBeforeDiscount","isvalid")]
sum(orders_subset$isvalid)
length(unique(orders_subset$order))

orders_subset <- merge(x = orders_subset, y = orders_table[,c("order","voucherCode","chosenServiceClass")], by='order', all.x = T)
names(orders_subset)
str(orders_subset)
orders_subset <- flatten(orders_subset)
unique(orders_subset$chosenServiceClass.reference)


#Extract Service Class from the string
orders_subset$serviceClass <- sapply(as.character(orders_subset$chosenServiceClass.reference),
                                     function(x) {
                                       unlist(strsplit(x,'-',fixed = T))[2]})

#Check again the total valid and total attempted orders
sum(orders_subset$isvalid)
length(unique(orders_subset$order))


#Make orders summary
orders_subset <- data.table(orders_subset)
names(orders_subset)
orders_summary <- orders_subset[isvalid==1,.(min(createdAt),max(createdAt),sum(isvalid),sum(GrossTotalBeforeDiscount),sum(!is.na(voucherCode))),by=.(customer)] 
totalOrders <- orders_subset[,.(sum(length(unique(order)))),by=.(customer)]
colnames(totalOrders)[1] <- c("customer_id")

#Check the total # of orders
sum(orders_summary$V3)
sum(orders_data$isvalid)
colnames(orders_summary) <- c("customer_id","firstOrder","lastOrder","validOrders","sumGrossBeforeDiscount","vouchersUsed")

#Left join totalOrders to crm
crm <- merge(x = crm, y = totalOrders, by = "customer_id",all.x = T)
colnames(crm)[12] <- c("totalOrders")

#Check total orders
sum(totalOrders$V1)
sum(na.omit(crm$totalOrders))

#Convert from long to wide format the object orders_subset
str(orders_subset)
unique(orders_subset$serviceClass)

long <- orders_subset[,.(order,serviceClass)]
wide <- dcast(long,order~serviceClass,value.var = "serviceClass") #order(left side) ~ to serviceClass(right side), by serviceClass





