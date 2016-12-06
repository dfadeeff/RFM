########################################################################################
#Now perform CRM analysis
########################################################################################
getwd()
setwd("/home/dima/Automation/Reports/CRM")

rm(list = ls(all=TRUE))

library(lubridate)
library(zoo)
library(dplyr)
library(mongolite)
library(jsonlite)
library(data.table)
library(reshape2)
library(tidyr)
library(stringr)

#Load data 
load("DataToCRM.dat")

collections <- c("intwash_notifications","ratings")
notifications_connection <- mongo(collection = collections[1], db = "uk_live", 
                url = "mongodb://172.31.51.215:27017",verbose = TRUE)
ratings_connection <- mongo(collection = collections[2], db = "uk_live",
                url = "mongodb://172.31.51.215:27017",verbose = TRUE)

notifications <- notifications_connection$find(fields = '{"_id":1,"email":1,"publicReference":1,"newsletterSubscribed":1,"reference":1}')
ratings <- ratings_connection$find(fields = '{"_id":1,"appVersion":1,"orderReference":1,
                                   "customerReference":1,"rating":1,"skipped":1,"topics":1}')

#####################################################
#Work with phones
#####################################################
#Check list inside the dataframe
#phones <- customers_table[,c(1,2)]
#colnames(phones) <- c("id","ph") 
#phones <- data.table(phones)
#Expands the data frame
#phones_temp1 <- as.data.table(phones)[,unlist(ph),by=id] #expands without types, aka names
#phones_temp <- phones[, .(phnames = names(unlist(ph)), phvalues = unlist(ph)), by = id] #expands with names
#Aggregate by values, transform from long to wide format
#phones_temp <- dcast(phones_temp,id~phnames, value.var = 'phvalues')
#Replace with 2 number if applicable
#phones_temp$number <- ifelse(is.na(phones_temp$number),phones_temp$number1,phones_temp$number)
#Check whether vector has NAs
#sum(is.na(phones_temp$number)) > 0
#Left join
#phones <- merge(x = phones[,1,with=F],y = phones_temp, by="id", all.x = T )
#Subset phones
#phones <- phones[,.(id,number,type)]


#####################################################
#Work with addresses
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
adr_temp <- addresses[, .(adrnames = names(unlist(adr)), adrvalues = unlist(adr)), by = id]


#Aggregate by values, transform from long to wide format
adr_temp <- dcast(adr_temp,id~adrnames, value.var = "adrvalues",
                  fun.aggregate = function(x) paste(x, collapse=""))

#Replace empty strings with data from another col
adr_temp$addressLine <- ifelse(str_length(adr_temp$addressLine)==0,adr_temp$addressLine1,adr_temp$addressLine) 
adr_temp$addressLine2 <- ifelse(str_length(adr_temp$addressLine2)==0,adr_temp$addressLine21,adr_temp$addressLine2) 
adr_temp$zip <- ifelse(str_length(adr_temp$zip)==0,adr_temp$zip1,adr_temp$zip)
adr_temp <- data.table(adr_temp)

#Subset the address lines and zip 
adr_temp <- adr_temp[,.(id,addressLine,addressLine2,zip)]

#Merge
addresses <- merge(x = addresses[,1,with=F],y = adr_temp, by="id", all.x = T )


#####################################################
#Work with tags
#####################################################
tags <- customers_table[,c(1,10)]
class(tags)
colnames(tags) <- c("id","tags")
#tags <- data.table(tags)
#tags <- tags[, .(tagnames = names(unlist(meta.tags)), tagvalues = unlist(meta.tags)), by = id]
tags <- as.data.table(tags)[,unlist(tags),by=id]


#####################################################
#Move to main body
#####################################################
crm <- customers_table[,c(1,3,5:9)]
crm <- flatten(crm)
str(crm)
colnames(crm)[1] <- c("id")

crm <- merge(x = crm, y = tags, by = "id", all.x = T)
colnames(crm)[8] <- c("tags")

#####################################################
#Now work with orders
#####################################################
orders_table <- flatten(orders_table)
str(orders_table)

#####################################################
#Try to split the user Agent
#####################################################
#Try to split the userByAgent
#str_split_fixed(orders_table$createdBy.userAgent,"()",4)
#out <- strsplit(as.character(orders_table$createdBy.userAgent),";")
#data.frame(orders_table$new,do.call(rbind,out))
#list <- strsplit(orders_table$createdBy.userAgent,";")

#orders_table$device_temp <- NULL
#orders_table$device_temp <- gsub("[\\(\\)]","", orders_table$createdBy.userAgent) #remove everything before semicolon
#orders_table$device_temp <- gsub(";.*$","", orders_table$device_temp) #remove parenthesis
#orders_table$device_temp <- sub(".+? ", "", orders_table$device_temp) #remove string before first whitespace

orders_table$device <- NULL
orders_table$device[which(grepl("iPhone",orders_table$createdBy.userAgent)==T)] = "iOS app"
orders_table$device[which(grepl("Android",orders_table$createdBy.userAgent)==T)] = "android app"
orders_table$device[which(!is.na(orders_table$createdBy.userAgent) 
                          & !grepl("^(iOS)",orders_table$device) 
                          & !grepl("^(android)",orders_table$device))] = "web app"

#Check tha table
table(orders_table$device)

#create device by customer
orders_table <- data.table(orders_table)

#Add isvalid column (first, initialize default value, then, adjust by particular state)
orders_table$isvalid = 1 #initialize isvalid
orders_table$isvalid[which(orders_table$state %in% c("new","payment_authorisation_error",
                                                   "canceled","reserved"))] = 0 

#Extract Service Class from the string
orders_table$serviceClass <- sapply(as.character(orders_table$chosenServiceClass.reference),
                                     function(x) {
                                       unlist(strsplit(x,'-',fixed = T))[2]})


#Create a dataframe with total orders
totalOrders <- orders_table[,.(sum(length(unique(reference)))),by=.(customer)]
colnames(totalOrders) <- c("customer","totalOrders")
#Check total orders
sum(totalOrders$totalOrders) 
length(unique(orders_table$reference))

#Make order summary, providing firstOrderDate, lastOrderDate,sumOfValidOrders,#of VouchersUsed
orders_summary <- orders_table[isvalid==1,.(min(createdAt),max(createdAt),sum(isvalid),sum(!is.na(voucherCode))),by=.(customer)] 
colnames(orders_summary) <- c("customer","firstOrder","lastOrder","validOrders","VouchersUsed")


#Create a dataframe for ServiceClasses
long <- orders_table[,.(reference,serviceClass)]
wide <- dcast(long,reference~serviceClass,value.var = "serviceClass") #order(left side) ~ to serviceClass(right side), by serviceClass
names(wide)

#Reformat into numbers
wide$ClassicN <- ifelse(is.na(wide$CLASSIC),0,1)
wide$ExpressN <- ifelse(is.na(wide$EXPRESS),0,1)
wide$LiteN <- ifelse(is.na(wide$LITE),0,1)
wide$PersilN <- ifelse(is.na(wide$PERSIL),0,1)
wide$PlusN <- ifelse(is.na(wide$PLUS),0,1)

#Check the number of reformatted classes
sum(wide$ClassicN)+sum(wide$ExpressN)+sum(wide$LiteN)+sum(wide$PersilN)+sum(wide$PlusN)
sum(!is.na(orders_table$serviceClass))


#Now merge the wide stuff back into orders subset
orders_table <- merge(x = orders_table, y = wide[,c("reference","ClassicN","ExpressN","LiteN","PersilN","PlusN")], by='reference', all.x = T)

#Calculate orders by class
classes <- orders_table[isvalid==1,.(sum(ClassicN),sum(ExpressN),sum(LiteN),sum(PersilN),sum(PlusN)),by=.(customer)]
colnames(classes) <- c("customer","Classic","Express","Lite","Persil","Plus")

#Merge back to orders summary
orders_summary <- merge(x = orders_summary, y = classes, by = 'customer', all.x = T)
orders_summary$NoClass <- orders_summary$validOrders-orders_summary$Classic-orders_summary$Express-orders_summary$Lite-orders_summary$Persil-orders_summary$Plus

#Merge to crm
colnames(crm)[1] <- c("customer") #remember that customer from crm is id in the final table

#Count Open order
orders_table$openOrder = 1 #initialize isvalid
orders_table$openOrder[which(orders_table$state %in% c("canceled","completed"))] = 0 

orders_table$canceledOrder = 0 #initialize isvalid
orders_table$canceledOrder[which(orders_table$state %in% c("canceled"))] = 1

customer_stat <- orders_table[,.(min(locationIdentifier),min(languageCode),min(device),sum(openOrder),sum(canceledOrder)),by=.(customer)]
colnames(customer_stat) <- c("customer","City","LanguageCode","Device","OpenOrders","CanceledOrders")
colnames(addresses)[1] <- c("customer")

#####################################################
#MERGE STEP BY STEP TO CRM
#####################################################
#Merge customer_stat with crm, left join
crm <- merge(x = crm, y = customer_stat, by = "customer",all.x = T)
#Left join orders_summary
crm <- merge(x = crm, y = orders_summary, by = "customer", all.x = T )
#Left join total orders
crm <- merge(x = crm, y = totalOrders, by = "customer", all.x = T)
#Left join addresses
crm <- merge(x = crm, y = addresses, by = "customer", all.x = T)

crm$CustomerStatus = "NA"
crm$CustomerStatus[which(crm$validOrders==1)] <- "New"
crm$CustomerStatus[which(crm$validOrders>1)] <- "Returning"


#####################################################
#Deal with ratings
#####################################################
#Unlist the column
colnames(ratings)[3] <- c("reference")

#Remove duplicates
ratings <- ratings[!duplicated(ratings$reference),]
#Make a subset
ratings_subset <- ratings[,c("reference","topics")]
ratings_subset <- data.table(ratings_subset)


#Unlist
rating_temp <- ratings_subset[, .(ratnames = names(unlist(topics)), ratvalues = unlist(topics)), by = reference]

rating_temp <- dcast(rating_temp,reference ~ ratnames, value.var = "ratvalues",
                  fun.aggregate = function(x) paste(x, collapse=""))
crm$CustomerStatus[which(crm$validOrders==1)] <- "New"
crm$CustomerStatus[which(crm$validOrders>1)] <- "Returning"


#####################################################
#Deal with ratings
#####################################################
#Unlist the column
colnames(ratings)[3] <- c("reference")

#Remove duplicates
ratings <- ratings[!duplicated(ratings$reference),]
#Make a subset
ratings_subset <- ratings[,c("reference","topics")]
ratings_subset <- data.table(ratings_subset)


#Unlist
rating_temp <- ratings_subset[, .(ratnames = names(unlist(topics)), ratvalues = unlist(topics)), by = reference]

rating_temp <- dcast(rating_temp,reference ~ ratnames, value.var = "ratvalues",
                  fun.aggregate = function(x) paste(x, collapse=""))
crm$CustomerStatus[which(crm$validOrders==1)] <- "New"
crm$CustomerStatus[which(crm$validOrders>1)] <- "Returning"


#####################################################
#Deal with ratings
#####################################################
#Unlist the column
colnames(ratings)[3] <- c("reference")

#Remove duplicates
ratings <- ratings[!duplicated(ratings$reference),]
#Make a subset
ratings_subset <- ratings[,c("reference","topics")]
ratings_subset <- data.table(ratings_subset)


#Unlist
rating_temp <- ratings_subset[, .(ratnames = names(unlist(topics)), ratvalues = unlist(topics)), by = reference]

rating_temp <- dcast(rating_temp,reference ~ ratnames, value.var = "ratvalues",
                  fun.aggregate = function(x) paste(x, collapse=""))

#Right join rating to rating_temp

rating_temp <- merge(x = rating_temp, y = ratings[,c("reference","rating")], by = "reference", all.y = T)
orders_table <- merge(x = orders_table, y = rating_temp, by = "reference", all.x = T)

#Create last rating for every customer
names(orders_table)
colnames(orders_table)[22:27] <- c("Punctuality","CleaningQ","DamagedI","DriverB","IroningQ","MissingI")
last_rating <- orders_table[,.(max(rating),max(Punctuality),max(CleaningQ),max(IroningQ)), by = .(customer)]
colnames(last_rating) <- c("customer","rating","Punctuality","CleaningQ","IroningQ")

#Merge ratings to crm
crm <- merge(x = crm ,  y = last_rating, by = "customer", all.x =  T)

#####################################################
#Deal with notifications
#####################################################
notifications <- data.table(notifications)
names(notifications)
#notifications_lastValue <- notifications[,list(c(lastpublicRef = publicReference[.N],lastSubscribed=newsletterSubscribed)),email]
notifications_lastpublicRef <- notifications[,list(lastpublicRef = publicReference[.N]),email] 
notifications_lastnewsSubs <- notifications[,list(lastnewsSubs = newsletterSubscribed[.N]),email]
notifications_last <- cbind(x = notifications_lastpublicRef, y = notifications_lastnewsSubs)

notifications_last <- notifications_last[,.(x.email,x.lastpublicRef,y.lastnewsSubs)]
colnames(notifications_last) <- c("email","publicReference","newsletterSubscribed")
colnames(crm)[3] <- c("email")

crm <- merge(x = crm, y = notifications_last, by = "email", all.x = T)

















