#Set wd
getwd()
setwd("/home/dima/Automation/Reports/Cohorts")

rm(list = ls(all=TRUE))

#Load libraries
library(zoo)
library(DT)
library(data.table)
library(dplyr)
library(DT)
library(plyr)
library(htmlwidgets)

#Load data 
load("DataToCRM.dat")

#Set the folders
from_folder <- "/home/dima/powerbi-share/Attribution"
to_folder <- "/home/dima/Automation/Reports/Cohorts/"

#Identify files
list_files <- list.files(from_folder,"output.csv",full.names = T)

#Copy the files
file.copy(list_files,to = to_folder,overwrite = T)

funnel <- read.csv("output.csv",header = T,sep = ";")
funnel <- funnel[,c("order_reference","first_channel","last_channel")]
colnames(funnel)[1] <- c("reference")

orders <- orders_table[,c("customer","voucherCode","reference","createdAt",
                          "state","locationIdentifier")]

#Load voucher channels
collections = c("intwash_voucher_blocks","intwash_voucher_campaigns",
                "intwash_voucher_items","intwash_voucher_models")

blocks <- mongo(collection = collections[1], db = "uk_live", 
                url = "mongodb://172.31.51.215:27017",verbose = TRUE)
campaigns <- mongo(collection = collections[2], db="uk_live",
                   url = "mongodb://172.31.51.215:27017",verbose = TRUE )
items <- mongo(collection = collections[3], db = "uk_live", 
                url = "mongodb://172.31.51.215:27017",verbose = TRUE)
models <- mongo(collection = collections[4], db="uk_live",
                   url = "mongodb://172.31.51.215:27017",verbose = TRUE )

df_blocks <- blocks$find(fields = '{"campaignId":1,"reference":1,"name":1}')
df_campaigns <- campaigns$find(fields = '{"name":1, "reference":1}')
df_items <- items$find(fields = '{"_id":1,"code":1,"modelId":1}')
df_models <- models$find(fields = '{"_id":1,"blockId":1,"template":1}')
 
#Get the channel name from blocks
df_blocks$channel <- sub('-.*',"",df_blocks$name)
#Rename to join
colnames(df_models)[names(df_models)=="blockId"] <- "reference"
df_models <- merge(x = df_models, y = df_blocks[,c("reference","channel")], by = "reference", all.x = T)
colnames(df_models)[names(df_models)=="_id"] <- "modelId"
df_items <- merge(x = df_items, y = df_models[,c("modelId","channel")], by = "modelId", all.x = T)

colnames(df_items)[names(df_items) == "code"] <- 'voucherCode'
unique(df_items$channel)

#Now I have to restructure, group and rename the channels 
df_items$final_channel <- "NA"

crm_campaign = "Campaign"
df_items$final_channel[which(sapply(crm_campaign, function(x) grepl(x,df_items$channel))==TRUE | df_items$channel == "CRM")] <- "CRM-Campaign"
crm_journey = "Journey"
df_items$final_channel[which(sapply(crm_journey, function(x) grepl(x,df_items$channel))==TRUE)] <- "CRM-Journey"
cooperations = c("PR","Cooperations","Airbnb","Enjoy")
df_items$final_channel[which(sapply(paste(cooperations,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Cooperations"
customer_care = c("Apology","CC","Thank you","Survey","Thank")
df_items$final_channel[which(sapply(paste(customer_care,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "CC"
flyering = c("Free","Helper","Flyering","Friends","Percentage","Santa","Friday","GrabOne","HubSpot","Lorenzo","Mailings","Rabbit","Reclean",
             "XMAS","Fiksu","FixedBlock","£","Google","Groupon","Scheme","Rabit","Welcome","External","Berenberg","Promo","Huei","SANTA",
             "EUR15","St Giles","Hassle","Big Boss","Expansion")
df_items$final_channel[which(sapply(paste(flyering,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Flyering"
display = c("Appstore","Display","Deal")
df_items$final_channel[which(sapply(paste(display,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Display"
fb = c("Facebook","Social Media Paid")
df_items$final_channel[which(sapply(paste(fb,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Facebook"
internal = c("Internal","Team")
df_items$final_channel[which(sapply(paste(internal,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Internal"
out_of_home = c("OOH","Out of Home")
df_items$final_channel[which(sapply(paste(out_of_home,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Out Of Home"
sea = c("SEA","SEM")
df_items$final_channel[which(sapply(paste(sea,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "SEM"
twitter = "Twitter"
df_items$final_channel[which(sapply(twitter, function(x) grepl(x,df_items$channel))==TRUE)] <- "Twitter"
flash = "Flash Sales"
df_items$final_channel[which(sapply(flash, function(x) grepl(x,df_items$channel))==TRUE)] <- "Flash Sales"
refer_friend = c("Refer a Friend","Refer a Friend ","Refer")
df_items$final_channel[which(sapply(paste(refer_friend,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Refer a Friend"
affiliate = "Affiliate"
df_items$final_channel[which(sapply(affiliate, function(x) grepl(x,df_items$channel))==TRUE)] <- "Affiliate"
appco = "Appco/Direct Sales"
df_items$final_channel[which(sapply(appco, function(x) grepl(x,df_items$channel))==TRUE)] <- "Appco/Direct Sales"
printads = c("Print","Print Ad","Print Ads")
df_items$final_channel[which(sapply(paste(printads,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Print Ads"
website = "Website"
df_items$final_channel[which(sapply(website, function(x) grepl(x,df_items$channel))==TRUE)] <- "Website"
seo = "SEO"
df_items$final_channel[which(sapply(seo, function(x) grepl(x,df_items$channel))==TRUE)] <- "SEO"
unpaid = c("Socia Media Unpaid", "Socia Media Unpaid ", "Unpaid")  
df_items$final_channel[which(sapply(paste(unpaid,collapse = "|"), function(x) grepl(x,df_items$channel))==TRUE)] <- "Social Media Unpaid"

df_items$final_channel[which(df_items$final_channel=="NA")] <- 'Other'


#Merge back to orders
orders <- merge(x = orders, y = df_items[,c("voucherCode","final_channel")], by = "voucherCode", all.x = T)

#Work with export from Google Analytics
funnel <- funnel[!duplicated(funnel),]
orders <- merge(x = orders, y = funnel, by = "reference", all.x = T)
colnames(orders)[names(orders)=="final_channel"] <- "voucher_channel"

#Rename, first set to the same class
orders$first_channel <- as.character(orders$first_channel)
orders$last_channel <- as.character(orders$last_channel)

orders[,c(7:9)][is.na(orders[,c(7:9)])] <- "not set"
orders[,c(8:9)][(orders[,c(8:9)])=="(unavailable)"] <- "not set"

orders$voucherScore <- 1
orders$voucherScore[which(orders$voucher_channel == "not set")] <- 0
orders$voucherScore[which(orders$first_channel == "not set" & orders$last_channel != "not set" & orders$voucher_channel != "not set")] <- 0.5
orders$voucherScore[which(orders$first_channel == "not set" & orders$last_channel == "not set" & orders$voucher_channel != "not set")] <- 1
orders$voucherScore[which(orders$first_channel != "not set" & orders$last_channel == "not set" & orders$voucher_channel != "not set")] <- 0.5
orders$voucherScore[which(orders$first_channel != "not set" & orders$last_channel != "not set" & orders$voucher_channel != "not set")] <- 0.33









