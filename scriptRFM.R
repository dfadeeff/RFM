getwd()
setwd('C:/Users/Administrator/Desktop/R/CLTV')

library(RODBC)

myconn <- odbcConnect("RCube", uid="", pwd="")
myconn
sqlTables(myconn)

mydata <- sqlQuery(myconn,"select a.time, a.reference, a.customer, b.netTotal_eur
                   from [Order Split] a
                   left join [Invoices_standard] b
                   on b._id = a.invoice
                   where a.isvalid=1 and b.netTotal_eur IS NOT NULL and b.netTotal_eur > 0 ")


mydata <- mydata[,c(3,4,1)]
colnames(mydata) <- c('customer_id','purchase_amount','date_of_purchase')
mydata$date_of_purchase = as.Date(mydata$date_of_purchase,format='%Y-%m-%d')

mydata$days_since = as.numeric(difftime(time1 = Sys.Date(),
                                      time2 = mydata$date_of_purchase,
                                      units = 'days'))

head(mydata)
summary(mydata)

#install.packages("sqldf")
library(sqldf)

customers = sqldf("Select customer_id,
                  min(days_since) as 'recency',
                  count(*) as 'frequency',
                  avg(purchase_amount) as 'amount'
                  from mydata GROUP by 1")

head(customers)
summary(customers)

hist(customers$recency)
hist(customers$frequency, xlim = c(0,30),breaks = 90)
hist(customers$amount, xlim = c(0,100), breaks = 200)


#Create new data frame, copying customers
new_data <- customers

#Remove customer_id as a variable
head(new_data)
#row.names(new_data)=new_data$customer_id
#new_data$customer_id = NULL


#Method to cut the new_date table

#rankR 1 is very recent while rankR 5 is least recent
#new_data$rankR = cut(new_data$recency, 5, labels = F)
new_data$rankR = cut(new_data$recency, breaks = c(quantile(new_data$recency, probs = seq(0,1,0.2))),labels = c("5","4","3","2","1"))

#rankF 1 is least frequent while rankF 5 is most frequent
new_data$rankF = cut(new_data$frequency, 5, labels = F)

#rankM 1 is lowest sales while rankM 5 is highest sales
#new_data$rankM = cut(new_data$amount, 5, labels = F)
new_data$rankM = cut(new_data$amount, breaks = c(quantile(new_data$amount, probs = seq(0,1,0.2))),labels = c("1","2","3","4","5"))

quantile(new_data$amount, probs = seq(0,1,0.2))
quantile(new_data$recency, probs = seq(0,1,0.2))
quantile(new_data$frequency, probs = seq(0,1,0.2))

#Transform the data
new_data = transform(new_data,score=interaction(new_data$rankR,new_data$rankF,new_data$rankM, sep = ''))



#Sort descending 
RFM <- new_data[order(new_data$score),]

write.csv(RFM, file='myoutput.csv',row.names = FALSE)


