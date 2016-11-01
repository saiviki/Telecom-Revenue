# Data is loaded into the environment.
original = read.csv("/home/saiviki/R/Assignment/Telecom Data1.csv")

# A look at the data which is imported.
summary(orignal)
str(original)

names(telecom)
# "region"   "tenure"   "age"      "marital"  "address"  "income"   "ed" "employ"   "retire"   "gender"  
# "reside"   "tollfree" "equip"    "callcard" "wireless" "longmon"  "tollmon"  "equipmon" "cardmon"  "wiremon" 
# "longten"  "tollten"  "equipten" "cardten"  "wireten"  "multline" "voice"    "pager"    "internet" "callid"  
# "callwait" "forward"  "confer"   "ebill"    "custcat"  "churn"   

# longmon+tollmon+equipmon+cardmon+wiremon+longten+tollten+equipten+cardten+wireten

# Variables that need to be converted from int to factor:
# region, marital, ed, retire, gender, reside, tollfree, equip, callcard, wireless, multiline, voice,
# pager, internet, callid, callwait, forward, confer, ebill, custcat, churn

# Variables that need to be converted from factor to numeric:
# income, longten, tollten, equipten, cardten, wireten

# Make a copy of the dataset
telecom = original


# Integer to Factor conversions.
a <- match(c("region", "marital", "ed", "retire", "gender", "reside", "tollfree", "equip", 
             "callcard", "wireless", "multline", "voice", "pager", "internet", "callid",
             "callwait", "forward", "confer", "ebill", "custcat", "churn"), names(telecom))
for(i in a)
  telecom[,i] = as.factor(telecom[,i])


# Factor to integer conversions
a <- match(c("income", "longten", "tollten", "equipten", "cardten", "wireten"), names(telecom))

# The data is introducing many NA's. The introduction of NAs are because of the values having 
# commas. Need to change the format in the excel so that no NAs are introduced by coercion.
for(i in a)
  telecom[,i] = as.numeric(as.character(telecom[,i]))

# Lets do some Clustering
library(cluster)
# Take only the relevant data for clustering.
num_data = telecom[,c(6,16:25)]
# Lets check whether there is any natural clustering. 4 is my guess.
consumer_cluster = kmeans(scale(num_data),4,50,100)
plot(num_data, col = consumer_cluster$cluster) #Can never make sense from this plot
clusplot(x= num_data, clus = consumer_cluster$cluster, color = T)
# In hindsight, 4 seems to be a bad guess.
# To confirm, we'll check the silhouette value for the different clusters.
# Silhouette function:
find_silhouette=function(data,cluster){
  require(cluster)
  fit=kmeans(scale(data),centers = cluster,nstart=100)
  s=silhouette(x = fit$cluster,dist(scale(data)))
  mean(s[,3])
}
# Silhoette values for different number of clusters.
sapply(2:10,find_silhouette,data=num_data)
# 0.4334209 0.3867378 0.2974899 0.3003457 0.2441933 0.2376881 0.2719538 0.2422153 0.2493057
# The Silhouette values are too low. Hence we will go ahead with some other approach.

tenure_clus = telecom[,c(21:25)]
sapply(2:10,find_silhouette,data=tenure_clus)
# 0.5608019 0.5241484 0.4841121 0.4984781 0.4298134 0.4339773 0.4414433 0.4317643 0.4335335
consumer_cluster = kmeans(scale(tenure_clus),3,50,100)
clusplot(x= tenure_clus, clus = consumer_cluster$cluster, color = T)

# K-means clustering with 3 clusters of sizes 740, 144, 116

normal1 = telecom[consumer_cluster$cluster == 1,]
normal2 = telecom[consumer_cluster$cluster == 2,]
normal3 = telecom[consumer_cluster$cluster == 3,]

# Aggregrate according to the different customer clusters
# Mean of the revenue for different customer clusters
avr_cluster = aggregate(cbind(longmon, tollmon, equipmon, cardmon, wiremon,
                              longten, tollten, equipten, cardten, wireten) ~ consumer_cluster$cluster,
                        data = telecom, mean)

# Median of the revenue for different customer clusters
med_cluster = aggregate(cbind(longmon, tollmon, equipmon, cardmon, wiremon,
                              longten, tollten, equipten, cardten, wireten) ~ consumer_cluster$cluster,
                        data = telecom, median)

# Total of revenue for different customer clusters
tot_cluster = aggregate(cbind(longmon, tollmon, equipmon, cardmon, wiremon,
                              longten, tollten, equipten, cardten, wireten) ~ consumer_cluster$cluster,
                        data = telecom, sum)

table(consumer_cluster$cluster, telecom$churn)

#     0   1
# 1 488 242
# 2  66  23
# 3  90   6
# 4  73   3
# 5   9   0

# We can do two things from here: 
# 1. Heirarchical Clustering
# 2. Creating subsets of customers based on categories and checking for kmeans clusters or heirarchical
# clusters after that.
# Lets check for Heirarchical clustering directly.



# Subsets of each category of customers
category1 = subset(original, subset = custcat == 1)
category2 = subset(original, subset = custcat == 2)
category3 = subset(original, subset = custcat == 3)
category4 = subset(original, subset = custcat == 4)

# Aggregrate according to the customer categories
# Mean revenue for different customer categories
avr_custcat = aggregate(cbind(longten, tollten, equipten, cardten, wireten) ~ custcat,
                data = telecom, mean)

# Median revenue for different customer categories
med_custcat = aggregate(cbind(longten, tollten, equipten, cardten, wireten) ~ custcat,
                        data = telecom, median)

# Total revenue for different customer categories
tot_custcat = aggregate(cbind(longten, tollten, equipten, cardten, wireten) ~ custcat,
                        data = telecom, sum)

# Number of Customers in each segment who avail those services and who does not avail those services
no_custcat = aggregate(cbind(tollfree, equip, callcard, wireless, multline, voice, pager, 
                             internet, callid, callwait, forward, confer) ~ custcat,
                       data = telecom, table)

# Multiline is not properly created using the above method, hence created seperately 
table(telecom$multline, telecom$custcat)

table(telecom$churn, telecom$custcat)

#     1   2   3   4
# 0 183 158 237 148
# 1  83  59  44  88

# Looking at different Customer Categories, the customer category 4 is generating the most revenue in each
# service, lets look at a plot of the different things.

tot_revenue = tot_custcat
for(i in 1:11) j[i] = sum(tot_custcat[,i])

tot_revenue = rbind(tot_custcat, j)

percen_revenue = tot_revenue
for(i in 1:11)
  for(j in 1:4)
    percen_revenue[j,i] = tot_revenue[j,i]*100/tot_revenue[5,i]

percen_revenue[,1] = NULL
percen_revenue = percen_revenue[1:4,]
rownames(percen_revenue) = percen_revenue[,1]

# Average monthly revenue from every customer.
telecom$av_monthly = (telecom$longten + telecom$tollten + telecom$equipten + telecom$cardten +
                      telecom$wireten) / telecom$tenure


# Barplot of every category of tenure revenue
barplot(height = c(percen_revenue$longten, percen_revenue$tollten, percen_revenue$equipten,
                   percen_revenue$cardten, percen_revenue$wireten))

# Customers leaving based on churn
plot(telecom$av_monthly, telecom$tenure, col = telecom$churn, pch = 8)

# Revenue from Customers who will churn vs Customers who will not

rev_churn = sum(churn_customers[,37])
rev_stay = sum(staying_customers[,37])
mosaicplot(x = c(rev_churn, rev_stay))
rev_churn_p = rev_churn/(rev_stay+rev_churn)
rev_stay_p= rev_stay/(rev_stay+rev_churn)


library(arules)
# Apriori Algorithm for Churning customers according to usage of secondary services
service_matrix = as(object = as.matrix(original[,c(26:33, 36)]), Class = "itemMatrix")
sec_rules = apriori(service_matrix, parameter = list(support = 0.1, confidence = 0.4), 
                appearance = list(rhs = "churn", default = "lhs"))
sec_rules = sort(sec_rules, by = "lift")
interestMeasure(x = sec_rules, transactions = service_matrix)
inspect(sec_rules)

# Apriori Algorithm for Churning customers according to usage of main services
rev_matrix = as(object = as.matrix(original[,c(12:15, 36)]), Class = "itemMatrix")
pri_rules = apriori(rev_matrix, parameter = list(support = 0.1, confidence = 0.4), 
                appearance = list(rhs = "churn", default = "lhs"))
pri_rules = sort(pri_rules, by = "lift")
interestMeasure(x = pri_rules, transactions = rev_matrix)
inspect(pri_rules)


# Apriori Algorithm for providing new services to the customers in Category 2 and 3
service = as(object = as.matrix(category4[,c(12:15,26:33)]), Class = "itemMatrix")
rules = apriori(service, parameter = list(support = 0.8, confidence = 0.9))
rules = sort(rules, by = "lift")
interestMeasure(x = rules, transactions = service)
inspect(rules)


cor(original[,c(12:15,26:33,36)])
corrplot::corrplot(cor(original[,c(12:15,26:33,36)]))


churn_customers = subset(telecom, subset = churn == 1)
staying_customers = subset(telecom, subset = churn == 0)
# 726 and 274
set.seed(16027)
clearsample = staying_customers[502:726,]
trainsample = sample(1:726, 501, replace = FALSE)
total = rbind(staying_customers[1:501,],churn_customers)
trainset = total[trainsample,]
testset = total[-trainsample,]

library(rpart)
library(caret)
library(rattle)
cnt = rpart.control(xval = 10, cp = 0.0125, surrogatestyle = 1, usesurrogate = 2, maxsurrogate = 10)
model1=rpart(formula = churn~.,data = trainset, control = cnt)
pred_model1=predict(model1,newdata = testset,type="class")
rattle::fancyRpartPlot(model1)
confusionMatrix(pred_model1,testset$churn)

longten + equipten + cardten + tollten + address + age + income + av_monthly
cntrl=trainControl(method = "cv",number=10)
model2=train(churn~longten + equipten + cardten + tollten + address + age + income + av_monthly
             ,data=trainset,method="C5.0",trControl=cntrl,metric="Kappa")
pred_model2=predict(model2,newdata=testset)
confusionMatrix(pred_model2,testset$churn)
plot(model2)
