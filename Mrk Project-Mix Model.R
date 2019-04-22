# Data cleaning & Prepare for modeling-----------------------------------------------------------

library(data.table)
library(dplyr)
options("scipen"=100, "digits"=4)

#read data
holiday <- fread('holiday.csv')
product <- fread('product_table_supp.csv')
transaction <- fread('transaction_table_supp.csv')
seasonality <- fread('seasonality.csv')
promo_ad <- fread('promo_ad.csv')

#merge seasonality data
seasonality$tran_wk <- cut(as.Date(seasonality$tran_wk), "week",start.on.monday = FALSE)
seasonality$tran_wk <- strftime(seasonality$tran_wk, format = "%y-%V")
seasonality <- seasonality[order(tran_wk),]
seasonality$weeknum <- rownames(seasonality)


#merge holiday data
holiday$tran_wk<- strftime(holiday$tran_wk, format = "%y-%V")

seasonality_holi <- merge(seasonality,holiday,"tran_wk",all.x = T)

seasonality_holi$holi_index <- 0

for (i in 1:nrow(seasonality_holi)){
  if (is.na(seasonality_holi[i,4])==TRUE){
    seasonality_holi[i,5] <- 0
  }else{
    seasonality_holi[i,5] <- 1
  }
}

#add week to transaction data
transaction$tran_wk = cut(as.Date(transaction$tran_dt), "week",start.on.monday = FALSE)
transaction$tran_wk <- strftime(transaction$tran_wk, format = "%y-%V")

seasonality_holi <- as.data.table(seasonality_holi)
seasonality_holi <- seasonality_holi[!duplicated(seasonality_holi[['weeknum']]),] 
seasonality_holi <- seasonality_holi[-106,]

transaction_new <- merge(transaction,seasonality_holi,by="tran_wk",all.x = T)

#devide promotion activities
typeof(promo_ad$tran_wk)
promo_ad$tran_wk <- cut(as.Date(promo_ad$tran_wk), "week",start.on.monday = FALSE)
promo_ad$tran_wk <- strftime(promo_ad$tran_wk, format = "%y-%V")

week_list <- seasonality[,c('weeknum','tran_wk')]
promo_ad  <- merge(promo_ad,week_list,"tran_wk",all.x = T)

#TV and radio adstock calculation
TV <- promo_ad[vehicle=='TV',]
names <-colnames(TV)

for (i in 1:6){
  TV[i,]$amount=(1-0.5**(1/8))*TV[i,]$amount
  num = as.numeric(TV[i,]$weeknum)
  tran_week = TV[i,]$tran_wk
  vehicle = TV[i,]$vehicle
  amount = TV[i,]$amount
  unit = TV[i,]$unit
  prod_assoc = TV[i,]$prod_assoc
  
  for (b in (num+1):106){
    df<-data.frame(tran_week,vehicle,amount*(0.5**(1/8))**(b-num),unit,prod_assoc,b)
    colnames(df)<-names
    TV <- rbind(TV, df)
  }
}

TV_new <- TV %>% group_by(weeknum) %>% summarise(amount=sum(amount))
TV_new <- as.data.table(TV_new)
TV_new$vehicle<- 'TV'
TV_new <- merge(TV_new,week_list, by='weeknum',all.x = TRUE)

#same for radio
radio <- promo_ad[vehicle=='Radio',]
names <-colnames(radio)

for (i in 1:6){
  radio[i,]$amount=(1-0.5**(1/4))*radio[i,]$amount
  num = as.numeric(radio[i,]$weeknum)
  tran_week = radio[i,]$tran_wk
  vehicle = radio[i,]$vehicle
  amount = radio[i,]$amount
  unit = radio[i,]$unit
  prod_assoc = radio[i,]$prod_assoc
  
  for (b in (num+1):106){
    df<-data.frame(tran_week,vehicle,amount*(0.5**(1/4))**(b-num),unit,prod_assoc,b)
    colnames(df)<-names
    radio <- rbind(radio, df)
  }
}

radio_new <- radio %>% group_by(weeknum) %>% summarise(amount=sum(amount))
radio_new <- as.data.table(radio_new)
radio_new$vehicle<- 'radio'
radio_new <- merge(radio_new,week_list, by='weeknum',all.x = TRUE)


#promotion on all
promo_ad_all <- promo_ad[(promo_ad$prod_assoc == 'ALL')&(promo_ad$vehicle !='TV' & promo_ad$vehicle !='Radio'),]
promo_ad_all <- promo_ad_all[,c('weeknum','amount','vehicle','tran_wk')]
promo_ad_all <- rbind(promo_ad_all,TV_new)
promo_ad_all <- rbind(promo_ad_all,radio_new)


#group transaction data
transaction_set<-transaction_new[,c(1,6,8:15,17)]
transaction_set$weeknum <-as.numeric(transaction_set$weeknum)

transaction_group<- transaction_set %>% group_by(prod_id, tran_wk) %>% summarise(sale_amount = sum(tran_prod_sale_qty), discount = sum(tran_prod_discount_amt)/sum(tran_prod_sale_amt), self_price = mean(prod_unit_price),
                                                                                    seas_index = mean(seas_index), holi_index = mean(holi_index), weeknum = max(weeknum))


transaction_group<-as.data.table(transaction_group)

#Initialize table and put variables values into it
transaction_group$flyer <- 0
transaction_group$store_display <- 0
transaction_group$paid_search <- 0
transaction_group$radio <- 0
transaction_group$tv <- 0
transaction_group$web_display <- 0
transaction_group$email <- 0
levels(promo_ad_all$vehicle)

for (i in 1:nrow(promo_ad_all[vehicle=='TV',]) ){
  transaction_group[weeknum==promo_ad_all[vehicle=='TV',]$weeknum[i],]$tv <- promo_ad_all[vehicle=='TV',]$amount[i]
}

for (i in 1:nrow(promo_ad_all[vehicle=='radio',]) ){
  transaction_group[weeknum==promo_ad_all[vehicle=='radio',]$weeknum[i],]$radio <- promo_ad_all[vehicle=='radio',]$amount[i]
}

for (i in 1:nrow(promo_ad_all[vehicle=='Paid Search',]) ){
  transaction_group[weeknum==promo_ad_all[vehicle=='Paid Search',]$weeknum[i],]$paid_search <- promo_ad_all[vehicle=='Paid Search',]$amount[i]
}

for (i in 1:nrow(promo_ad_all[vehicle=='Web Display',]) ){
  transaction_group[weeknum==promo_ad_all[vehicle=='Web Display',]$weeknum[i],]$web_display <- promo_ad_all[vehicle=='Web Display',]$amount[i]
}

for (i in 1:nrow(promo_ad_all[vehicle=='Email',]) ){
  transaction_group[weeknum==promo_ad_all[vehicle=='Email',]$weeknum[i],]$email <- promo_ad_all[vehicle=='Email',]$amount[i]
}



promo_ad_138936951 <- promo_ad[promo_ad$prod_assoc == '138936951']

for (i in 1:nrow(promo_ad_138936951[vehicle=='Flyer',]) ){
  transaction_group[weeknum==promo_ad_138936951[vehicle=='Flyer',]$weeknum[i] & prod_id==138936951,]$flyer <- promo_ad_138936951[vehicle=='Flyer',]$amount[i]
}

promo_ad_138936952 <- promo_ad[promo_ad$prod_assoc == '138936952']

for (i in 1:nrow(promo_ad_138936952[vehicle=='Flyer',]) ){
  transaction_group[weeknum==promo_ad_138936952[vehicle=='Flyer',]$weeknum[i] & prod_id==138936952,]$flyer <- promo_ad_138936952[vehicle=='Flyer',]$amount[i]
}

for (i in 1:nrow(promo_ad_138936952[vehicle=='Store Display',]) ){
  transaction_group[weeknum==promo_ad_138936952[vehicle=='Store Display',]$weeknum[i] & prod_id==138936952,]$store_display <- promo_ad_138936952[vehicle=='Store Display',]$amount[i]
}


promo_ad_138936953 <- promo_ad[promo_ad$prod_assoc == '138936953']

for (i in 1:nrow(promo_ad_138936953[vehicle=='Flyer',]) ){
  transaction_group[weeknum==promo_ad_138936953[vehicle=='Flyer',]$weeknum[i] & prod_id==138936953,]$flyer <- promo_ad_138936953[vehicle=='Flyer',]$amount[i]
}

for (i in 1:nrow(promo_ad_138936953[vehicle=='Store Display',]) ){
  transaction_group[weeknum==promo_ad_138936953[vehicle=='Store Display',]$weeknum[i] & prod_id==138936953,]$store_display <- promo_ad_138936953[vehicle=='Store Display',]$amount[i]
}

# Modeling 1----------------------------------------------------------------
unique(transaction_group$prod_id)
transaction_product1<-transaction_group[prod_id==138936951,]

transaction_product1$tv<- 0.95*(1-exp(-0.02*transaction_product1$tv))
transaction_product1$radio<- 0.9*(1-exp(-0.025*transaction_product1$radio))
#transaction_product1$tv <- transaction_product1$tv-2
#transaction_product1$radio <- transaction_product1$radio-2

s_mean <- mean(transaction_product1$seas_index)  #8073.6
s_std <- sd(transaction_product1$seas_index)     #2134.4
transaction_product1$seas_index<- (transaction_product1$seas_index-s_mean)/s_std

p_mean <- mean(transaction_product1$paid_search)  #25383.5
p_std <- sd(transaction_product1$paid_search)     #14005.8
transaction_product1$paid_search<- (transaction_product1$paid_search-p_mean)/p_std

w_mean <- mean(transaction_product1$web_display)  #26666.7
w_std <- sd(transaction_product1$web_display)     #90156.6
transaction_product1$web_display<- (transaction_product1$web_display-w_mean)/w_std

e_mean <- mean(transaction_product1$email)  #9523.8
e_std <- sd(transaction_product1$email)     #42796.0
transaction_product1$email<- (transaction_product1$email-e_mean)/e_std

lm_prod1 <- lm(log(sale_amount) ~ discount+self_price+seas_index+as.factor(holi_index)+as.factor(flyer)+paid_search+radio+tv+web_display+email,data=transaction_product1)
summary(lm_prod1)

#logit function has good improvement
# transaction_product1$log_amount = (transaction_product1$sale_amount-min(transaction_product1$sale_amount))/(max(transaction_product1$sale_amount)-min(transaction_product1$sale_amount))
# lm2_prod1 <- lm(log(log_amount/(1-log_amount)) ~ discount+self_price+seas_index+as.factor(holi_index)+as.factor(flyer)+paid_search+radio+tv+web_display+email,data=transaction_product1[log_amount!=0&log_amount!=1,])
# summary(lm2_prod1)

#assume maximum volume to be 10% up the current max
lm2_prod1 <- lm(log(sale_amount/(330-sale_amount)) ~ discount+self_price+seas_index+as.factor(holi_index)+as.factor(flyer)+paid_search+radio+tv+web_display+email,data=transaction_product1[-1,])
summary(lm2_prod1)

# Modeling 2 --------------------------------------------------------------

transaction_product2<-transaction_group[prod_id==138936952,]

transaction_product2$tv<- 0.95*(1-exp(-0.02*transaction_product2$tv))
transaction_product2$radio<- 0.9*(1-exp(-0.025*transaction_product2$radio))
#transaction_product1$tv <- transaction_product1$tv-2
#transaction_product1$radio <- transaction_product1$radio-2

s_mean <- mean(transaction_product2$seas_index)  #8073.6
s_std <- sd(transaction_product2$seas_index)     #2134.4
transaction_product2$seas_index<- (transaction_product2$seas_index-s_mean)/s_std

p_mean <- mean(transaction_product2$paid_search)  #25383.5
p_std <- sd(transaction_product2$paid_search)     #14005.8
transaction_product2$paid_search<- (transaction_product2$paid_search-p_mean)/p_std

w_mean <- mean(transaction_product2$web_display)  #26666.7
w_std <- sd(transaction_product2$web_display)     #90156.6
transaction_product2$web_display<- (transaction_product2$web_display-w_mean)/w_std

e_mean <- mean(transaction_product2$email)  #9523.8
e_std <- sd(transaction_product2$email)     #42796.0
transaction_product2$email<- (transaction_product2$email-e_mean)/e_std

lm_prod2 <- lm(log(sale_amount) ~ discount+self_price+seas_index+as.factor(holi_index)+as.factor(store_display)+as.factor(flyer)+paid_search+radio+tv+web_display+email,data=transaction_product2)
summary(lm_prod2)

#logit function has good improvement
# transaction_product2$log_amount = (transaction_product2$sale_amount-min(transaction_product2$sale_amount))/(max(transaction_product2$sale_amount)-min(transaction_product2$sale_amount))
# lm2_prod2 <- lm(log(log_amount/(1-log_amount)) ~ discount+self_price+seas_index+as.factor(holi_index)+as.factor(store_display)+as.factor(flyer)+paid_search+radio+tv+web_display+email,data=transaction_product2[log_amount!=0&log_amount!=1,])
# summary(lm2_prod2)

#assume maximum volume to be 10% up the current max
lm2_prod2 <- lm(log(sale_amount/(227-sale_amount)) ~ discount+self_price+seas_index+as.factor(holi_index)+as.factor(store_display)+as.factor(flyer)+paid_search+radio+tv+web_display+email,data=transaction_product2[-1,])
summary(lm2_prod2)

# Modeling 3 --------------------------------------------------------------

transaction_product3<-transaction_group[prod_id==138936953,]

transaction_product3$tv<- 0.95*(1-exp(-0.02*transaction_product3$tv))
transaction_product3$radio<- 0.9*(1-exp(-0.025*transaction_product3$radio))
#transaction_product1$tv <- transaction_product1$tv-2
#transaction_product1$radio <- transaction_product1$radio-2

s_mean <- mean(transaction_product3$seas_index)  #8073.6
s_std <- sd(transaction_product3$seas_index)     #2134.4
transaction_product3$seas_index<- (transaction_product3$seas_index-s_mean)/s_std

p_mean <- mean(transaction_product3$paid_search)  #25383.5
p_std <- sd(transaction_product3$paid_search)     #14005.8
transaction_product3$paid_search<- (transaction_product3$paid_search-p_mean)/p_std

w_mean <- mean(transaction_product3$web_display)  #26666.7
w_std <- sd(transaction_product3$web_display)     #90156.6
transaction_product3$web_display<- (transaction_product3$web_display-w_mean)/w_std

e_mean <- mean(transaction_product3$email)  #9523.8
e_std <- sd(transaction_product3$email)     #42796.0
transaction_product3$email<- (transaction_product3$email-e_mean)/e_std

lm_prod3 <- lm(log(sale_amount) ~ discount+self_price+seas_index+as.factor(holi_index)+as.factor(store_display)+as.factor(flyer)+paid_search+radio+tv+web_display+email,data=transaction_product3)
summary(lm_prod3)

#logit function has good improvement
# transaction_product3$log_amount = (transaction_product3$sale_amount-min(transaction_product3$sale_amount))/(max(transaction_product3$sale_amount)-min(transaction_product3$sale_amount))
# lm2_prod3 <- lm(log(log_amount/(1-log_amount)) ~ discount+self_price+seas_index+as.factor(holi_index)+as.factor(store_display)+as.factor(flyer)+paid_search+radio+tv+web_display+email,data=transaction_product3[log_amount!=0&log_amount!=1,])
# summary(lm2_prod3)
max(transaction_product3$sale_amount)

#assume maximum volume to be 10% up the current max
lm2_prod3 <- lm(log(sale_amount/(83-sale_amount)) ~ discount+self_price+seas_index+as.factor(holi_index)+as.factor(store_display)+as.factor(flyer)+paid_search+radio+tv+web_display+email,data=transaction_product3[-1,])
summary(lm2_prod3)

# Validation --------------------------------------------------------------
#R^2
summary(lm_prod1)$r.squared

# SSTotal <- var(transaction_product1$sale_amount ) * (nrow(transaction_product1)-1)
# SSE<- sum((transaction_product1$sale_amount-exp(lm_prod1$fitted.values))^2)
# SSreg   <- SSTotal - SSE

#MAPE
MAPE(y_pred=exp(lm_prod1$fitted.values), y_true=transaction_product1$sale_amount)

#F-test
summary(lm_prod1)$fstatistic

# SST<- sum((log(transaction_product1$sale_amount)-mean(log(transaction_product1$sale_amount)))^2)
# SSE<- sum(lm_prod1$residuals^2)
# SSR<- SST-SSE
# 
# dfR<- length(lm_prod1$coefficients)-1
# dfE<- nrow(transaction_product1)-length(lm_prod1$coefficients)
# 
# F_test<- (SSR/dfR)/(SSE/dfE)

#VIF
vif(lm_prod1)

#DURBIN-WATSON STATISTIC
durbinWatsonTest(lm_prod1)



#prod1
pred2 <- predict(lm2_prod1, transaction_product1[-1,])
val_prod1<-data.frame('Model'= c('lm_prod1', 'lm2_prod1'))
val_prod1['rsquare']<-c(summary(lm_prod1)$r.squared,summary(lm2_prod1)$r.squared)
val_prod1['adj_rsquare']<-c(summary(lm_prod1)$adj.r.squared,summary(lm2_prod1)$adj.r.squared)
val_prod1['MAPE']<-c(MAPE(y_pred=exp(lm_prod1$fitted.values), y_true=transaction_product1$sale_amount),
                     MAPE(y_pred=330/(1+exp(-lm2_prod1$fitted.values)), y_true=transaction_product1[-1,]$sale_amount))
val_prod1['F_stat']<-c(summary(lm_prod1)$fstatistic[1],summary(lm2_prod1)$fstatistic[1])
val_prod1['dfR']<-c(summary(lm_prod1)$fstatistic[2],summary(lm2_prod1)$fstatistic[2])
val_prod1['dfE']<-c(summary(lm_prod1)$fstatistic[3],summary(lm2_prod1)$fstatistic[3])
val_prod1['DurbinWatson_stat']<-c(durbinWatsonTest(lm_prod1)$dw,durbinWatsonTest(lm2_prod1)$dw )

val_prod1_vari<-as.data.frame(vif(lm_prod1))
colnames(val_prod1_vari)[1]<-"VIF_lm"
val_prod1_vari['VIF_lm2']<-vif(lm2_prod1)
val_prod1_vari['Pr(>|t|)_lm']<- summary(lm_prod1)$coefficient[2:11,4]
val_prod1_vari['Pr(>|t|)_lm2']<- summary(lm2_prod1)$coefficient[2:11,4]

rownames(val_prod1_vari) <- c("discount","shelf_price","seasonality","holiday",
                              "flyer","paid_search","radio","tv",                   
                              "webdisplay","email"  )


#prod2
val_prod2<-data.frame('Model'= c('lm_prod2', 'lm2_prod2'))
val_prod2['rsquare']<-c(summary(lm_prod2)$r.squared,summary(lm2_prod2)$r.squared)
val_prod2['adj_rsquare']<-c(summary(lm_prod2)$adj.r.squared,summary(lm2_prod2)$adj.r.squared)
val_prod2['MAPE']<-c(MAPE(y_pred=exp(lm_prod2$fitted.values), y_true=transaction_product2$sale_amount),
                     MAPE(y_pred=227/(1+exp(-lm2_prod2$fitted.values)), y_true=transaction_product2[-1,]$sale_amount))
val_prod2['F_stat']<-c(summary(lm_prod2)$fstatistic[1],summary(lm2_prod2)$fstatistic[1])
val_prod2['dfR']<-c(summary(lm_prod2)$fstatistic[2],summary(lm2_prod2)$fstatistic[2])
val_prod2['dfE']<-c(summary(lm_prod2)$fstatistic[3],summary(lm2_prod2)$fstatistic[3])
val_prod2['DurbinWatson_stat']<-c(durbinWatsonTest(lm_prod2)$dw,durbinWatsonTest(lm2_prod2)$dw )

val_prod2_vari<-as.data.frame(vif(lm_prod2))
colnames(val_prod2_vari)[1]<-"VIF_lm"
val_prod2_vari['VIF_lm2']<-vif(lm2_prod2)
val_prod2_vari['Pr(>|t|)_lm']<- summary(lm_prod2)$coefficient[2:12,4]
val_prod2_vari['Pr(>|t|)_lm2']<- summary(lm2_prod2)$coefficient[2:12,4]

rownames(val_prod2_vari) <- c("discount","shelf_price","seasonality","holiday","store_display",
                              "flyer","paid_search","radio","tv",                   
                              "webdisplay","email"  )

#prod3
val_prod3<-data.frame('Model'= c('lm_prod3', 'lm2_prod3'))
val_prod3['rsquare']<-c(summary(lm_prod3)$r.squared,summary(lm2_prod3)$r.squared)
val_prod3['adj_rsquare']<-c(summary(lm_prod3)$adj.r.squared,summary(lm2_prod3)$adj.r.squared)
val_prod3['MAPE']<-c(MAPE(y_pred=exp(lm_prod3$fitted.values), y_true=transaction_product3$sale_amount),
                     MAPE(y_pred=83/(1+exp(-lm2_prod3$fitted.values)), y_true=transaction_product3[-1,]$sale_amount))
val_prod3['F_stat']<-c(summary(lm_prod3)$fstatistic[1],summary(lm2_prod3)$fstatistic[1])
val_prod3['dfR']<-c(summary(lm_prod3)$fstatistic[2],summary(lm2_prod3)$fstatistic[2])
val_prod3['dfE']<-c(summary(lm_prod3)$fstatistic[3],summary(lm2_prod3)$fstatistic[3])
val_prod3['DurbinWatson_stat']<-c(durbinWatsonTest(lm_prod3)$dw,durbinWatsonTest(lm2_prod3)$dw )

val_prod3_vari<-as.data.frame(vif(lm_prod3))
colnames(val_prod3_vari)[1]<-"VIF_lm"
val_prod3_vari['VIF_lm2']<-vif(lm2_prod3)
val_prod3_vari['Pr(>|t|)_lm']<- summary(lm_prod3)$coefficient[2:12,4]
val_prod3_vari['Pr(>|t|)_lm2']<- summary(lm2_prod3)$coefficient[2:12,4]

rownames(val_prod3_vari) <- c("discount","shelf_price","seasonality","holiday","store_display",
                              "flyer","paid_search","radio","tv",                   
                              "webdisplay","email"  )

val_prod_all<-rbind(val_prod1,val_prod2,val_prod3)


# Decomposition -----------------------------------------------------------

#For model 1

#create datatable to store variables at base condition
base_1 <- transaction_product1[-1,]
df_temp <- apply(base_1[,c(4,seq(9,15))],2,min)

s=1
for (i in c(4,seq(9,15))){
  base_1[[i]] <- df_temp[s]
  s=s+1
}

base_1$self_price <- max(base_1$self_price)

#input real amount, predicted base amount and predicted amount
df_new <- as.data.table(transaction_product1[-1,]$sale_amount)
colnames(df_new) <- 'real_amount'
df_new$base_amount <- predict(lm2_prod1, base_1) 
df_new$base_amount <- 330/(1+exp(-df_new$base_amount))

df_new$pred_amount <- predict(lm2_prod1, transaction_product1[-1,])
df_new$pred_amount <- 330/(1+exp(-df_new$pred_amount))

name_list <- names(base_1[,c(4,seq(9,15))])

#calculate duetos for all factors
for (i in c(4,seq(9,15))){
  df_temp <- transaction_product1[-1,]
  df_temp[[i]] <- min(df_temp[[i]])
  df_result <- as.data.table(predict(lm2_prod1, df_temp))
  df_result$V1 <- 330/(1+exp(-df_result$V1))
  colnames(df_result)<-colnames(transaction_product1)[i]
  df_new<- cbind(df_new,df_result)
}

df_temp <- transaction_product1[-1,]
df_temp[[5]] <- max(df_temp[[5]])
df_new$self_price <- as.data.table(predict(lm2_prod1, df_temp))
df_new$self_price <- 330/(1+exp(-df_new$self_price))

for (i in 4:12){
  df_new[[i]]<-df_new[[3]]-df_new[[i]]
}

#scale by the real volume
df_new$scale <- df_new$real_amount/df_new$pred_amount

for (i in c(2,4:12)){
  df_new[[i]]<-df_new[[i]]*df_new[[13]]
}

#write.csv(df_new, 'C:/Users/Godfr/Documents/mkt1.csv', row.names=FALSE)

#For model 2

#create datatable to store variables at base condition
base_2 <- transaction_product2[-1,]
df_temp <- apply(base_2[,c(4,seq(9,15))],2,min)

s=1
for (i in c(4,seq(9,15))){
  base_2[[i]] <- df_temp[s]
  s=s+1
}

base_2$self_price <- max(base_2$self_price)

#input real amount, predicted base amount and predicted amount
df_new <- as.data.table(transaction_product2[-1,]$sale_amount)
colnames(df_new) <- 'real_amount'
df_new$base_amount <- predict(lm2_prod2, base_2) 
df_new$base_amount <- 227/(1+exp(-df_new$base_amount))

df_new$pred_amount <- predict(lm2_prod2, transaction_product2[-1,])
df_new$pred_amount <- 227/(1+exp(-df_new$pred_amount))

name_list <- names(base_2[,c(4,seq(9,15))])

#calculate duetos for all factors
for (i in c(4,seq(9,15))){
  df_temp <- transaction_product2[-1,]
  df_temp[[i]] <- min(df_temp[[i]])
  df_result <- as.data.table(predict(lm2_prod2, df_temp))
  df_result$V1 <- 227/(1+exp(-df_result$V1))
  colnames(df_result)<-colnames(transaction_product2)[i]
  df_new<- cbind(df_new,df_result)
}

df_temp <- transaction_product2[-1,]
df_temp[[5]] <- max(df_temp[[5]])
df_new$self_price <- as.data.table(predict(lm2_prod2, df_temp))
df_new$self_price <- 227/(1+exp(-df_new$self_price))

for (i in 4:12){
  df_new[[i]]<-df_new[[3]]-df_new[[i]]
}

#scale by the real volume
df_new$scale <- df_new$real_amount/df_new$pred_amount

for (i in c(2,4:12)){
  df_new[[i]]<-df_new[[i]]*df_new[[13]]
}

#write.csv(df_new, 'C:/Users/Godfr/Documents/mkt2.csv', row.names=FALSE)


#For model 3

#create datatable to store variables at base condition
base_3 <- transaction_product3[-1,]
df_temp <- apply(base_3[,c(4,seq(9,15))],2,min)

s=1
for (i in c(4,seq(9,15))){
  base_3[[i]] <- df_temp[s]
  s=s+1
}

base_3$self_price <- max(base_3$self_price)

#input real amount, predicted base amount and predicted amount
df_new <- as.data.table(transaction_product3[-1,]$sale_amount)
colnames(df_new) <- 'real_amount'
df_new$base_amount <- predict(lm2_prod3, base_3) 
df_new$base_amount <- 83/(1+exp(-df_new$base_amount))

df_new$pred_amount <- predict(lm2_prod3, transaction_product3[-1,])
df_new$pred_amount <- 83/(1+exp(-df_new$pred_amount))

name_list <- names(base_3[,c(4,seq(9,15))])

#calculate duetos for all factors
for (i in c(4,seq(9,15))){
  df_temp <- transaction_product3[-1,]
  df_temp[[i]] <- min(df_temp[[i]])
  df_result <- as.data.table(predict(lm2_prod3, df_temp))
  df_result$V1 <- 83/(1+exp(-df_result$V1))
  colnames(df_result)<-colnames(transaction_product3)[i]
  df_new<- cbind(df_new,df_result)
}

df_temp <- transaction_product3[-1,]
df_temp[[5]] <- max(df_temp[[5]])
df_new$self_price <- as.data.table(predict(lm2_prod3, df_temp))
df_new$self_price <- 83/(1+exp(-df_new$self_price))

for (i in 4:12){
  df_new[[i]]<-df_new[[3]]-df_new[[i]]
}

#scale by the real volume
df_new$scale <- df_new$real_amount/df_new$pred_amount

for (i in c(2,4:12)){
  df_new[[i]]<-df_new[[i]]*df_new[[13]]
}

#write.csv(df_new, 'C:/Users/Godfr/Documents/mkt3.csv', row.names=FALSE)
