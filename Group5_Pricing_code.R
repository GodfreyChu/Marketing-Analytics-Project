# Initializing ------------------------------------------------------------
#import necessary packages
library(data.table)
library(dplyr)
library(ggplot2)
library(stats)

memory.limit(size=10000000)

#read data from local drive
product<-fread('C:/Users/Godfr/Documents/US/Marketing/product_table.csv')
options(scipen = 999)
transaction<-fread('C:/Users/Godfr/Documents/US/Marketing/transaction_table.csv')
#fix the issue of transaction id
transaction$tran_id<-sapply(1:nrow(transaction), function(x) paste(substr(toString(transaction$tran_id[x]),1,8),toString(transaction$cust_id[x]),toString(transaction$store_id[x]),sep = ''))
#add a column of year-week
transaction$week<-format(as.Date(transaction$tran_dt), "%Y-%W")
#20180402-20180407 is the 14th week
format(as.Date('2018-04-02'), "%Y-%W")
#exclude fresh products
product_new<-product[substr(product$category_desc_eng,1,5)!='FRESH',]


# Select category (Final choice is Ice_cream and Child_food)---------------------------------------------------------

#merge transaction with product
transaction_new <- merge(transaction,product_new,by="prod_id")
transaction_new$prod_unit_paid <- transaction_new$tran_prod_paid_amt/transaction_new$tran_prod_sale_qty

#write csv
#write.csv(transaction_new,'C:/Users/Godfr/Documents/US/Marketing/transaction_new.csv')

#sort the category based on revenue, extract top 20
product_count <-  transaction_new[,.(.N,sum(tran_prod_paid_amt)),by=.(category_id,prod_id)]
category_count <- product_count[,.(.N,sum(V2)),by=.(category_id)]
#keep categories with more 100 products
category_over100 <- category_count[category_count$N>=100,]

trans <- transaction_new %>% group_by(week) %>% summarise(trans = length(unique(tran_id))) 
trans <- as.data.table(trans)

#wine 95797, e:0.4682 violation of assumption
transaction_wine <- transaction_new[transaction_new$category_id == 95797,]
wine_data <- transaction_wine %>% group_by(week) %>% summarise(price = mean(prod_unit_price), demand = sum(tran_prod_sale_qty))
wine_data <- as.data.table(wine_data)
wine_data <- merge(wine_data,trans, all.x=TRUE, by='week')
wine_data$demand <- wine_data$demand/wine_data$trans 

lm_wine <- glm(log(demand)~log(price),data=wine_data)
summary(lm_wine)

#yogurt 95854, e:-0.23597
transaction_yogurt <- transaction_new[transaction_new$category_id == 95854,]
yogurt_data <- transaction_yogurt %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
yogurt_data <- as.data.table(yogurt_data)
yogurt_data <- merge(yogurt_data,trans, all.x=TRUE, by='week')
yogurt_data$demand <- yogurt_data$demand/yogurt_data$trans 

lm_yogurt <- glm(log(demand)~log(price),data=yogurt_data)
summary(lm_yogurt)

#coffee 96026, e:-0.8849
transaction_coffee <- transaction_new[transaction_new$category_id == 96026,]
coffee_data <- transaction_coffee %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
coffee_data <- as.data.table(coffee_data)
coffee_data <- merge(coffee_data,trans, all.x=TRUE, by='week')
coffee_data$demand <- coffee_data$demand/coffee_data$trans 

lm_coffee <- glm(log(demand)~log(price),data=coffee_data)
summary(lm_coffee)

#detergent 95788, e:-0.009619
transaction_detergent <- transaction_new[transaction_new$category_id == 95788,]
detergent_data <- transaction_detergent %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
detergent_data <- as.data.table(detergent_data)
detergent_data <- merge(detergent_data,trans, all.x=TRUE, by='week')
detergent_data$demand <- detergent_data$demand/detergent_data$trans 

lm_detergent <- glm(log(demand)~log(price),data=detergent_data)
summary(lm_detergent)

#wafers 95991, e:-0.31110
transaction_wafers <- transaction_new[transaction_new$category_id == 95991,]
wafers_data <- transaction_wafers %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
wafers_data <- as.data.table(wafers_data)
wafers_data <- merge(wafers_data,trans, all.x=TRUE, by='week')
wafers_data$demand <- wafers_data$demand/wafers_data$trans 

lm_wafers <- glm(log(demand)~log(price),data=wafers_data)
summary(lm_wafers)

#juices 95803, e:-0.56184
transaction_juices <- transaction_new[transaction_new$category_id == 95803,]
juices_data <- transaction_juices %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
juices_data <- as.data.table(juices_data)
juices_data <- merge(juices_data,trans, all.x=TRUE, by='week')
juices_data$demand <- juices_data$demand/juices_data$trans 

lm_juices <- glm(log(demand)~log(price),data=juices_data)
summary(lm_juices)

#cereals 96027, e:-0.3491
transaction_cereals <- transaction_new[transaction_new$category_id == 96027,]
cereals_data <- transaction_cereals %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
cereals_data <- as.data.table(cereals_data)
cereals_data <- merge(cereals_data,trans, all.x=TRUE, by='week')
cereals_data$demand <- cereals_data$demand/cereals_data$trans 

lm_cereals <- glm(log(demand)~log(price),data=cereals_data)
summary(lm_cereals)

#chocolate 95998, e:-0.15970
transaction_chocolate <- transaction_new[transaction_new$category_id == 95998,]
chocolate_data <- transaction_chocolate %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
chocolate_data <- as.data.table(chocolate_data)
chocolate_data <- merge(chocolate_data,trans, all.x=TRUE, by='week')
chocolate_data$demand <- chocolate_data$demand/chocolate_data$trans 

lm_chocolate <- glm(log(demand)~log(price),data=chocolate_data)
summary(lm_chocolate)

#deodorisers 95747, e:-0.2727
transaction_deodorisers <- transaction_new[transaction_new$category_id == 95747,]
deodorisers_data <- transaction_deodorisers %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
deodorisers_data <- as.data.table(deodorisers_data)
deodorisers_data <- merge(deodorisers_data,trans, all.x=TRUE, by='week')
deodorisers_data$demand <- deodorisers_data$demand/deodorisers_data$trans 

lm_deodorisers <- glm(log(demand)~log(price),data=deodorisers_data)
summary(lm_deodorisers)

#ice_cream 95863, e:-3.8546
transaction_ice_cream <- transaction_new[transaction_new$category_id == 95863,]
ice_cream_data <- transaction_ice_cream %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
ice_cream_data <- as.data.table(ice_cream_data)
ice_cream_data <- merge(ice_cream_data,trans, all.x=TRUE, by='week')
ice_cream_data$demand <- ice_cream_data$demand/ice_cream_data$trans 

lm_ice_cream <- glm(log(demand)~log(price),data=ice_cream_data)
summary(lm_ice_cream)

#hair_cond 95730, e:-0.3099
transaction_hair_cond <- transaction_new[transaction_new$category_id == 95730,]
hair_cond_data <- transaction_hair_cond %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
hair_cond_data <- as.data.table(hair_cond_data)
hair_cond_data <- merge(hair_cond_data,trans, all.x=TRUE, by='week')
hair_cond_data$demand <- hair_cond_data$demand/hair_cond_data$trans 

lm_hair_cond <- glm(log(demand)~log(price),data=hair_cond_data)
summary(lm_hair_cond)

#shampoo 95731, e:-0.3501
transaction_shampoo <- transaction_new[transaction_new$category_id == 95731,]
shampoo_data <- transaction_shampoo %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
shampoo_data <- as.data.table(shampoo_data)
shampoo_data <- merge(shampoo_data,trans, all.x=TRUE, by='week')
shampoo_data$demand <- shampoo_data$demand/shampoo_data$trans 

lm_shampoo <- glm(log(demand)~log(price),data=shampoo_data)
summary(lm_shampoo)

#feeding 96006, e:-0.5477
transaction_feeding <- transaction_new[transaction_new$category_id == 96006,]
feeding_data <- transaction_feeding %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
feeding_data <- as.data.table(feeding_data)
feeding_data <- merge(feeding_data,trans, all.x=TRUE, by='week')
feeding_data$demand <- feeding_data$demand/feeding_data$trans 

lm_feeding <- glm(log(demand)~log(price),data=feeding_data)
summary(lm_feeding)

#animal_dryfood 95594, e:0.05030 violation of assumption
transaction_animal_dryfood <- transaction_new[transaction_new$category_id == 95594,]
animal_dryfood_data <- transaction_animal_dryfood %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
animal_dryfood_data <- as.data.table(animal_dryfood_data)
animal_dryfood_data <- merge(animal_dryfood_data,trans, all.x=TRUE, by='week')
animal_dryfood_data$demand <- animal_dryfood_data$demand/animal_dryfood_data$trans 

lm_animal_dryfood <- glm(log(demand)~log(price),data=animal_dryfood_data)
summary(lm_animal_dryfood)

#child_food 96029, e:-1.5029
transaction_child_food <- transaction_new[transaction_new$category_id == 96029,]
child_food_data <- transaction_child_food %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
child_food_data <- as.data.table(child_food_data)
child_food_data <- merge(child_food_data,trans, all.x=TRUE, by='week')
child_food_data$demand <- child_food_data$demand/child_food_data$trans 

lm_child_food <- glm(log(demand)~log(price),data=child_food_data)
summary(lm_child_food)

#sausages 95811, e:-0.90394
transaction_sausages <- transaction_new[transaction_new$category_id == 95811,]
sausages_data <- transaction_sausages %>% group_by(week) %>% summarise(price = mean(prod_unit_paid), demand = sum(tran_prod_sale_qty))
sausages_data <- as.data.table(sausages_data)
sausages_data <- merge(sausages_data,trans, all.x=TRUE, by='week')
sausages_data$demand <- sausages_data$demand/sausages_data$trans 

lm_sausages <- glm(log(demand)~log(price),data=sausages_data)
summary(lm_sausages)


# Build sample model to select 100 products --------------------------------------

#filter the products and subcategories in the selected two categories
product_filter <- product[category_id%in%c(95863,96029),]

subcat_list<-list(unique(transaction_new$subcategory_id))
subcat_filter <- list(unique(product_filter$subcategory_id))
#transaction_new <- as.data.table(transaction_new)

#number of transaction is 2815619 in total
length(unique(transaction_new$tran_id))

#build lift matrix to find the complementary products for each subcategory
lift_matrix <- matrix(nrow=1410,ncol=10)

for (i in 1:10){
  transaction_filter <- transaction_new[subcategory_id==subcat_filter[[1]][i],]$tran_id
  for (j in 1:1410){
    transaction_inter <- transaction_new[subcategory_id==subcat_list[[1]][j],]$tran_id
    lift_matrix[j,i] <- sum(transaction_filter%in%transaction_inter)/(length(transaction_filter)*length(transaction_inter))*2815619
  }
}

lift_matrix <- as.data.frame(lift_matrix)
rownames(lift_matrix) <- as.character(unlist(subcat_list)) 
colnames(lift_matrix) <- as.character(unlist(subcat_filter))


#try the first product 999543475
product_id = 999543475
transaction_temp <- transaction_new[transaction_new$prod_id == product_id,]
#get data of product's price, demand and discount information
temp_data <- transaction_temp %>% group_by(week) %>% dplyr::summarise(price = mean(prod_unit_price), demand = sum(tran_prod_sale_qty), discount = abs(sum(tran_prod_discount_amt))/sum(tran_prod_sale_amt))
temp_data <- as.data.table(temp_data)
temp_data <- temp_data[-1,]

#get data of product's substitude price
sub_id <- product[prod_id==product_id,subcategory_id]
substitude <- transaction_new[(subcategory_id == sub_id)&(prod_id != product_id),] 
substitude_data <- substitude %>% group_by(week) %>% dplyr::summarise(price_sub = mean(prod_unit_price))
substitude_data <- as.data.table(substitude_data)

#get data of product's complementary price
com_id <- sort(lift_matrix[,as.character(sub_id)],decreasing = TRUE)[2:4]
com_id <- which(lift_matrix[,as.character(sub_id)]%in%com_id)
com_id <- subcat_list[[1]][c(unlist(com_id))]
complementary <- transaction_new[(subcategory_id %in% com_id),] 
complementary_data <- complementary %>% group_by(week) %>% dplyr::summarise(price_com = mean(prod_unit_price))
complementary_data <- as.data.table(complementary_data)

#get data of seasonality
temp_data$seasonality <- 0
#if same week of two years are both over 10% higher/lower than average, seasonality is set to 1/-1 
for (s in 1:length(temp_data$week)){
  week_temp <- substr(temp_data[s]$week,6,7)
  if (nrow(temp_data[substr(temp_data$week,6,7)==week_temp,])>1){
    if (temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2016',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2016',]$demand)-1>0.1 & 
        temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2017',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2017',]$demand)-1>0.1) {
      temp_data[s]$seasonality <- 1
    } else if (temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2016',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2016',]$demand)-1< -0.1 & 
               temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2017',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2017',]$demand)-1< -0.1){
      temp_data[s]$seasonality <- -1
    } 
  }
}

#merge tables
temp_data <- merge(temp_data, substitude_data, all.x=TRUE, by='week')
temp_data <- merge(temp_data, complementary_data, all.x=TRUE, by='week')

#build model
if (length(unique(temp_data$seasonality))>=2){
  lm_temp <- glm(log(demand)~log(price)+log(price_sub)+log(price_com)+as.factor(seasonality)+discount,data=temp_data)
} else {
  lm_temp <- glm(log(demand)~log(price)+log(price_sub)+log(price_com)+discount,data=temp_data)
}
summary(lm_temp)


#############################################################
##                apply to all products                    ##
#############################################################
elasticity_table<-data.table(Product='Prod_id',Elasticity=NA)

for (product_id in product_filter$prod_id){
  transaction_temp <- transaction_new[transaction_new$prod_id == product_id,]
  #get data of product's price, demand and discount information
  temp_data <- transaction_temp %>% group_by(week) %>% dplyr::summarise(price = mean(prod_unit_price), demand = sum(tran_prod_sale_qty), discount = abs(sum(tran_prod_discount_amt))/sum(tran_prod_sale_amt))
  temp_data <- as.data.table(temp_data)
  temp_data$demand <- temp_data$demand/trans$trans 
  
  #get data of product's substitude price
  sub_id <- product[prod_id==product_id,subcategory_id]
  substitude <- transaction_new[(subcategory_id == sub_id)&(prod_id != product_id),] 
  substitude_data <- substitude %>% group_by(week) %>% dplyr::summarise(price_sub = mean(prod_unit_price))
  substitude_data <- as.data.table(substitude_data)
  
  #get data of product's complementary price
  com_id <- sort(lift_matrix[,as.character(sub_id)],decreasing = TRUE)[2:4]
  com_id <- which(lift_matrix[,as.character(sub_id)]%in%com_id)
  com_id <- subcat_list[[1]][c(unlist(com_id))]
  complementary <- transaction_new[(subcategory_id %in% com_id),] 
  complementary_data <- complementary %>% group_by(week) %>% dplyr::summarise(price_com = mean(prod_unit_price))
  complementary_data <- as.data.table(complementary_data)
  
  #get data of seasonality
  temp_data$seasonality <- 0
  #if same week of two years are both over 10% higher/lower than average, seasonality is set to 1/-1 
  
  for (s in 1:length(temp_data$week)){
    week_temp <- substr(temp_data[s]$week,6,7)
    if (nrow(temp_data[substr(temp_data$week,6,7)==week_temp,])>1){
      if (temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2016',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2016',]$demand)-1>0.1 & 
          temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2017',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2017',]$demand)-1>0.1) {
        temp_data[s]$seasonality <- 1
      } else if (temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2016',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2016',]$demand)-1< -0.1 & 
                 temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2017',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2017',]$demand)-1< -0.1){
        temp_data[s]$seasonality <- -1
      } 
    }
  }
  
  #merge tables
  temp_data <- merge(temp_data, substitude_data, all.x=TRUE, by='week')
  temp_data <- merge(temp_data, complementary_data, all.x=TRUE, by='week')
  
  #build model
  if (any(is.na(temp_data))){
    next
  } else{
    if (length(unique(temp_data$seasonality))>=2){
      lm_temp <- glm(log(demand)~log(price)+log(price_sub)+log(price_com)+as.factor(seasonality)+discount,data=temp_data)
    } else {
      lm_temp <- glm(log(demand)~log(price)+log(price_sub)+log(price_com)+discount,data=temp_data)
    } 
    elasticity_table<- rbind(elasticity_table, data.table(Product=as.character(product_id), Elasticity=lm_temp$coefficients[2]))
  }
}


#select 100 products with most elasticity, 83 are elastic(e<-1) and 17 products are inelastic(-1<e<0)
elasticity_table <- elasticity_table[order(Elasticity),]
product_table <- elasticity_table[1:100,]

# Solution for 100 products and 10 stores ---------------------------------

##Across stores
result_table_full <- data.table()

for (product_id in product_table$Product){
  transaction_temp <- transaction_new[transaction_new$prod_id == product_id,]
  #get data of product's price, demand and discount information
  temp_data <- transaction_temp %>% group_by(week) %>% dplyr::summarise(price = mean(prod_unit_price), demand = sum(tran_prod_sale_qty), discount = abs(sum(tran_prod_discount_amt))/sum(tran_prod_sale_amt))
  temp_data <- as.data.table(temp_data)
  temp_data$demand <- temp_data$demand/trans$trans 
  
  #get data of product's substitude price
  sub_id <- product[prod_id==product_id,subcategory_id]
  substitude <- transaction_new[(subcategory_id == sub_id)&(prod_id != product_id),] 
  substitude_data <- substitude %>% group_by(week) %>% dplyr::summarise(price_sub = mean(prod_unit_price))
  substitude_data <- as.data.table(substitude_data)
  
  #get data of product's complementary price
  com_id <- sort(lift_matrix[,as.character(sub_id)],decreasing = TRUE)[2:4]
  com_id <- which(lift_matrix[,as.character(sub_id)]%in%com_id)
  com_id <- subcat_list[[1]][c(unlist(com_id))]
  complementary <- transaction_new[(subcategory_id %in% com_id),] 
  complementary_data <- complementary %>% group_by(week) %>% dplyr::summarise(price_com = mean(prod_unit_price))
  complementary_data <- as.data.table(complementary_data)
  
  #get data of seasonality
  temp_data$seasonality <- 0
  #if same week of two years are both over 10% higher/lower than average, seasonality is set to 1/-1 
  
  for (s in 1:length(temp_data$week)){
    week_temp <- substr(temp_data[s]$week,6,7)
    if (nrow(temp_data[substr(temp_data$week,6,7)==week_temp,])>1){
      if (temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2016',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2016',]$demand)-1>0.1 & 
          temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2017',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2017',]$demand)-1>0.1) {
        temp_data[s]$seasonality <- 1
      } else if (temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2016',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2016',]$demand)-1< -0.1 & 
                 temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2017',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2017',]$demand)-1< -0.1){
        temp_data[s]$seasonality <- -1
      } 
    }
  }
  
  #merge tables
  temp_data <- merge(temp_data, substitude_data, all.x=TRUE, by='week')
  temp_data <- merge(temp_data, complementary_data, all.x=TRUE, by='week')
  
  #build model
  if (any(is.na(temp_data))){
    next
  } else{
    if (length(unique(temp_data$seasonality))>=2){
      lm_temp <- glm(log(demand)~log(price)+log(price_sub)+log(price_com)+as.factor(seasonality)+discount,data=temp_data)
    } else {
      lm_temp <- glm(log(demand)~log(price)+log(price_sub)+log(price_com)+discount,data=temp_data)
    } 
    
    #using latest price, and discount information in 14th week in 2017
    result_table <- data.table(prod_id=product_id)
    result_table$week <- '2018-14'
    result_table$price <- tail(temp_data$price,1) 
    result_table$demand <- temp_data[week=='2017-14',demand]
    result_table$discount <- temp_data[week=='2017-14',discount]
    result_table$seasonality <- temp_data[week=='2017-14',seasonality]
    result_table$price_sub <- tail(temp_data$price_sub,1)
    result_table$price_com <- tail(temp_data$price_com,1)
    
    #a practice that reduce 10% price if it's elastic(e<-1) and increase 10% price if it's inelastic(-1<e<0)
    if (lm_temp$coefficients[2]< -1){
      result_table$recommend_price <- tail(temp_data$price,1)*0.9
    } else {
      result_table$recommend_price <- tail(temp_data$price,1)*1.1
    }
    
    #store and compare the revenue generated using latest price/recommended price
    result_table$rev_origin <- exp(predict.glm(lm_temp,newdata = result_table))*trans[week=='2017-14',]$trans*result_table$price
    result_table$rev_new <- exp(predict.glm(lm_temp,newdata = data.frame(price=result_table$recommend_price, discount=result_table$discount,
                                                                         seasonality=result_table$seasonality, price_sub=result_table$price_sub,
                                                                         price_com=result_table$price_com)))*trans[week=='2017-14',]$trans*result_table$recommend_price
    result_table$rev_increase <- result_table$rev_new - result_table$rev_origin
    
    temp_data$pred <- predict.glm(lm_temp,newdata = temp_data)
    result_table$explained_variance_ratio <- 1-cov(matrix(temp_data$demand-exp(temp_data$pred)))/cov(matrix(temp_data$demand))
    result_table$elasticity <- product_table[Product==product_id,'Elasticity']
    
    result_table_full <- rbind(result_table_full,result_table)
  }
}

##For 10 different stores
result_table_store <- data.table()

#The 10 stores we are going to check 
data_store <- transaction_new[transaction_new$prod_id%in%product_table$product,] %>% group_by(store_id) %>% dplyr::summarise(amount = sum(tran_prod_paid_amt))
data_store <- as.data.table(data_store)
data_store <- data_store %>% arrange(-amount)
data_store <- data_store[1:10,]

for (q in 1:10){
  store <- as.numeric(data_store[q,1])
  transaction_store <- transaction_new[store_id==store,]
  
  trans_store <- transaction_store %>% group_by(week) %>% dplyr::summarise(trans = length(unique(tran_id))) 
  trans_store <- as.data.table(trans_store)
  
  for (product_id in product_table$Product){
    transaction_temp <- transaction_store[transaction_store$prod_id == product_id,]
    if (length(unique(transaction_temp$week))<=2){
      next
    }
    #get data of product's price, demand and discount information
    temp_data <- transaction_temp %>% group_by(week) %>% dplyr::summarise(price = mean(prod_unit_price), demand = sum(tran_prod_sale_qty), discount = abs(sum(tran_prod_discount_amt))/sum(tran_prod_sale_amt))
    temp_data <- as.data.table(temp_data)
    temp_data$demand <- temp_data$demand/trans_store$trans 
    
    #get data of product's substitude price
    sub_id <- product[prod_id==product_id,subcategory_id]
    substitude <- transaction_store[(subcategory_id == sub_id)&(prod_id != product_id),] 
    substitude_data <- substitude %>% group_by(week) %>% dplyr::summarise(price_sub = mean(prod_unit_price))
    substitude_data <- as.data.table(substitude_data)
    
    #get data of product's complementary price
    com_id <- sort(lift_matrix[,as.character(sub_id)],decreasing = TRUE)[2:4]
    com_id <- which(lift_matrix[,as.character(sub_id)]%in%com_id)
    com_id <- subcat_list[[1]][c(unlist(com_id))]
    complementary <- transaction_store[(subcategory_id %in% com_id),] 
    complementary_data <- complementary %>% group_by(week) %>% dplyr::summarise(price_com = mean(prod_unit_price))
    complementary_data <- as.data.table(complementary_data)
    
    #get data of seasonality
    temp_data$seasonality <- 0
    #if same week of two years are both over 10% higher/lower than average, seasonality is set to 1/-1 
    
    for (s in 1:length(temp_data$week)){
      week_temp <- substr(temp_data[s]$week,6,7)
      if (nrow(temp_data[substr(temp_data$week,6,7)==week_temp,])>1){
        if (temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2016',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2016',]$demand)-1>0.1 & 
            temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2017',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2017',]$demand)-1>0.1) {
          temp_data[s]$seasonality <- 1
        } else if (temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2016',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2016',]$demand)-1< -0.1 & 
                   temp_data[substr(temp_data$week,6,7)==week_temp & substr(temp_data$week,1,4)=='2017',]$demand/mean(temp_data[substr(temp_data$week,1,4)=='2017',]$demand)-1< -0.1){
          temp_data[s]$seasonality <- -1
        } 
      }
    }
    
    #merge tables
    temp_data <- merge(temp_data, substitude_data, all.x=TRUE, by='week')
    temp_data <- merge(temp_data, complementary_data, all.x=TRUE, by='week')
    
    #deal with NAs
    temp_data <- lapply(temp_data, function(x) { 
      x[is.na(x)] <- mean(x, na.rm = TRUE)
      x
    })
    temp_data <- as.data.table(temp_data)
    
    #build model
    if (sum(complete.cases(temp_data))<=2){
      next
    } else{
      if (length(unique(temp_data$seasonality))>=2){
        lm_temp <- glm(log(demand)~log(price)+log(price_sub)+log(price_com)+as.factor(seasonality)+discount,data=temp_data,na.action = na.omit)
      } else {
        lm_temp <- glm(log(demand)~log(price)+log(price_sub)+log(price_com)+discount,data=temp_data,na.action = na.omit)
      } 
      
      if (is.na(lm_temp$coefficients[2])){
        next
      } else{
        #using latest price, and discount information in 14th week in 2017
        result_table <- data.table(prod_id=product_id)
        result_table$week <- '2018-14'
        result_table$price <- tail(temp_data$price,1) 
        result_table$demand <- temp_data[week=='2017-14',demand]
        result_table$discount <- temp_data[week=='2017-14',discount]
        result_table$seasonality <- temp_data[week=='2017-14',seasonality]
        result_table$price_sub <- tail(temp_data$price_sub,1)
        result_table$price_com <- tail(temp_data$price_com,1)
        result_table$store <- store
        
        #a practice that reduce 10% price if it's elastic(e<-1) and increase 10% price if it's inelastic(-1<e<0)
        if (lm_temp$coefficients[2]< -1){
          result_table$recommend_price <- tail(temp_data$price,1)*0.9
        } else {
          result_table$recommend_price <- tail(temp_data$price,1)*1.1
        }
        
        #store and compare the revenue generated using latest price/recommended price
        result_table$rev_origin <- exp(predict.glm(lm_temp,newdata = result_table))*trans_store[week=='2017-14',]$trans*result_table$price
        result_table$rev_new <- exp(predict.glm(lm_temp,newdata = data.frame(price=result_table$recommend_price, discount=result_table$discount,
                                                                             seasonality=result_table$seasonality, price_sub=result_table$price_sub,
                                                                             price_com=result_table$price_com)))*trans_store[week=='2017-14',]$trans*result_table$recommend_price
        result_table$rev_increase <- result_table$rev_new - result_table$rev_origin
        
        result_table_store <- rbind(result_table_store,result_table)
      }
    }
  }
}

#write csv
#write.csv(result_table_full,'C:/Users/Godfr/Documents/US/Marketing/result_table_full.csv')
#write.csv(result_table_store,'C:/Users/Godfr/Documents/US/Marketing/result_table_store.csv')

#save product across 10 stores data
result_full <- result_table_full[,c('prod_id','rev_increase')]
table_full_temp <- na.omit(result_table_full, cols = c('prod_id','rev_increase'))
result_full <- na.omit(result_full)
result_full$rev_increase_ratio <- table_full_temp$rev_increase/table_full_temp$rev_origin*100
result_full[rev_increase_ratio>100,]$rev_increase_ratio <-NA
result_full$rev_increase_ratio <- as.character(result_full$rev_increase_ratio)
result_full[is.na(rev_increase_ratio),]$rev_increase_ratio <-'>100'
result_full$rev_increase_ratio <- paste(result_full$rev_increase_ratio,'%',sep = '')
#if change rate is higher than 100%, show as >100%
result_full$quant_change <- (table_full_temp$rev_new/table_full_temp$recommend_price)-(table_full_temp$rev_origin/table_full_temp$price)
result_full$quant_change_ratio <- result_full$quant_change/((table_full_temp$rev_origin/table_full_temp$price))*100
result_full[quant_change_ratio>100,]$quant_change_ratio <-NA
result_full$quant_change_ratio <- as.character(result_full$quant_change_ratio)
result_full[is.na(quant_change_ratio),]$quant_change_ratio <-'>100'
result_full$quant_change_ratio <- paste(result_full$quant_change_ratio,'%',sep = '')

#write.csv(result_full,'C:/Users/Godfr/Documents/US/Marketing/change_full.csv')

#save product-store data
result_store <- result_table_store[,c('prod_id','store','rev_increase')]
table_store_temp <- na.omit(result_table_store, cols = c('prod_id','store','rev_increase'))
result_store <- na.omit(result_store)
result_store$rev_increase_ratio <- table_store_temp$rev_increase/table_store_temp$rev_origin*100
result_store[rev_increase_ratio>100,]$rev_increase_ratio <-NA
result_store$rev_increase_ratio <- as.character(result_store$rev_increase_ratio)
result_store[is.na(rev_increase_ratio),]$rev_increase_ratio <-'>100'
result_store$rev_increase_ratio <- paste(result_store$rev_increase_ratio,'%',sep = '')
#if change rate is higher than 100%, show as >100%
result_store$quant_change <- (table_store_temp$rev_new/table_store_temp$recommend_price)-(table_store_temp$rev_origin/table_store_temp$price)
result_store$quant_change_ratio <- result_store$quant_change/((table_store_temp$rev_origin/table_store_temp$price))*100
result_store[quant_change_ratio>100,]$quant_change_ratio <-NA
result_store$quant_change_ratio <- as.character(result_store$quant_change_ratio)
result_store[is.na(quant_change_ratio),]$quant_change_ratio <-'>100'
result_store$quant_change_ratio <- paste(result_store$quant_change_ratio,'%',sep = '')

#write.csv(result_store,'C:/Users/Godfr/Documents/US/Marketing/change_store.csv')

product_full <- result_table_full[,c('product','elasticity')]
product_full$price_origin <- result_table_full$price
product_full$price_recommend <- result_table_full$recommend_price
product_full$price_change <- '10%'
product_full[elasticity< -1,]$price_change<-'-10%'

#write.csv(product_full,'C:/Users/Godfr/Documents/US/Marketing/data_product.csv')

###adjust demand and Revenue estimation for extreme values
adj_revenue<- data.table(prod_id=result_full$prod_id[1:27])

for (i in 1:27) {
  product_id = adj_revenue$prod_id[i]
  transaction_temp <- transaction_new[transaction_new$prod_id == product_id,]
  #get data of product's price, demand 
  temp <- transaction_temp %>% group_by(week) %>% dplyr::summarise(price = mean(prod_unit_price), demand = sum(tran_prod_sale_qty))
  temp <- as.data.table(temp)
  lm <- glm(demand~price+price_sub+price_com+discount,data=temp)
  adj_revenue$adjusted_demand[i]<-predict.glm(lm,price=product_full$price_recommend[product_full$prod_id==product_id])[1]
  adj_revenue$adjusted_revenue[i]<-adj_revenue$adjusted_demand[i]*product_full$price_recommend[product_full$prod_id==product_id]
}


final_table<-result_table_full[,c(1,3,9,10,11)]
final_table$sales<-final_table$rev_origin/(final_table$price)
final_table$sales_new<-final_table$rev_new/(final_table$recommend_price)

for (id in adj_revenue$prod_id) {
  final_table$sales_new[final_table$prod_id==id]<-adj_revenue$adjusted_demand[adj_revenue$prod_id==id]
  final_table$rev_new[final_table$prod_id==id]<-adj_revenue$adjusted_revenue[adj_revenue$prod_id==id]
}


final_table$rev_increase_ratio<-(final_table$rev_new-final_table$rev_origin)/final_table$rev_origin
final_table$rev_increase<-final_table$rev_new-final_table$rev_origin
final_table$quant_change_ratio<-(final_table$sales_new-final_table$sales)/final_table$sales
final_table$quant_change<-final_table$sales_new-final_table$sales

final_table<- final_table[,c('prod_id','quant_change','quant_change_ratio','rev_increase','rev_increase_ratio')]

#write.csv(final_table, file = "Final_table.csv")

# Validation & Visualization ----------------------------------------------

sum(result_table_full$rev_increase/result_table_full$rev_origin >1, na.rm = TRUE)
sum(result_table_full$rev_increase/result_table_full$rev_origin <1, na.rm = TRUE)

#check the average increase rate
temp_temp <- result_table_full$rev_increase/result_table_full$rev_origin
temp_temp <- temp_temp[temp_temp<1]
mean(temp_temp,na.rm = TRUE)

#25 of them have increase rate > 100%

#Validate the sample product model (999543475)
temp_data$pred <- predict.glm(lm_temp,newdata = temp_data)
plot(temp_data$price, exp(temp_data$pred), pch = 16, xlab = "price", ylab = "demand")

plot_table<-data.table(matrix(nrow=201,ncol = 2))
plot_table$xweight <- seq(1.5, 3.5, 0.01)
plot_table$yweight <- predict(lm_temp, list(price = xweight, price_sub = rep(tail(temp_data$price_sub,1),201), price_com = rep(tail(temp_data$price_com,1),201), 
                                 seasonality = rep(temp_data[week=='2017-14',seasonality],201), discount = rep(temp_data[week=='2017-14',discount],201)))

#Plot of Revenue vs Price
ggplot(aes(x=xweight, y=exp(yweight)*xweight),data = plot_table)+geom_line(size=1.2)+theme_light()+ylab('Revenue')+xlab('Price')+geom_vline(xintercept =  tail(temp_data$price,1),colour='red',size=0.9)

#Plot of Demand vs Price
ggplot(aes(x=xweight, y=exp(yweight)),data = plot_table)+geom_line(size=1.2)+theme_light()+ylab('Demand')+xlab('Price')+geom_vline(xintercept =  tail(temp_data$price,1),colour='red',size=0.9)

#explained variance score
1-cov(matrix(temp_data$demand-exp(temp_data$pred)))/cov(matrix(temp_data$demand))

