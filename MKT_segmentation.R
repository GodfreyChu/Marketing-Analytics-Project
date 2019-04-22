library(data.table)
library(dplyr)
library(ggplot2)
product<-fread('C:/Users/Godfr/Documents/US/Marketing/product_table.csv')
transaction<-fread('C:/Users/Godfr/Documents/US/Marketing/transaction_table.csv')

transaction$tran_id <- format(transaction$tran_id, scientific = FALSE) 
transaction$tran_id <- paste(transaction$cust_id,transaction$tran_dt,sep=' ')
transaction$tran_id<-as.character(transaction$tran_id)
head(transaction$tran_id)
length(unique(transaction$tran_id))
head(product)

product_short<-product[,c(1,2,4,6)]
options(scipen = 999,digits = 2)
head(transaction$tran_id)

data<-merge(transaction,product_short,by=intersect('prod_id','prod_id'))
head(data)


# A -----------------------------------------------------------------------


# Who are the best customers in terms of revenues, profits, transactions/store visits, number of products, etc.?
data$cust_id<-as.character(data$cust_id)
#data$tran_id<-as.character(data$tran_id)
data$prod_id<-as.character(data$prod_id)

customer_information<-data %>% group_by(cust_id) %>% summarise(revenues = sum(tran_prod_paid_amt),transactions = 
                                                                 n_distinct(tran_id),products = n_distinct(prod_id))
customer_information<-as.data.table(customer_information)

####1 potential visualization 
ggplot(data=customer_information,aes(x=revenues))+geom_histogram(binwidth = 800,fill='#DDE3ED',color='black')+theme_light()

customer_information$revenues <- sort(customer_information$revenues,decreasing = TRUE)
customer_information$cumulative_revenue <- cumsum(customer_information$revenues/sum(customer_information$revenues))

write.csv(customer_information,'cust_infooooo.csv')
####1 potential visualization
ymax<-1000000
customer_information$cust_id<-as.character(customer_information$cust_id)
ggplot(data=customer_information) + 
  geom_bar(aes(x=cust_id,y=revenues),stat = 'identity')+
  scale_x_discrete(name = 'Revenues')+
  geom_line(aes(x=cust_id,y=cumulative_revenue*ymax), col="red", lwd=1)+
  scale_y_continuous(name = 'Number of customers', sec.axis = sec_axis(~./ymax, 
                                                                       name = "Cumulative percentage of revenues [%]"))+theme_light()
customer_information1<-customer_information[1:100,]

store_information$revenues <- sort(store_information$revenues,decreasing = TRUE)
store_information$cumulative_revenue <- cumsum(store_information$revenues/sum(store_information$revenues))


product_information$revenues <- sort(product_information$revenues,decreasing = TRUE)
product_information$cumulative_revenue <- cumsum(product_information$revenues/sum(product_information$revenues))
product_information$ID <- seq.int(nrow(product_information))

ggplot(data=product_information, aes(x=ID)) + 
  geom_bar(aes(y=revenues), stat="identity",width = 1) +
  geom_line(aes(y=cumulative_revenue*100000,group=1,color='red'))+
  xlab("Product") + ylab("Revenues")+coord_cartesian(ylim = c(0,100000))+theme_light()+theme(axis.title.x=element_blank(),
                                                                              axis.text.x=element_blank(),axis.ticks.x=element_blank())

#+theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5)) 

head(customer_information)
#+geom_point(aes(y=cumulative_revenue*10000))
#+scale_x_discrete(limits=data[,1])
ggplot(data = customer_information,aes(x=cust_id,y=revenues))+geom_bar(stat='identity')+geom_line(aes(y=cumulative_revenue,group=1))

#7920 customers in total
#Top1
customer_information[customer_information$revenues==max(customer_information$revenues),]
#Top 25%
customer_information<-customer_information[order(revenues,decreasing = TRUE),]
customer_information[1:(nrow(customer_information)*0.25),]  #ie. 9138.17 as threshold

#Top1
customer_information[customer_information$products==max(customer_information$products),]
#Top 25%
customer_information<-customer_information[order(products,decreasing = TRUE),]
customer_information[1:(nrow(customer_information)*0.25),]  #ie. 1147 as threshold

#Top1
customer_information[customer_information$transactions==max(customer_information$transactions),]
#Top 25%
customer_information<-customer_information[order(transactions,decreasing = TRUE),]
customer_information[1:(nrow(customer_information)*0.25),]  #ie. 389 as threshold

# B -----------------------------------------------------------------------


#What are the products and product groups with the best volumes, revenues, profits, transactions, customers, etc.?
product_information<-data %>% group_by(prod_id) %>% summarise(revenues = sum(tran_prod_paid_amt),volumes=sum(tran_prod_sale_qty), transactions = 
                                                                 n_distinct(tran_id),customers = n_distinct(cust_id))
product_information<-as.data.table(product_information)
#10767 products in total
#Top1
product_information[product_information$revenues==max(product_information$revenues),]
#Top 25%
product_information<-product_information[order(revenues,decreasing = TRUE),]
sum(product_information[1:(nrow(product_information)*0.1),'revenues'])/sum(product_information$revenues)  #ie. 4567.11 as threshold


category_information<-data %>% group_by(category_id) %>% summarise(revenues = sum(tran_prod_paid_amt),volumes=sum(tran_prod_sale_qty), transactions = 
                                                                n_distinct(tran_id),customers = n_distinct(cust_id))
category_information<-as.data.table(category_information)
#429 categories in total
#Top1
category_information[category_information$revenues==max(category_information$revenues),]
#Top 25%
category_information<-category_information[order(revenues,decreasing = TRUE),]
category_information[1:(nrow(category_information)*0.25),]  #ie. 153390.6 as threshold

# C -----------------------------------------------------------------------


#   Which stores rank the highest in volumes, revenues, profits, transactions, customers, etc.?
store_information<-data %>% group_by(store_id) %>% summarise(revenues = sum(tran_prod_paid_amt), transactions = 
                                                                n_distinct(tran_id),customers = n_distinct(cust_id),
                                                             products=n_distinct(prod_id),categories=n_distinct(category_id))
store_information<-as.data.table(store_information)
#421 stores in total
#Top1
store_information[store_information$revenues==max(store_information$revenues),]
#Top 25%
store_information<-store_information[order(transactions,decreasing = TRUE),]
sum(store_information[1:(nrow(store_information)*0.2),'transactions'])/sum(store_information$transactions)  #ie. 182016.7 as threshold


# D -----------------------------------------------------------------------
#unique(product$category_desc_eng)

#   Are there interesting groupings of customers, e.g., most valuable (buy everything at any price) or cherry-pickers (buy mostly on promotions), defined by certain categories (buy baby products or never buy milk), etc.?
list_of_categories<-unique(data$category_id)
list_of_customers<-unique(data$cust_id)

cust_segment1<-as.data.table(list_of_customers)
for (i in 1:length(list_of_categories)){
  cust_segment1[,as.character(list_of_categories[i])]<-sapply(list_of_customers, function(x) ifelse(nrow(data[cust_id==x&category_id==list_of_categories[i],'tran_prod_paid_amt'])==0,
                                                                                                    0,sum(data[cust_id==x&category_id==list_of_categories[i],'tran_prod_paid_amt'])))
}

#write.csv(cust_segment1,'cust_segment2.csv')
#cust_segment1 <- fread('cust_segment1.csv')
#cust_segment1<-cust_segment1[,-1]


#library(sBIC)
#library(poLCA)

#data[store_id==102,]

cust_segment1<-fread('C:/Users/Godfr/Documents/cust_segment2.csv')

cust_segment1<-as.data.frame(cust_segment1)
rownames(cust_segment1)<-list_of_customers
cust_segment1<-cust_segment1[,-1]
cust_segment1<-cust_segment1[,-1]

cust_segment1[,c(1:429)] <- lapply(cust_segment1[,c(1:429)], function(x) c(scale(x)) )
cust_seg_matrix<-as.matrix(cust_segment1)

#names<-paste(list_of_categories,collapse = ', ')
#f = cbind() ~ 1
#model_x<-poLCA(cbind(names)~1,data=cust_segment1, nclass = 3, verbose=FALSE)

# res2 = poLCA(cbind(homeless=homeless+1, 
#                    cesdcut=cesdcut+1, satreat=satreat+1, 
#                    linkstatus=linkstatus+1) ~ 1, 
#              maxiter=50000, nclass=3, 
#              nrep=10, data=ds)

#library(vegan)

#cal_fit <- cascadeKM(cust_seg_matrix, 1, 10, iter = 50)
#cal_fit
#plot(cal_fit,sortg = TRUE,grpmts.plot = TRUE)
#prod_clust1<-kmeans(product_matrix, centers=10, nstart = 100)


d <- dist(cust_seg_matrix)
c <- hclust(d, method = 'ward.D2')

plot(c)
plot(c, cex = 0.6)
rect.hclust(c, k = 6,border=2:7)

library(factoextra)
fviz_cluster(list(data = cust_seg_matrix, cluster = members))

members <- cutree(c,k = 6)

temp<-aggregate(cust_segment1, by=list(members), mean)
#head(data_2)
data_2<-data
customer_seg<-as.data.table(list_of_customers)
customer_seg$cust_seg<-members
data_2<-left_join(data,customer_seg,by = c("cust_id" = "list_of_customers"))
#data_2$cust_seg<-as.character(data_2$cust_seg)
customer_seg_information<-data_2 %>% group_by(cust_seg) %>% summarise(revenues = mean(tran_prod_paid_amt),volumes=mean(tran_prod_sale_qty), transactions = 
                                                                        n_distinct(tran_id),customers = n_distinct(prod_id),unit_price=mean(prod_unit_price),
                                                                      discount_ratio=sum(tran_prod_discount_amt!=0)/n(),discount_amt_ratio = abs(sum(tran_prod_discount_amt))/sum(tran_prod_sale_amt),
                                                                      unique = n_distinct(cust_id))
write.csv(product_seg1,'product_groups.csv')
#write.csv(customer_seg_information,'customer_groups_info.csv')
######one question######
#length(unique(data$tran_id))

groups<-as.data.frame( t(sapply(temp, function(x) list(means=mean(x,na.rm=TRUE), 
                                                       sds=sd(x,na.rm=TRUE))) ))
groups<-groups[-1,]
groups$means<-as.numeric(groups$means)
groups$sds<-as.numeric(groups$sds)
groups<-as.data.table(groups)
groups[,variation:=sds/means]
#groups[means>=4&variation>=0.1,]


cols <- colnames(temp[,2:430]) # get names of categorical variables
temp_2<-sapply(temp, function(x) mean(x))
temp_3<-temp
for(i in 1:429){
  temp_3[,i+1]<-(temp_3[,i+1]-groups$means[i])/groups$sds[i]
}

sample<-t(temp_3[2,])
sample<-sample[-1,]
sample<-as.data.table(sample)
sample$names<-cols
sample<-sample[sample>1.8,]
#sample<-sample[sample>2.2,]
unique(product[category_id%in%sample$names,'category_desc_eng'])

transaction[cust_id==1107,]
# E -----------------------------------------------------------------------

#   Other than product categories and sub-categories, are there other product groupings, e.g., Key Value Items (KVI) and Key Value Categories (KVC), traffic drivers, always promoted versus seldom/never promoted, etc.?
product_seg<-data %>% group_by(prod_id) %>% summarise(revenues = mean(tran_prod_paid_amt),volumes=mean(tran_prod_sale_qty), transactions = 
                                                                n_distinct(tran_id),customers = n_distinct(cust_id),unit_price=mean(prod_unit_price),
                                                              discount_ratio=sum(tran_prod_discount_amt!=0)/n(),discount_amt_ratio = abs(sum(tran_prod_discount_amt))/sum(tran_prod_sale_amt))
product_seg<-as.data.frame(product_seg)

rownames(product_seg) <- product_seg$prod_id

product_seg[,c(2:8)] <- lapply(product_seg[,c(2:8)], function(x) c(scale(x)) )
product_seg<-product_seg[,-1]
product_matrix<-as.matrix(product_seg)


#write.csv(customer_seg,'cust_seg.csv')
#write.csv(product_segment1,'prod_seg.csv')

product_segment1<-fread('prod_seg.csv')
product_segment1<-product_segment1[,-1]
# d <- dist(as.matrix(product_seg))
# c <- hclust(d, method = 'ward.D2')
# plot(c)
# members <- cutree(c,k = 4)
# 
library(vegan)

cal_fit <- cascadeKM(product_matrix, 1, 10, iter = 200)
cal_fit
plot(cal_fit,sortg = TRUE,grpmts.plot = TRUE)
prod_clust1<-kmeans(product_matrix, centers=10, nstart = 100)

# library(mclust)
# 
# prod_clust <- Mclust(product_matrix, G=1:20)
# plot(prod_clust)
# m.best <- dim(prod_clust$z)[2]
sum(transaction[prod_id%in%t(product[category_desc_eng=='BAGS','prod_id']),'tran_prod_paid_amt'])/length(unique(transaction$cust_id))

sum(transaction$prod_id%in%t(product[category_desc_eng=='BAGS','prod_id']))

product_segment1<-as.numeric(rownames(product_matrix))
product_segment1<-as.data.table(product_segment1)
product_segment1$group<-as.numeric(prod_clust1$cluster)

data_3<-left_join(data,product_segment1,by = c("prod_id" = "product_segment1"))

product_seg1<-data_3 %>% group_by(group) %>% summarise(revenues = mean(tran_prod_paid_amt),volumes=mean(tran_prod_sale_qty), transactions = 
                                                        n_distinct(tran_id),customers = n_distinct(cust_id),unit_price=mean(prod_unit_price),
                                                      discount_ratio=sum(tran_prod_discount_amt!=0)/n(),discount_amt_ratio = abs(sum(tran_prod_discount_amt))/sum(tran_prod_sale_amt),unique=n_distinct(category_id))
unique(product[prod_id%in%t(product_segment1[group==1,'product_segment1']),'category_desc_eng'])
unique(product[prod_id%in%t(product_segment1[group==5,'product_segment1']),'sub_category_desc'])
unique(transaction[prod_id%in%t(product_segment1[group==10,'product_segment1']),'prod_unit'])

mean(transaction[prod_id==999176228,'tran_prod_paid_amt'])
#write.csv(product_seg1,'product_seg_1.csv')
product_seg1<-fread('product_seg_1.csv')
  
sum(data$tran_prod_paid_amt)
haha<-data_3[group==1,]%>% group_by(subcategory_id)%>%summarise(percent = sum(tran_prod_paid_amt)/62209854)


list_p<-unique(product[prod_id%in%t(product_segment1[group==1,'product_segment1']),'subcategory_id'])
list_p2<-left_join(list_p,haha,by = c("subcategory_id" = "subcategory_id"))
list_p2<-as.data.table(list_p2)
list_p2<-list_p2[order(percent,decreasing = TRUE),]
unique(product[subcategory_id%in%list_p2$subcategory_id[1:20],'category_desc_eng'])



pca_res <- prcomp(as.matrix(product_seg), center = TRUE, scale. = TRUE)
product_segment1$group<-as.factor(product_segment1$group)
plot_data <- cbind(as.data.frame(pca_res$x[, 1:2]), labels = product_segment1$group)

ggplot(plot_data, aes(x = PC1, y = PC2, color = labels)) +scale_color_manual(values=c('#ffc30f','#900c3f','#c70039','#ff5733','#581845','#2b4141','#C8E087','#21897E','#6dbc95','#9DC5BB'))+
  geom_point(alpha=0.7)+xlim(-6,8)+ylim(-8,6)+ theme_light()

names(wes_palettes)

head(data_4)
# SS ----------------------------------------------------------------------

data_4<-left_join(data_2,product_segment1,by = c("prod_id" = "product_segment1"))
head(data_4)
memory.limit(size=10000000)
#extra<-data_4 %>% group_by(cust_seg) %>% summarise(group1 = nrow())
data_4<-as.data.table(data_4)
num<-paste('customer',c(1:6))
extra<-as.data.table(num)
customer_seg_information<-as.data.table(customer_seg_information)
product_seg1<-as.data.table(product_seg1)

customer_seg_information<-as.data.table(customer_seg_information)
product_seg1<-as.data.table(product_seg1)
for (i in 1:10){
  extra[,paste('product',i,collapse = '-')]<-sapply(1:6, function(x) ifelse(nrow(data_4[cust_seg==x&group==i,'tran_prod_paid_amt'])==0,
                                                                            0,sum(data_4[cust_seg==x&group==i,'tran_prod_paid_amt'])))
}

extra<-as.data.frame(extra)
rownames(extra)<-extra$num
extra<-extra[,-1]



num_p<-product_seg1$unique
nam<-rownames(extra)


extra<-extra/rowSums(extra)

test<-fread('C:/Users/Godfr/Documents/US/Marketing/P1/Result.csv')
test[sub_grp==4,'category_desc_eng']
# F -----------------------------------------------------------------------

#   Are there natural groupings of stores, e.g., stores frequented by cherry-pickers versus stores visited by most loyal customers?

store_information<-data %>% group_by(store_id) %>% summarise(revenues = sum(tran_prod_paid_amt), transactions = 
                                                               n_distinct(tran_id),customers = n_distinct(cust_id),
                                                             products=n_distinct(prod_id),categories=n_distinct(category_id))
length(unique(data_3$tran_id))

##combine groups and store
list_of_stores<-unique(data$store_id)
data_2<-as.data.table(data_2)
stores_table<-as.data.table(list_of_stores)
memory.limit(size=100000)
for (i in 1:6){
  stores_table[,paste('customer',i,collapse = '-')]<-sapply(list_of_stores, function(x) ifelse(nrow(data_2[store_id==x&cust_seg==i,'tran_prod_paid_amt'])==0,
                                                                                                    0,sum(data_2[store_id==x&cust_seg==i,'tran_prod_paid_amt'])))
}
stores_table[,c(2:7)] <- lapply(stores_table[,c(2:7)], function(x) c(scale(x)) )
data_3<-as.data.table(data_3)
for (i in 1:10){
  stores_table[,paste('product',i,collapse = '-')]<-sapply(list_of_stores, function(x) ifelse(nrow(data_3[store_id==x&group==i,'tran_prod_paid_amt'])==0,
                                                                                               0,sum(data_3[store_id==x&group==i,'tran_prod_paid_amt'])))
}
stores_table[,c(8:17)] <- lapply(stores_table[,c(8:17)], function(x) c(scale(x)) )
write.csv(stores_table,'stores_table.csv')
