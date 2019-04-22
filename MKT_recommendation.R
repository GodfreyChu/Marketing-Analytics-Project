# Initializing ------------------------------------------------------------
#import necessary packages
library(data.table)
library(dplyr)
library(ggplot2)
library(igraph)
library(recommenderlab)
library(tidyverse)
#read data from local drive
product<-fread('C:/Users/Godfr/Documents/US/Marketing/product_table.csv')
transaction<-fread('C:/Users/Godfr/Documents/US/Marketing/transaction_table.csv')

#find the products of dove
dove<-product[brand_desc=='DOVE',]
#find the categories dove has products in 
list_of_category<-unique(dove$category_id)
category<-product[category_id%in%list_of_category,]

#transaction data for 6 categories(including products of other brands) of dove
for (i in 1:6){
  nam <- paste('cat_',list_of_category[i],sep='')
  assign(nam,product[category_id==list_of_category[i],])
} 

for (i in 1:6){
  nam <- paste('trans_',list_of_category[i],sep='')
  assign(nam,transaction[prod_id%in%unique(get(paste('cat_',list_of_category[i],sep=''))$prod_id),c('cust_id','prod_id','tran_prod_sale_qty')])
} 

#transaction data for dove products
dove_products<-unique(dove$prod_id)
trans_dove<-transaction[prod_id%in%dove_products,c('cust_id','prod_id','tran_prod_sale_qty')]

#all_category transaction network
transaction_withcat<-merge(transaction,product[,c('prod_id','category_id')],by='prod_id',all.x=TRUE)
trans_cate<-transaction_withcat %>% group_by(cust_id,category_id) %>% summarise(sales_amt=sum(tran_prod_paid_amt)/n())
trans_cate<-as.data.table(trans_cate)

#DOVE brand transaction network
trans_dove<-trans_dove %>% group_by(cust_id,prod_id) %>% summarise(sales_qty=sum(tran_prod_sale_qty))
trans_dove<-as.data.table(trans_dove)

#unique(transaction[prod_id%in%dove_products,'prod_unit'])
#find out that all products are in the same unit

dove_graph <- graph.data.frame(trans_dove[,1:2], directed=FALSE) # make general undirected graph
V(dove_graph)$type <- V(dove_graph)$name %in% unique(trans_dove$prod_id) # specify type to make bipartite
E(dove_graph)$weight <- trans_dove$sales_qty # add in rating as weight


# visualization -----------------------------------------------------------
#Visualization of the network
dove.subplot <- induced_subgraph(dove_graph,v=sample(unlist(V(dove_graph)$name),200))

shape <- c("circle", "square")
col <- c("#98C7FF","#61A0EC")

#plot of subplot of network
plot(dove.subplot,vertex.color = adjustcolor(col[as.numeric(V(dove.subplot)$type)+1],alpha.f=0.8),
     vertex.shape = shape[as.numeric(V(dove_graph)$type)+1], layout=layout_as_bipartite(dove.subplot, hgap=30),
     vertex.label="", 
     vertex.frame.color=adjustcolor("#CBE2FE",alpha.f = 0.4),
     edge.color = "#CCD3DC",
     vertex.size=5)

#plot of full network
plot(dove_graph,vertex.color = adjustcolor(col[as.numeric(V(dove_graph)$type)+1],alpha.f=0.8),
     vertex.shape = shape[as.numeric(V(dove_graph)$type)+1], layout=layout_as_bipartite(dove_graph, hgap=30),
     vertex.label="", 
     vertex.frame.color=adjustcolor("#CBE2FE",alpha.f = 0.4),
     edge.color = "#CCD3DC",
     vertex.size=5)


# recommendation model----------------------------------------------------------

##Collaborative filtering within dove's products

#transform the dataframe into binary (0-haven't bought; 1-have bought)
trans_dove_simp<-trans_dove
trans_dove_simp[,'sales_qty']<-1
dove_graph_simp <- graph.data.frame(trans_dove_simp[,1:2], directed=FALSE)
#building biparatte social network model
V(dove_graph_simp)$type <- V(dove_graph_simp)$name %in% unique(trans_dove_simp$prod_id)

matrix_dove <- as_incidence_matrix(dove_graph_simp, sparse=TRUE)
matrix_dove<-as(matrix_dove,'matrix')
matrix_dove = as(matrix_dove , 'binaryRatingMatrix')

#build a model based on item-based collaborative filtering 
model_01 = Recommender(matrix_dove, method = "IBCF")
#build a model based on singular vector decomposition **note: SVD not support binary matrix
#model_01 = Recommender(matrix_dove, method = "SVD")

#test.cust<-sample(trans_dove_simp$cust_id, 1)

# test application --------------------------------------------------------

#sample a customer to test the application
rec.test.item = predict(model_01, matrix_dove[1,], n=10)
rec.compare <- as.numeric(unlist(as(rec.test.item, "list")))

#items he bought
rated.high <- trans_dove[cust_id==rownames(matrix_dove[1,]) ,]
high <- product[prod_id %in% rated.high$prod_id,]

#items recommended
recommend <- product[prod_id %in% rec.compare, ]

#see the products
high
recommend

#subset the customers to those who have bought more than 5 things (so that can learn more from training set)
dove_agg<-trans_dove_simp%>%group_by(cust_id)%>%summarise(num=n())
dove_agg<-dove_agg[dove_agg$num>5,'cust_id']

trans_dove_simp_n<-trans_dove_simp[cust_id%in%dove_agg$cust_id,]
dove_graph_simp_n <- graph.data.frame(trans_dove_simp_n[,1:2], directed=FALSE)

#building biparatte social network model
V(dove_graph_simp_n)$type <- V(dove_graph_simp_n)$name %in% unique(trans_dove_simp_n$prod_id)

matrix_dove_n <- as_incidence_matrix(dove_graph_simp_n, sparse=TRUE)
matrix_dove_n<-as(matrix_dove_n,'matrix')
matrix_dove_n = as(matrix_dove_n , 'binaryRatingMatrix')

#evaluate the accuracy of recommender model
eval = evaluationScheme(matrix_dove, method="split", train=0.75, goodRating = 1, given=1)
results1 <- evaluate(eval, 'IBCF', type = "topNList", n=c(1,2,3,4,5))

# ROC Curve
plot(main="ROC Curve for 1-5 recommended items", results1,
     legend="topleft", col=c("#231F20"),
     cex=0.8, lwd=1.2, annotate=c(5),xlim=c(0,0.4),ylim=c(0,0.4))

#precision-recall Curve
plot(y="prec/rec", main="Precision-recall for 1-5 recommended items", results1,
     legend="bottomright", col=c("#231F20"), 
     cex=0.8, lwd=1.2, annotate=c(5),xlim=c(0,0.4),ylim=c(0,0.4))

# calculate similarity ----------------------------------------------------

#build jaccard similarity matrix between products
#similarity(matrix_dove,method = 'jaccard',which='prod_id')
similarity_matrix<-model_01@model$sim
similarity_matrix<-as(similarity_matrix,'matrix')
similarity_matrix<-as.data.table(similarity_matrix)
#rownames(similarity_matrix)<-colnames(similarity_matrix)
sim_list<-colnames(similarity_matrix)

#find the best selling products in dove
dove_prod_sum<-transaction[prod_id%in%dove_products,]%>%group_by(prod_id)%>%summarise(tran_qty=sum(tran_prod_sale_qty))
dove_prod_sum<-as.data.table(dove_prod_sum)
dove_prod_sum<-dove_prod_sum[order(-tran_qty),]

dove_prod_sum$prod_id <- factor(dove_prod_sum$prod_id) %>% fct_reorder(-dove_prod_sum$tran_qty)
ggplot(data=dove_prod_sum)+geom_col(aes(x=prod_id,y=tran_qty),fill='#61A0EC')+theme_light()

#find the category names of Dove
unique(product[category_id%in%list_of_category,'category_desc_eng'])

#for each of the most popular products, find other products of highest jaccard similarity
product[prod_id%in%c('999527671','999399982','999233749'),]
similarity_matrix[similarity_matrix$`999527671`>=0.1,'999527671']
product[prod_id%in%sim_list[similarity_matrix$`999399982`>=0.07],]

#see the promotion details of a product 
transaction[prod_id=='999527671',]%>%group_by(prod_id)%>%summarise(avg_offer=sum(tran_prod_offer_cts)/n(),avg_ratio=mean(-tran_prod_discount_amt/tran_prod_sale_amt))

# Same process for other categories 1---------------------------------------

#transform the dataframe into binary (0-haven't bought; 1-have bought)
trans_95704_simp<-trans_95704
trans_95704_simp[,'tran_prod_sale_qty']<-1
trans_95704_simp<-trans_95704_simp[!duplicated(trans_95704_simp),]
c95704_graph_simp <- graph.data.frame(trans_95704_simp[,1:2], directed=FALSE)
#building biparatte social network model
V(c95704_graph_simp)$type <- V(c95704_graph_simp)$name %in% unique(trans_95704_simp$prod_id)

matrix_95704 <- as_incidence_matrix(c95704_graph_simp, sparse=TRUE)
matrix_95704 <-as(matrix_95704,'matrix')
matrix_95704 = as(matrix_95704,'binaryRatingMatrix')

#build a model based on item-based collaborative filtering 
model_02 = Recommender(matrix_95704, method = "IBCF")

#build jaccard similarity matrix between products
#similarity(matrix_dove,method = 'jaccard',which='prod_id')
similarity_matrix2<-model_02@model$sim
similarity_matrix2<-as(similarity_matrix2,'matrix')
similarity_matrix2<-as.data.table(similarity_matrix2)
#rownames(similarity_matrix)<-colnames(similarity_matrix)
sim_list2<-colnames(similarity_matrix2)

#find the product ids of Dove
unique(cat_95704[category_id=='95704'&brand_desc=='DOVE',])


# category2_shower gel ---------------------------------------------------------------

#transform the dataframe into binary (0-haven't bought; 1-have bought)
trans_95746_simp<-trans_95746
trans_95746_simp[,'tran_prod_sale_qty']<-1
trans_95746_simp<-trans_95746_simp[!duplicated(trans_95746_simp),]
c95746_graph_simp <- graph.data.frame(trans_95746_simp[,1:2], directed=FALSE)
#building biparatte social network model
V(c95746_graph_simp)$type <- V(c95746_graph_simp)$name %in% unique(trans_95746_simp$prod_id)

matrix_95746 <- as_incidence_matrix(c95746_graph_simp, sparse=TRUE)
matrix_95746 <-as(matrix_95746,'matrix')
matrix_95746 = as(matrix_95746,'binaryRatingMatrix')

#build a model based on item-based collaborative filtering 
model_03 = Recommender(matrix_95746, method = "IBCF")

#build jaccard similarity matrix between products
#similarity(matrix_dove,method = 'jaccard',which='prod_id')
similarity_matrix3<-model_03@model$sim
similarity_matrix3<-as(similarity_matrix3,'matrix')
similarity_matrix3<-as.data.table(similarity_matrix3)
#rownames(similarity_matrix)<-colnames(similarity_matrix)
sim_list3<-colnames(similarity_matrix3)

#find the best selling products in this category
c95746_prod_sum<-transaction[prod_id%in%sim_list3,]%>%group_by(prod_id)%>%summarise(tran_qty=sum(tran_prod_sale_qty))
c95746_prod_sum<-as.data.table(c95746_prod_sum)
c95746_prod_sum<-c95746_prod_sum[order(-tran_qty),]
#c95746_prod_sum$prod_id<-as.character(c95746_prod_sum$prod_id)
c95746_prod_sum<-merge(c95746_prod_sum,product[,c('prod_id','brand_desc')],by='prod_id',all.x=TRUE)
c95746_prod_sum<-c95746_prod_sum[brand_desc!='PRIVATE LABEL',]

#visualization
c95746_prod_sum$prod_id <- factor(c95746_prod_sum$prod_id) %>% fct_reorder(-c95746_prod_sum$tran_qty)
ggplot(data=c95746_prod_sum)+geom_col(aes(x=prod_id,y=tran_qty),fill='#61A0EC')+theme_light()
#closer look at the products
rownames(similarity_matrix3)<-sim_list3
similarity_matrix3[which(sim_list3=='999426139'),'999527671']

eval = evaluationScheme(matrix_95746, method="split", train=0.75, goodRating = 1, given=1)
results1 <- evaluate(eval, 'IBCF', type = "topNList", n=c(1,2,3,4,5))

# ROC Curve
plot(main="ROC Curve for 1-5 recommended items", results1,
     legend="topleft", col=c("#231F20"),
     cex=0.8, lwd=1.2, annotate=c(5),xlim=c(0,0.3),ylim=c(0,0.3))
#precision-recall Curve
plot(y="prec/rec", main="Precision-recall for 1-5 recommended items", results1,
     legend="bottomright", col=c("#231F20"), 
     cex=0.8, lwd=1.2, annotate=c(5),xlim=c(0,0.4),ylim=c(0,0.4))


# category3_deodorizer ---------------------------------------------------------------

#transform the dataframe into binary (0-haven't bought; 1-have bought)
trans_95747_simp<-trans_95747
trans_95747_simp[,'tran_prod_sale_qty']<-1
trans_95747_simp<-trans_95747_simp[!duplicated(trans_95747_simp),]
c95747_graph_simp <- graph.data.frame(trans_95747_simp[,1:2], directed=FALSE)
#building biparatte social network model
V(c95747_graph_simp)$type <- V(c95747_graph_simp)$name %in% unique(trans_95747_simp$prod_id)

matrix_95747 <- as_incidence_matrix(c95747_graph_simp, sparse=TRUE)
matrix_95747 <-as(matrix_95747,'matrix')
matrix_95747 = as(matrix_95747,'binaryRatingMatrix')

#build a model based on item-based collaborative filtering 
model_04 = Recommender(matrix_95747, method = "IBCF")

#build jaccard similarity matrix between products
#similarity(matrix_dove,method = 'jaccard',which='prod_id')
similarity_matrix4<-model_04@model$sim
similarity_matrix4<-as(similarity_matrix4,'matrix')
similarity_matrix4<-as.data.table(similarity_matrix4)
#rownames(similarity_matrix)<-colnames(similarity_matrix)
sim_list4<-colnames(similarity_matrix4)

#find the best selling products in this category
c95747_prod_sum<-transaction[prod_id%in%sim_list4,]%>%group_by(prod_id)%>%summarise(tran_qty=sum(tran_prod_sale_qty))
c95747_prod_sum<-as.data.table(c95747_prod_sum)
c95747_prod_sum<-c95747_prod_sum[order(-tran_qty),]
#c95746_prod_sum$prod_id<-as.character(c95746_prod_sum$prod_id)
c95747_prod_sum<-merge(c95747_prod_sum,product[,c('prod_id','brand_desc')],by='prod_id',all.x=TRUE)
c95747_prod_sum<-c95747_prod_sum[brand_desc!='PRIVATE LABEL',]

c95747_prod_sum$prod_id <- factor(c95747_prod_sum$prod_id) %>% fct_reorder(-c95747_prod_sum$tran_qty)
ggplot(data=c95747_prod_sum)+geom_col(aes(x=prod_id,y=tran_qty),fill='#61A0EC')+theme_light()

rownames(similarity_matrix4)<-sim_list4
#closer look at products similarity
similarity_matrix4[which(sim_list4=='999177474'),'999152445']

eval = evaluationScheme(matrix_95747, method="split", train=0.75, goodRating = 1, given=1)
results1 <- evaluate(eval, 'IBCF', type = "topNList", n=c(1,2,3,4,5))

# ROC Curve
plot(main="ROC Curve for 1-5 recommended items", results1,
     legend="topleft", col=c("#231F20"),
     cex=0.8, lwd=1.2, annotate=c(5),xlim=c(0,0.2),ylim=c(0,0.2))
#precision-recall Curve
plot(y="prec/rec", main="Precision-recall for 1-5 recommended items", results1,
     legend="bottomright", col=c("#231F20"), 
     cex=0.8, lwd=1.2, annotate=c(5),xlim=c(0,0.3),ylim=c(0,0.3))


# All categories ----------------------------------------------------------
cate_graph <- graph.data.frame(trans_cate[,1:2], directed=FALSE) # make general undirected graph
V(cate_graph)$type <- V(cate_graph)$name %in% unique(trans_cate$category_id) # specify type to make bipartite
E(cate_graph)$weight <- trans_cate$sales_amt # add in rating as weight

#transform the dataframe into binary (0-haven't bought; 1-have bought)
trans_cate_simp<-trans_cate
trans_cate_simp[,'sales_qty']<-1
cate_graph_simp <- graph.data.frame(trans_cate_simp[,1:2], directed=FALSE)
V(cate_graph_simp)$type <- V(cate_graph_simp)$name %in% unique(trans_cate_simp$category_id)

#change into binary matrix
matrix_cate <- as_incidence_matrix(cate_graph_simp, sparse=TRUE)
matrix_cate <- as_incidence_matrix(cate_graph_simp, sparse=TRUE)
matrix_cate<-as(matrix_cate,'matrix')
matrix_cate = as(matrix_cate , 'binaryRatingMatrix')
#recommender model
model_02 = Recommender(matrix_cate, method = "IBCF")

similarity_matrix<-model_02@model$sim
similarity_matrix<-as(similarity_matrix,'matrix')
similarity_matrix<-as.data.table(similarity_matrix)
rownames(similarity_matrix)<-colnames(similarity_matrix) #does not work

sim_matrix<-as.data.frame(similarity_matrix)
sim_matrix["category_id"]<-colnames(sim_matrix)
#rownames(sim_matrix)<-colnames(sim_matrix)

#filter out the matrix for categories that Dove carries
dove_sim<- sim_matrix[,c("category_id","95746","95747","95732","95708","95704","95743")]

#sim_list<-colnames(similarity_matrix)
#rownames(similarity_matrix)<-sim_list

directory<- unique(product[,c("category_id","category_desc_eng")])

#construct the tables showing the most similar categories and the name ranked by similarity 
#SHOWER GEL
sim95746<-dove_sim[ ,c("category_id","95746")]
sim95746[sim95746==0] <- NA
sim95746<- na.omit(sim95746)
sim95746<-merge(sim95746,directory, by="category_id",all.x=TRUE)
sim95746<- sim95746[order(-sim95746$`95746`),]

#PERSONAL DEODORISERS
sim95747<-dove_sim[ ,c("category_id","95747")]
sim95747[sim95747==0] <- NA
sim95747<- na.omit(sim95747)
sim95747<-merge(sim95747,directory, by="category_id",all.x=TRUE)
sim95747<- sim95747[order(-sim95747$`95747`),]

#OUT. BABY HYGIENE ITEMS
sim95732<-dove_sim[ ,c("category_id","95732")]
sim95732[sim95732==0] <- NA
sim95732<- na.omit(sim95732)
sim95732<-merge(sim95732,directory, by="category_id",all.x=TRUE)
sim95732<- sim95732[order(-sim95732$`95732`),]

#BODY CREAMS
sim95708<-dove_sim[ ,c("category_id","95708")]
sim95708[sim95708==0] <- NA
sim95708<- na.omit(sim95708)
sim95708<-merge(sim95708,directory, by="category_id",all.x=TRUE)
sim95708<- sim95746[order(-sim95708$`95708`),]

#CHRISTMAS PERFUMERY |do not take into consideration
sim95704<-dove_sim[ ,c("category_id","95704")]
sim95704[sim95704==0] <- NA
sim95704<- na.omit(sim95704)
sim95704<-merge(sim95704,directory, by="category_id",all.x=TRUE)
sim95704<- sim95704[order(-sim95704$`95704`),]

#SOAPS
sim95743<-dove_sim[ ,c("category_id","95743")]
sim95743[sim95743==0] <- NA
sim95743<- na.omit(sim95743)
sim95743<-merge(sim95743,directory, by="category_id",all.x=TRUE)
sim95743<- sim95743[order(-sim95743$`95743`),]

# create evaluation scheme splitting taking 70% of the date for training
e <- evaluationScheme(matrix_cate, method="split", train=0.7, given=3)
eval <- evaluate(e, 'IBCF', type = "topNList", n=c(1,2,3,4,5))

# ROC Curve
#plot(main="ROC Curve for 1-5 recommended items", results1,
#     legend="topleft", col=c("#231F20"),
#     cex=0.8, lwd=1.2, annotate=c(5),xlim=c(0,0.1),ylim=c(0,0.1))

#precision-recall Curve
plot(y="prec/rec", main="Precision-recall for 1-5 recommended items", results1,
     legend="bottomright", col=c("#231F20"), 
     cex=0.8, lwd=1.2, annotate=c(5))
