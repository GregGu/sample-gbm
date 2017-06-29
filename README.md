
---
title: "Untitled"
author: "greg"
date: "6/23/2017"
output: html_document
---

```{r, eval=F}
library(fpc)


#read in data
data<-read.csv('sample_notes.csv')
data2<-read.csv('state.csv')


#lets try to find correlation... maybe too many variables for this... segment data
class(data_clean$small_hatchback)

result = matrix(nrow = 30, ncol = 2)
count <- 0;
threshold<-0.75

for(i in 2:ncol(data.frame)){
  for(j in 2:ncol(data.frame)){
    if((cor(data_clean[ , i], data.frame[ j])) > threshold){
      result[count, 1] <- names(data.frame)[i]
      result[count, 2] <- names(data.frame)[j]
      count <- count + 1
    }
  }
}

result <- result[which(result[ , 1] != result[ ,2]) , ]
result





#reduce dimensions
data_clean<-data[,-c(1,2,4,5,6,19,21,22,23,24,26,31,32)]

#create disimilarity matrix
z<-data_clean
m<-apply(z,2,mean)
s<-apply(z,2,sd)
z<-scale(z,m,s)
distance <- dist(z)
print(distance, digits=3)

#cluster
hc_c<-hclust(distance)
plot(hc_c)
rect.hclust(hc_c,3)

plot(hc_c, labels=data2$State)

#cluster dendogram with average linkage
hc_a<-hclust(distance, method='average')
plot(hc_a)
rect.hclust(hc_a,5)



#now lets prune the tree of 5,10,44 and re cluster
data_clean2<-data_clean[-c(5,10,44),]


z<-data_clean2
m<-apply(z,2,mean)
s<-apply(z,2,sd)
z<-scale(z,m,s)
distance <- dist(z)
print(distance, digits=3)

#cluster
hc_c<-hclust(distance)
plot(hc_c)
rect.hclust(hc_c,5)


#cluster dendogram with average linkage
hc_a<-hclust(distance, method='average')
plot(hc_a)
rect.hclust(hc_a,5)
#not as good...

hc_a<-hclust(distance, method='single')
plot(hc_a)
rect.hclust(hc_a,5)


#cluster membership
member_c<-cutree(hc_c,4)
member_a<-cutree(hc_a,4)
table(member_c,member_a)

#cluster means
aggregate(z, list(member_c),mean)


data_scaled<-scale(data_clean)
wssplot <- function(data, nc=15, seed=1234){
               wss <- (nrow(data)-1)*sum(apply(data,2,var))
               for (i in 2:nc){
                    set.seed(seed)
                    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
                plot(1:nc, wss, type="b", xlab="Number of Clusters",
                     ylab="Within groups sum of squares")
           }
wssplot(data_scaled)




km<-kmeans(data_scaled,3)
table(data2$State, km$cluster)
km

library(cluster)
fit.pam <- pam(distance, 3)
plot

clusplot(fit.pam)
```
