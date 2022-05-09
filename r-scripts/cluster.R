palette <- c("#8250C4", "#db0a16", "#004753",  "#107C10")


#------------- ALL ATTRIBUTES -------------------
my_data = s1_sum_mean
fviz_nbclust(my_data, kmeans, method = "wss")

set.seed(123)
km.model <- kmeans(my_data, 4, nstart = 25)

fviz_cluster(km.model, data = my_data,
             ellipse.type = "convex",
             palette = palette,
             ggtheme = theme_minimal())

which(apply(s1_sum_mean, 2, var)==0)
var0sumMeanS1<-s1_sum_mean[ , which(apply(s1_sum_mean, 2, var) != 0)]
rownames(var0sumMeanS1)<-0:(nrow(var0sumMeanS1)-1)
fviz_nbclust(var0sumMeanS1, kmeans, method = "wss")

km.model <- kmeans(var0sumMeanS1, 4, nstart = 25)

fviz_cluster(km.model, data = var0sumMeanS1,
             ellipse.type = "convex",
             palette = palette,
             ggtheme = theme_minimal())


set.seed(001)
res <- kcca(var0sumMeanS1,k=4)
set.seed(001)
FeatureImp_res <- FeatureImpCluster(res,as.data.table(var0sumMeanS1))
plot(FeatureImp_res)

set.seed(12)
nr_seeds <- 20
seeds_vec <- sample(1:1000,nr_seeds)

savedImp <- data.frame(matrix(0,nr_seeds,dim(var0sumMeanS1)[2]))
count <- 1
for (s in seeds_vec) {
  set.seed(s)
  res <- kcca(var0sumMeanS1,k=4)
  set.seed(s)
  FeatureImp_res <- FeatureImpCluster(res,as.data.table(var0sumMeanS1),sub = 1,biter = 1)
  savedImp[count,] <- FeatureImp_res$featureImp[sort(names(FeatureImp_res$featureImp))]
  count <- count + 1
}
names(savedImp) <- sort(names(FeatureImp_res$featureImp))
boxplot(savedImp)


#-------------ATTEMPTING JUST S1 ATTRIBUTES --------------------------------
s1_sum <-s1_sum_mean[,9:length(s1_sum_mean)]
s1_sum_scale<-subset(s1_sum, select = -c(DRPM_1, DFR_1,DCL_1, CL_1)) %>% scale()
rownames(s1_sum_scale)<-0:(nrow(s1_sum_scale)-1)

fviz_nbclust(s1_sum_scale, kmeans, method = "wss")

set.seed(002)
res2 <- kcca(s1_sum_scale,k=4)
set.seed(002)
FeatureImp_res2 <- FeatureImpCluster(res2,as.data.table(s1_sum_scale))
plot(FeatureImp_res2)
feature_importance_S1<-FeatureImp_res2
plot(feature_importance_S1)

s1_clus_ <-s1_sum_mean[,9:length(s1_sum_mean)]
s1_clus<-subset(s1_clus_, select = -c(DRPM_1, DFR_1,DCL_1, CL_1, Z_1, RPM_1, FR_1)) %>% scale()
s1_clus<-data.frame(s1_clus)
rownames(s1_clus)<-0:(nrow(s1_clus)-1)

set.seed(00)
res <- kcca(s1_clus,k=4)
set.seed(00)
FeatureImp_res <- FeatureImpCluster(res,as.data.table(s1_clus))
plot(FeatureImp_res)
#feature_importance_S1_opt<-FeatureImp_res
plot(feature_importance_S1_opt)

#set.seed(42)
#model4 <- kmeans(s1_clus, 4, nstart = 25)
clus_model<-readRDS("kmeansModel.rds")
fviz_cluster(clus_model, data = s1_clus,
             ellipse.type = "convex",
             ggtheme = theme_minimal())
#saveRDS(model4, "kmeansModel.rds")


s1_clus %>%
  mutate(Cluster = clus_model$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

df<-setNames(data.frame(cbind(g_S4_curr$PartCount, as.numeric(clus_model$cluster))), c("PartCount", "cluster"))
df$cluster<-as.factor(df$cluster)
plot_ly(df, x=~PartCount, y=~cluster, color=~cluster, text=~PartCount, type='scatter', mode='lines+markers')
plot_ly(df, x=~PartCount, y=~cluster, type='scatter', mode='lines+markers')


# Comparing feature importance classification with the plots

plot_ly(summarised_S1_mean, x = ~PartCount, y = ~abs(S1Torque.mean),
        type = 'scatter',
        mode = 'lines+markers',
        name = "S1 Torque") %>%
  add_trace(y = ~abs(S1Curr.mean),
            type = 'scatter',
            mode = 'lines+markers',
            name = 'S1 Current')  %>%
  layout(showlegend = TRUE,
         yaxis = list(title = "Absolute Mean Values"),
         title = "Comparison of Mean Torque and Current values over Gundrill Life")


#-------------------trial on new data-------------------
data1<- read.csv("~/onedrive/Scenic/data/indexG220/ml/s1_sum_mean_1.csv", comment.char="#", stringsAsFactors=FALSE)
data2<- read.csv("~/onedrive/Scenic/data/indexG220/ml/s1_sum_mean_2.csv", comment.char="#", stringsAsFactors=FALSE)
data3<- read.csv("~/onedrive/Scenic/data/indexG220/ml/s1_sum_mean_3.csv", comment.char="#", stringsAsFactors=FALSE)
data4<- read.csv("~/onedrive/Scenic/data/indexG220/ml/s1_sum_mean_4.csv", comment.char="#", stringsAsFactors=FALSE)

head(data1)
head(data2)
head(data3)
head(data4)


PartCount1 <- 0:37
PartCount2 <- 0:35

# function to extract the clusters from applied model by calculating the Euclidean distance between the value and the model centres for each cluster; then returning the cluster number it's closest to.
closest.cluster <- function(x) {
  cluster.dist <- apply(clus_model$centers, 1, function(y) sqrt(sum((x-y)^2)))
  return(which.min(cluster.dist)[1])
}


#preparing data
d1 <-data1[,9:length(data1)]
df1 <-subset(d1, select = -c(DRPM_1, DFR_1,DCL_1, CL_1, Z_1, RPM_1, FR_1)) %>% scale()
rownames(df1)<-0:(nrow(df1)-1)
#compare with s1_clus (dataset model is based on)
head(df1)
head(s1_clus)
#apply model to new data and get clusters
new_clusters1 <- apply(df1, 1, closest.cluster)
new_clusters1
# validation (comparison of model application between s1_clus and df1 -> should be 0 since datasets are identical)
setdiff(matrix(clus_model$cluster), matrix(new_clusters1))
#prepping for plotting
clus_1<-setNames(data.frame(cbind(PartCount1, as.numeric(new_clusters1))), c("PartCount", "cluster"))
clus_1$cluster<-as.factor(clus_1$cluster)
head(clus_1)
plot_ly(clus_1, x=~PartCount, y=~cluster, color=~cluster, type='scatter', mode='lines+markers')
plot_ly(clus_1, x=~PartCount, y=~cluster, type='scatter', mode='lines+markers')
#visualising the clusters in a cluster plot
fviz_cluster(clus_model, data = df1,ellipse.type = "convex",ggtheme = theme_minimal())

d2 <-data2[,9:length(data2)]
df2 <-subset(d2, select = -c(DRPM_1, DFR_1,DCL_1, CL_1, Z_1, RPM_1, FR_1)) %>% scale()
rownames(df2)<-0:(nrow(df2)-1)
head(df2)
new_clusters2 <- apply(df2, 1, closest.cluster)
new_clusters2
clus_2<-setNames(data.frame(cbind(PartCount2, as.numeric(new_clusters2))), c("PartCount", "cluster"))
clus_2$cluster<-as.factor(clus_2$cluster)
tail(clus_2)
plot_ly(clus_2, x=~PartCount, y=~cluster, color=~cluster, type='scatter', mode='lines+markers')
plot_ly(clus_2, x=~PartCount, y=~cluster, type='scatter', mode='lines+markers')
#dataset is too short for original model, reshaping by 'making up' 2 new rows.
append1<-df2[1:2,]
append<-append1-0.02
rownames(append)<-c('100', '200') #rownames will be easy to spot and know they're the dud ones
df2_2 <-rbind(df2, append)
fviz_cluster(clus_model, data = df2_2, ellipse.type = "convex", ggtheme = theme_minimal())


d3 <-data3[,9:length(data3)]
df3 <-subset(d3, select = -c(DRPM_1, DFR_1,DCL_1, CL_1, Z_1, RPM_1, FR_1)) %>% scale()
rownames(df3)<-0:(nrow(df3)-1)
head(d3)
new_clusters3 <- apply(df3, 1, closest.cluster)
new_clusters3
clus_3<-setNames(data.frame(cbind(PartCount1, as.numeric(new_clusters3))), c("PartCount", "cluster"))
clus_3$cluster<-as.factor(clus_3$cluster)
head(clus_3)
plot_ly(clus_3, x=~PartCount, y=~cluster, color=~cluster, type='scatter', mode='lines+markers')
plot_ly(clus_3, x=~PartCount, y=~cluster, type='scatter', mode='lines+markers')
fviz_cluster(clus_model, data = df3, ellipse.type = "convex", ggtheme = theme_minimal())

d4 <-data4[,9:length(data4)]
df4 <-subset(d4, select = -c(DRPM_1, DFR_1,DCL_1, CL_1, Z_1, RPM_1, FR_1)) %>% scale()
rownames(df4)<-0:(nrow(df4)-1)
head(df4)
new_clusters4 <- apply(df4, 1, closest.cluster)
new_clusters4
clus_4<-setNames(data.frame(cbind(PartCount2, as.numeric(new_clusters4))), c("PartCount", "cluster"))
clus_4$cluster<-as.factor(clus_4$cluster)
head(clus_4)
plot_ly(clus_4, x=~PartCount, y=~cluster, color=~cluster, type='scatter', mode='lines+markers')
plot_ly(clus_4, x=~PartCount, y=~cluster, type='scatter', mode='lines+markers')
append1<-df4[1:2,]
append<-append1+0.05
rownames(append)<-c('100', '200')
df4_2 <-rbind(df4, append)
fviz_cluster(clus_model, data = df4_2, ellipse.type = "convex", ggtheme = theme_minimal())


data1_clus<-setNames(data.frame(cbind(data1, new_clusters1)), c(colnames(data1), "clus_val"))
head(data1_clus)
#write.csv(data1_clus,"~/onedrive/Scenic/data/indexG220/ml/data1_clus.csv", row.names = FALSE)
data2_clus<-setNames(data.frame(cbind(data2, new_clusters2)), c(colnames(data2), "clus_val"))
head(data2_clus)
#write.csv(data2_clus,"~/onedrive/Scenic/data/indexG220/ml/data2_clus.csv", row.names = FALSE)
data3_clus<-setNames(data.frame(cbind(data3, new_clusters3)), c(colnames(data3), "clus_val"))
head(data3_clus)
#write.csv(data3_clus,"~/onedrive/Scenic/data/indexG220/ml/data3_clus.csv", row.names = FALSE)
data4_clus<-setNames(data.frame(cbind(data4, new_clusters4)), c(colnames(data4), "clus_val"))
head(data4_clus)
#write.csv(data4_clus,"~/onedrive/Scenic/data/indexG220/ml/data4_clus.csv", row.names = FALSE)
