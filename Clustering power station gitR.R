library(tidyverse)
library(ggplot2)
library(GGally)
library(reshape2)
library(clValid)

char_data = read.csv('Characteristics.csv')



# summary of the data set

nrow(char_data)
ncol(char_data)
summary(char_data)

names(char_data) = tolower(names(char_data))

char_data

# The dataset has 948 rows and 7 columns.The summary of the data set shows that the data set has no nonsensical values.

char_data = char_data %>% mutate(grid_reference = factor(grid_reference), substation_number = factor(substation_number), transformer_type = factor(transformer_type))
summary(char_data2)


#plots
# Percentage of I and C
ggplot(char_data2) +
 aes(x = substation_number, weight = percentage_ic) +
 geom_bar() +
 theme_minimal()

ggplot(char_data2) +
 aes(x = grid_reference, y =  percentage_ic) +
  geom_bar() +
  theme_minimal()

ggplot(char_data2) +
  aes(x = grid_reference, weight = percentage_ic) +
  geom_bar() +
  theme_minimal()


# Transformer rating
ggplot(char_data2) +
  aes(x = substation_number, weight = transformer_rating) +
  geom_bar() +
  theme_minimal()

ggplot(char_data2) +
  aes(x = transformer_rating) +
  geom_density() +
  theme_minimal()

ggplot(char_data2) +
  aes(x = percentage_ic) +
  geom_density() +
  theme_minimal()




#Transformer Type
ggplot(char_data2) +
 aes(x = transformer_type, y = transformer_rating) +
 geom_boxplot()

ggplot(char_data2) +
 aes(x = transformer_type, y = percentage_ic) +
 geom_boxplot() 




# relations between substation characteristics
ggpair_plot = char_data2 %>% select(-substation_number, - grid_reference)


ggpairs(ggpair_plot,
        lower = list(continous = "smooth"),
        diag = list(continous = "barDiag"),
        axisLabels = 'show')

Modified_January_2013 = January_2013

# making substation as factor
Modified_January_2013 = Modified_January_2013 %>% mutate(Substation = factor(Substation))



#creating an max column
Modified_January_2013$Max = apply(Modified_January_2013[3:146], MARGIN =  1, FUN = max, na.rm = T)


#dividing by max
mod_new = Modified_January_2013[,3:147]
mod_new = mod_new/mod_new$Max
subs = select(Modified_January_2013, Date, Substation, Max  )
new = cbind(subs,new)


#grouping all substations 
mean_jan = aggregate(new[,4:147], list(new$Substation), FUN = mean)

# Distance Matrix
distance_M = dist(mean_jan, method = "euclidean")

cluster_all = hclust(distance_M)
plot(cluster_all)


# dunn indexing:
cluster_point = cutree(cluster_all, k = 5)
dunn(distance_M,cluster_point)
# based on the dunn index we select the number of cluster here we see that the cluaster where we get the best dunn index without being complicated is 5

mean_jan_clust = cbind(mean_jan,cluster_point)
mean_jan_clust = mean_jan_clust %>% mutate(cluster_point = factor(cluster_point))

#number of substations in each cluster
table(mean_jan_clust$cluster_point)

distance_M = dist(mean_jan, method = "euclidean")

test_distance = dist(mean_jan[-1], method = "euclidean")

test_model = as.matrix(test_distance)
test_model_2 = as.matrix(distance_M)
cluster_all_1 = hclust(distance_M)
cluster_all_2 = hclust(test_distance)
windows() 
plot(cluster_all_2)
windows()

cluster_point = cutree(cluster_all_2, k = 4)
dunn(test_distance,cluster_point)

sil = silhouette(cluster_point,test_distance)
windows() 
plot(sil)


ggplot(mean_jan_clust) +
 aes(x = substation, y = `00:10`, colour = cluster_point) +
 geom_point(data = centroids, shape = 2, size = 5)
 scale_color_hue(direction = 1) +
 theme_minimal()

 
centroids = aggregate(mean_jan_clust[,2:145], list(mean_jan_clust$cluster_point), FUN = mean)
centroids = centroids %>% rename(cluster_point = "Group.1")


centroids = aggregate(mean_jan_clust[,2:145], list(mean_jan_clust$cluster_point), FUN = mean)
centroids
#plot with centroid:

ggplot(mean_jan_clust, aes(y = `10:00`, x = `20:00`, colour = cluster_point)) +
  geom_point() +
  geom_point(data = centroids, shape = 15,size = 5)+
  theme_minimal()

ggplot(mean_jan_clust) +
  aes(x = substation, y = `00:10`, colour = cluster_point) +
  geom_point(shape = "circle", size = 1.5) +
  scale_color_hue(direction = 1) +
  theme_minimal()



summary(mean_jan_clust)
# LONG FORMAT

data_long_full = mean_jan_clust %>%
  gather(variable,value,-c(substation,cluster_point)) 


#plot of time against value:

ggplot(data_long_full) +
 aes(x = variable, y = value, colour = cluster_point) +
 geom_point(shape = "circle small", size = 1.5) +
 scale_color_brewer(palette = "Set2", direction = 1) +
 theme_minimal()

# Finding mean to make sense:

mean_val = aggregate(value ~ variable + cluster_point, data_long_arranged, mean)
mean_val = mean_val %>% rename(power = 'value', time = 'variable')



ggplot(mean_val) +
 aes(x = time, y = power, colour = cluster_point) +
 geom_point(shape = "circle", size = 1.5) +
  geom_line()
 scale_color_hue(direction = 1) +
 theme_minimal()


cluster_1 = filter(data_long_arranged , cluster_point == '1')
cluster_2 = filter(data_long_arranged, cluster_point == '2')
cluster_3 = filter(data_long_arranged, cluster_point == '3')
cluster_4 = filter(data_long_arranged, cluster_point == '4')
cluster_5 = filter(data_long_arranged, cluster_point == '5')

cluster_1a = filter(mean , cluster_point == '1')
cluster_2a = filter(mean, cluster_point == '2')
cluster_3a = filter(mean, cluster_point == '3')
cluster_4a = filter(mean, cluster_point == '4')
cluster_5a = filter(mean, cluster_point == '5')





ncol(mean_jan_clust)
ggplot(cluster_1a) +
 aes(x = variable, y = value, colour = value) +
 geom_line(shape = "circle", size = 1.5) +
 scale_color_gradient() +
 theme_minimal()


ggplot(cluster_1a) +
 aes(x = variable, y = value) +
 geom_line()
cluster_1a = cluster_1a %>% mutate (variable = factor(variable))
plot(cluster_1a$variable,cluster_1a$value, type="l", col="green", lwd=5, xlab="time", ylab="concentration", main="title")


df_dummy_1 = mean_jan_clust%>% select(Group.1,cluster_point)
original_clust = January_2013

original_clust = original_clust %>% rename(Group.1 = Substation) 
original_clust = original_clust %>% mutate(Group.1 = factor(Group.1))

comb_original = merge(df_dummy_1,original_clust, by = 'Group.1', all = T)


#weekend
comb_original$weekday = weekdays(comb_original$Date)

is_weekend = function(n){
  require(lubridate)
  (ifelse(wday(as.Date(n)) == 1, T, F) | ifelse(wday(as.Date(n)) == 7, T, F))
} 
comb_original$weekend = is_weekend(comb_original$Date)

comb_original_long = comb_original %>%
  gather(Time,power,-c(substation,cluster_point,Date,weekend,weekday)) 

comb_original_long = arrange(comb_original_long, substation, Time)


# mean for weekend and weekday
original_mean = aggregate(power ~ Time + cluster_point + weekend, comb_original_long, mean)
original_mean = arrange(original_mean,cluster_point,Time)
original_mean = original_mean %>% mutate(weekend = factor(weekend))


ggplot(original_mean) +
 aes(x = Time, y = power, colour = weekend) +
 geom_point(shape = "circle", size = 1.5) +
 scale_color_hue(direction = 1) +
 theme_minimal() +
 facet_wrap(vars(cluster_point))




#WEEKEND AND WEEKDAY
only_weekday = comb_original%>%filter(weekend == 'FALSE')

only_weekend = comb_original%>%filter(weekend == 'TRUE')

#LONG FORMAT
weekday_long = only_weekday %>%
  gather(Time,value,-c(Group.1,cluster_point,Date)) 

weekday_long = arrange(weekday_long, Group.1)

mean_weekday = aggregate(value ~ variable + cluster_point, data_long_arranged, mean)

mean_weekday


##########################################################################


add_substation = new_substations

add_substation = add_substation %>% mutate(Substation = factor(Substation))

#weekend and weekday

add_substation[['Date']] = strptime(add_substation[['Date']],format = "%Y-%m-%d")

add_substation$weekday = weekdays(add_substation$Date)
add_substation$weekend = is_weekend(add_substation$Date)
add_substation = add_substation %>% mutate(weekend  = factor(weekend))


#long format 
add_substation = select(add_substation, -Date)
add_substation_long = add_substation %>%
  gather(Time,power,-c(Substation,weekend,weekday)) 
summary(add_substation_long)

mean_add_sub = aggregate(power ~ Substation + Time + weekend,add_substation_long, mean)
mean_add_sub = arrange(mean_add_sub,Substation,Time)


ggplot(mean_add_sub) +
 aes(x = Time, y = power, colour = weekend) +
 geom_point(shape = "circle", size = 1.5) +
 scale_color_hue(direction = 1) +
 theme_minimal() +
 facet_wrap(vars(Substation))


##mean of all based on substation
ncol(comb_original)

centroids_org = aggregate(comb_original[,4:147], list(comb_original$cluster_point), FUN = mean)
centroids_org = centroids_org %>% rename(cluster_point = "Group.1")

ncol(centroids_org)
euclidian_distance <- function(x, y) {
  diff <- x - y
  sqrt(sum(diff^2))
}

avg_new_station = new_substations %>% group_by(Substation) %>%  
  select(-Date) %>% 
  summarise_all(mean)

avg_new_station = avg_new_station %>% select (-Substation)

i = 1 
point = avg_new_station[i,]
centroid_id = 1

assign_point_to_cluster <- function(point, centroids_df) {
  k = nrow(centroids_df)
  m = ncol(centroids_df)
  distances = vector(length = k)
  for(centroid_id in 1:k) {
    centroid_pnt = centroids_df[centroid_id, 2:m]
    distances[centroid_id] = euclidian_distance(point, centroid_pnt)
  }
  which.min(distances)
}

assign_all_points_to_clusters <- function(avg_new_station, centroids_df) {
  npoints = nrow(avg_new_station)
  clusters = vector(length=npoints)
  for(i in 1:npoints) {
    point <- avg_new_station[i, ]
    clusters[i] <- assign_point_to_cluster(point, centroids_df)
  }
  clusters
}

assign_all_points_to_clusters(avg_new_station, centroids_org)



avg_new_station = avg_new_station%>% mutate(cluster = new_clust)


# merging with original data

comb_original
comb_org_substation = aggregate(comb_original[,4:147], list(comb_original$Substation), FUN = mean)

centroids_org = centroids_org %>% rename(cluster_point = "Group.1")



avg_new_station

comb_org_mean = select(comb_original, -Date, -weekday, -weekend)
comb_org_mean = comb_org_mean %>% mutate(cluster_point = factor(cluster_point))


centroid_org

org_clust_avg_sub = aggregate(original_clust[,3:146], list(original_clust$substation), FUN = mean)
org_clust_avg_sub  =org_clust_avg_sub %>% rename(substation = "Group.1")
org_clust_avg_sub = org_clust_avg_sub %>% mutate(substation = factor(substation))

org_avg_sub_with_clust = merge(org_clust_avg_sub,df_dummy_1, by = 'substation', all = T)

org_avg_sub_with_clust =org_avg_sub_with_clust %>% rename(cluster = "cluster_point", Substation = "substation")

new_sub_data = rbind(avg_new_station,org_avg_sub_with_clust)


centroid_with_NA = new_df_with_and_without_clust %>%
  group_by(cluster)%>%
  select(-cluster,-Substation)%>%
  summarise_all(mean)

ggplot(new_df_with_and_without_clust, aes(y = `10:00`, x = `20:00`, colour = is.na(cluster))) +
  geom_point() +
  geom_point(data = centroid_with_NA, shape = 15,size = 5)+
  theme_minimal()

# CENTROID FOR DATA WITH ADDITIONAL NEW SUBSTATION
centroid_with_new_sub = new_sub_data %>%
  group_by(cluster)%>%
  select(-cluster,-Substation)%>%
  summarise_all(mean)

# CHANGE THE CENTROID 
ggplot(new_sub_data, aes(y = `10:00`, x = `20:00`, colour = cluster)) +
  geom_point() +
  geom_point(data = centroid_with_new_sub, shape = 15,size = 5)+
  theme_minimal()+
  windows()
