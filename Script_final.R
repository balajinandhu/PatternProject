setwd("~/Desktop/h")

library(dplyr)
library(date)
library(cluster)
library(fpc)
library(e1071)
library(caret)

total <- 100
# create progress bar
pb <- txtProgressBar(min = 0, max = total, style = 3)

  
#close(pb)

csvDataBusiness <- read.csv("~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_academic_dataset_business.csv", na.strings="?")
csvDataReview <- read.csv("~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_academic_dataset_review.csv", na.strings="?")
csvDataUser <- read.csv("~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_academic_dataset_user.csv", na.strings="?")
csvDataCheckIn <- read.csv("~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_academic_dataset_checkin.csv", na.strings="?")


#select only businesses in Las Veags
csvFilteredBusinesses <- csvDataBusiness[which(csvDataBusiness$city == "Las Vegas"),]

# make these businesses a data frame and collect these ID's
dfFilteredBusiness <- as.data.frame(csvFilteredBusinesses)
dfFilteredBusiness1 <- filter(dfFilteredBusiness, grepl('Restaurants', categories))
write.csv(file="~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_LVRES_dataset_business.csv", x=dfFilteredBusiness1)
#rm(test)
businessIds <- dfFilteredBusiness[,c("business_id")]

# Convert review csv to data frame and filter based on business ids
dfReview <- as.data.frame(csvDataReview)
csvFilteredReview <- dfReview[dfReview$business_id %in% businessIds,] 
dfFilteredReview <- as.data.frame(csvFilteredReview)
write.csv(file="~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_LVRES_dataset_review.csv", x=dfFilteredReview)

# Based on the user id's from filtered Review, filter users from the user table
userIds <- dfFilteredReview[,c("user_id")]
dfUser <- as.data.frame(csvDataUser)
csvFilteredUser <- dfUser[dfUser$user_id %in% userIds,]
dfFilteredUser <- as.data.frame(csvFilteredUser)

# Filter the checkIn's based on the business ids
dfCheckIn <- as.data.frame(csvDataCheckIn)
csvFilteredCheckIn <- dfCheckIn[dfCheckIn$business_id %in% businessIds,]
dfFilteredCheckIn <- as.data.frame(csvFilteredCheckIn)

# Splitting the data into Training and TEST for all the data i.e. business, user, reviews, checkins 
index_bus_test = sample(1:nrow(dfFilteredBusiness), size=0.7*nrow(dfFilteredBusiness))
tempFilteredBusiness=dfFilteredBusiness[index_bus_test,]
testFilteredBusiness=dfFilteredBusiness[-index_bus_test,]
index_bus_train = sample(1:nrow(tempFilteredBusiness), size=0.7*nrow(tempFilteredBusiness))
trainFilteredBusiness=tempFilteredBusiness[index_bus_train,]
validationFilteredBusiness=tempFilteredBusiness[-index_bus_train,]
#write.csv(file="~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_train_dataset_business.csv", x=trainFilteredBusiness)
trainBusinessIds <- trainFilteredBusiness[,c("business_id")]
validationBusinessIds <- validationFilteredBusiness[,c("business_id")]
testBusinessIds <- testFilteredBusiness[,c("business_id")]


#indexes = sample(1:nrow(dfFilteredReview), size=0.7*nrow(dfFilteredReview))
trainFilteredReview=dfFilteredReview[dfFilteredReview$business_id %in% trainBusinessIds,] 
validationFilteredReview=dfFilteredReview[dfFilteredReview$business_id %in% validationBusinessIds,] 
testFilteredReview=dfFilteredReview[dfFilteredReview$business_id %in% testBusinessIds,] 
#write.csv(file="~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_train_dataset_review.csv", x=trainFilteredReview)
trainUserIds <- trainFilteredReview[,c("user_id")]
validationUserIds <- validationFilteredReview[,c("user_id")]
testUserIds <- testFilteredReview[,c("user_id")]

#indexes = sample(1:nrow(dfFilteredUser), size=0.7*nrow(dfFilteredUser))
trainFilteredUser=dfFilteredUser[dfFilteredUser$user_id %in% trainUserIds,]
validationFilteredUser=dfFilteredUser[dfFilteredUser$user_id %in% validationUserIds,]
testFilteredUser=dfFilteredUser[dfFilteredUser$user_id %in% testUserIds,]

# indexes = sample(1:nrow(dfFilteredCheckIn), size=0.7*nrow(dfFilteredCheckIn))
# trainFilteredCheckIn=dfFilteredCheckIn[indexes,]
# testFilteredCheckIn=dfFilteredCheckIn[-indexes,]



# helper methods
error_pc <- function(resi,actual){
  (resi/actual)*100
}

error_ratio <- function(actual,predicted){
  (predicted-actual/mean(predicted)-actual)
}

# Time dependent features
noOfReviews <- nrow(trainFilteredReview)

#---finished running filter on restaurants, city---

e <- new.env(hash = TRUE, parent=emptyenv(), size=100L)
count <- 0
pb <- txtProgressBar(min = 0, max = length(businessIds), style = 3)
cat("\n\nHang tight...Hashing Businesses to reviews...\n\n\n")
for (i in businessIds){
  my_key <- i
  assign(my_key, subset(dfFilteredReview,business_id==i), e)
  setTxtProgressBar(pb, count)
  count <- count + 1
}
close(pb)

########### SCALING - EXECUTE ONCE MORE ##########

# store the computed time dependent features in a data frame
timeFeatureVect <- data.frame()
count <- 0
pb <- txtProgressBar(min = 0, max = length(businessIds), style = 3)
cat("\n\nAwww..Fetching beautiful features...\n\n\n")
for(i in businessIds){
  dfTemp <- get(i, e)
  count <- 1
  price_range <- dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Price.Range"]
  newRow = data.frame(business_id = i,
                      num_reviews = nrow(dfTemp),
                      avg_stars = mean(dfTemp[,c("stars")]),
                      max_stars = max(dfTemp[,c("stars")]),
                      min_stars = min(dfTemp[,c("stars")]),
                      num_cool_votes = sum(dfTemp$votes.cool>0,na.rm = TRUE),
                      num_funny_votes = sum(dfTemp$votes.funny>0,na.rm = TRUE),
                      num_useful_votes = sum(dfTemp$votes.useful>0,na.rm = TRUE),
                      num_uniq_reviewers = as.numeric(apply(subset(dfTemp, select=c("user_id")), 2, FUN = function(x) length(unique(x)))),
                      days_first_attention = as.numeric(Sys.Date()-min(as.Date(dfTemp$date))),
                      days_last_attention = as.numeric(Sys.Date()-max(as.Date(dfTemp$date))),
                      days_between_attention = as.numeric(max(as.Date(dfTemp$date))-min(as.Date(dfTemp$date))),
                      max_reviews_per_day = as.numeric(max(table(dfTemp$date))),
                      reservations_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Takes.Reservations"]=='True',
                      waiter_service_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Waiter.Service"]=='True',
                      wheel_chair_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Wheelchair.Accessible"]=='True',
                      price_range_1or2 = price_range == 1 | price_range == 2,
                      outdoor_seating_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Outdoor.Seating"]=='True',
                      credit_cards_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Accepts.Credit.Cards"]=='True',
                      take_out_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Take.out"]=='True',
                      pkg_lot_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Parking.lot"]=='True',
                      wifi_free = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Wi.Fi"]=='free',
                      lunch_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Good.For.lunch"]=='True',
                      has_TV_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Has.TV"]=='True',
                      groups_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Good.For.Groups"]=='True',
                      caters_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Caters"]=='True',
                      casual_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Ambience.casual"]=='True',
                      kids_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Good.for.Kids"]=='True',
                      dinner_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Good.For.dinner"]=='True',
                      attire_casual = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Attire"]=='casual',
                      noise_level_average = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Noise.Level"]=='average',
                      alcohol_full_bar = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Alcohol"]=='full_bar'
                      
  )
  setTxtProgressBar(pb, count)
  count <- count + 1
  timeFeatureVect <- rbind(timeFeatureVect, newRow)
}
close(pb)

meanValues <- colMeans(timeFeatureVect[,2:13])
centeredTimeFeatVec <- sweep(timeFeatureVect[,2:13],2, meanValues, "-")
sdValues = vector(mode = "double", length = 12)
for(i in 2:13){
  sdValues[i-1] <- sd(timeFeatureVect[,i])
}
scaledTimeFeatureVect <- sweep(centeredTimeFeatVec,2, sdValues, "/") 
timeFeatureVect[,2:13] <- scaledTimeFeatureVect

trainTimeFeatureVect <- timeFeatureVect[timeFeatureVect$business_id%in%trainBusinessIds,]
validationTimeFeatureVect <- timeFeatureVect[timeFeatureVect$business_id %in% validationBusinessIds,]
testTimeFeatureVect <- timeFeatureVect[timeFeatureVect$business_id%in%testBusinessIds,]


########### END OF SCALING ################

# #handling missing data in timeFeatureVect
# levels(timeFeatureVect$wheel_chair_true) <- c(NA, "True", "False")
# levels(timeFeatureVect$reservations_true) <- c(NA, "True", "False")
# levels(timeFeatureVect$waiter_service_true) <- c(NA, "True", "False")
# levels(timeFeatureVect$outdoor_seating_true) <- c(NA, "True", "False")
# levels(timeFeatureVect$credit_cards_true) <- c(NA, "True", "False")
# levels(timeFeatureVect$take_out_true) <- c(NA, "True", "False")
# levels(timeFeatureVect$pkg_lot_true) <- c(NA, "True", "False")





trainUserIds <- trainFilteredUser[,c("user_id")]
testUserIds <- testFilteredUser[,c("user_id")]
pb <- txtProgressBar(min = 0, max = length(userIds), style = 3)
user_hash <- new.env(hash = TRUE, parent=emptyenv(), size=100L)
count=1
cat("\n\nHang tight...Hashing Users to their reviews...\n\n\n")
for (i in userIds){
  my_key <- i
  assign(my_key, subset(dfFilteredReview,user_id==i), user_hash)
  setTxtProgressBar(pb, round(count))
  count <- count + total/length(userIds)
}

close(pb)

# Number of reviews per user
userFeatureVectors <- data.frame()
count <- 1
pb <- txtProgressBar(min = 0, max = length(userIds), style = 3)
cat("\n\nAwww...Engineering beautiful features...\n\n\n")
for(i in userIds){
  dfTemp <- get(i, user_hash)
  #changed code here for features
  newRow = data.frame(user_id = i,
                      num_reviews = trainFilteredUser[i, "review_count"],
                      avg_stars = trainFilteredUser[i, "average_stars"],
                      num_funny_votes = trainFilteredUser[i, "votes.funny"],
                      num_useful_votes = trainFilteredUser[i, "votes.useful"],
                      num_cool_votes = trainFilteredUser[i, "votes.cool"],
                      date_of_first_review = as.numeric(Sys.Date()-min(as.Date(dfTemp$date))),
                      date_of_last_review = as.numeric(Sys.Date()-max(as.Date(dfTemp$date))),
                      days_between_reviews = as.numeric(max(as.Date(dfTemp$date))-min(as.Date(dfTemp$date)))
  )
  setTxtProgressBar(pb, count)
  count <- count + 1
  userFeatureVectors <- rbind(userFeatureVectors, newRow)
}
close(pb)

date_of_last_review <- unlist(userFeatureVectors[,c('date_of_last_review')])
date_of_last_review[is.infinite(date_of_last_review)] <- NA 
userFeatureVectors$date_of_last_review[is.infinite(userFeatureVectors$date_of_last_review)] <- NA 
userFeatureVectors$date_of_last_review[is.na(userFeatureVectors$date_of_last_review)] <- mean(date_of_last_review, na.rm = TRUE) 

days_bw_reviews <- unlist(userFeatureVectors[,c('days_between_reviews')])
days_bw_reviews[is.infinite(days_bw_reviews)] <- NA 
userFeatureVectors$days_between_reviews[is.infinite(userFeatureVectors$days_between_reviews)] <- NA 
userFeatureVectors$days_between_reviews[is.na(userFeatureVectors$days_between_reviews)] <- mean(days_bw_reviews, na.rm = TRUE) 

date_of_first_review <- unlist(userFeatureVectors[,c('date_of_first_review')])
date_of_first_review[is.infinite(date_of_first_review)] <- NA 
userFeatureVectors$date_of_first_review[is.infinite(userFeatureVectors$date_of_first_review)] <- NA 
userFeatureVectors$date_of_first_review[is.na(userFeatureVectors$date_of_first_review)] <- mean(date_of_first_review, na.rm = TRUE) 

res <- kmeans(userFeatureVectors[,2:9], 6)
aggregate(userFeatureVectors[,2:9],by=list(res$cluster),FUN=mean)

# allFeatureVector <- c(timeFeatureVect, userFeatureVectors)
# dfAllFeatureVector <- as.data.frame(timeFeatureVect)

########################### TRAINING SET ###########################
modFeatures <- trainTimeFeatureVect[!rowSums(is.na(trainTimeFeatureVect)), ]
model <- svm(num_reviews~. , modFeatures, kernel = "linear", cross = 10)
m <- predict(model, newdata = modFeatures)

x <- error_pc(model$residuals, modFeatures$num_reviews)
mean(x) #CV error with train dataset is -5.222521
plot(x,modFeatures$num_reviews)

y <- error_ratio(modFeatures$num_reviews, m)
plot(y,modFeatures$num_reviews)

########### TUNING THE MODEL ############
modFeatures <- validationTimeFeatureVect[!rowSums(is.na(validationTimeFeatureVect)), ]
tuneResult <- tune(svm, num_reviews~.,  data = modFeatures,
                   ranges = list(epsilon = seq(0,1,0.1), cost = 2^(2:9), kernel = "linear")
)
print(tuneResult)

tunedModel <- tuneResult$best.model
tunedModelY <- predict(tunedModel, modFeatures)

x <- error_pc(modFeatures$num_reviews-tunedModelY, modFeatures$num_reviews)
mean(x) #error after tuning is 0.02079183

################ TESTING SET #################
modFeatures <- testTimeFeatureVect[!rowSums(is.na(testTimeFeatureVect)), ]
tunedModelY <- predict(tunedModel, newdata = modFeatures)

x <- error_pc(modFeatures$num_reviews-tunedModelY, modFeatures$num_reviews)
mean(x) #test error is 1.187648


##### TEMPORAL PREDICTION ######
getTimeVectorsBeforeDt <- function(dt){
  
  tempVect <- data.frame()
  count <- 1
  cat("\n\nFetching Features...\n\n\n")
  pb <- txtProgressBar(min = 0, max = length(trainBusinessIds), style = 3)
  for(i in trainBusinessIds){
    dfTemp <- get(i, e) #modify dfTemp to have only reviews before the target date
    dfTemp <- dfTemp[which(as.Date(dfTemp$date)<=as.Date(dt)),]
    future_date <- seq(as.Date(dt), length = 2, by = "6 months")[2]
    dfTemp2 <- dfTemp[which(as.Date(dfTemp$date)<=as.Date(future_date)),]
    if(nrow(dfTemp)>0){     #needs fixup of review count >0
      newRow = data.frame(business_id = i,
                          num_reviews = nrow(dfTemp2),
                          avg_stars = mean(dfTemp[,c("stars")]),
                          max_stars = max(dfTemp[,c("stars")]),
                          min_stars = min(dfTemp[,c("stars")]),
                          num_cool_votes = sum(dfTemp$votes.cool>0,na.rm = TRUE),
                          num_funny_votes = sum(dfTemp$votes.funny>0,na.rm = TRUE),
                          num_useful_votes = sum(dfTemp$votes.useful>0,na.rm = TRUE),
                          num_uniq_reviewers = as.numeric(apply(subset(dfTemp, select=c("user_id")), 2, FUN = function(x) length(unique(x)))),
                          days_first_attention = as.numeric(Sys.Date()-min(as.Date(dfTemp$date))),
                          days_last_attention = as.numeric(Sys.Date()-max(as.Date(dfTemp$date))),
                          days_between_attention = as.numeric(max(as.Date(dfTemp$date))-min(as.Date(dfTemp$date))),
                          max_reviews_per_day = as.numeric(max(table(dfTemp$date))),
                          reservations_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Takes.Reservations"]=='True',
                          waiter_service_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Waiter.Service"]=='True',
                          wheel_chair_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Wheelchair.Accessible"]=='True',
                          price_range_1or2 = price_range == 1 | price_range == 2,
                          outdoor_seating_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Outdoor.Seating"]=='True',
                          credit_cards_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Accepts.Credit.Cards"]=='True',
                          take_out_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Take.out"]=='True',
                          pkg_lot_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Parking.lot"]=='True',
                          wifi_free = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Wi.Fi"]=='free',
                          lunch_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Good.For.lunch"]=='True',
                          has_TV_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Has.TV"]=='True',
                          groups_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Good.For.Groups"]=='True',
                          caters_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Caters"]=='True',
                          casual_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Ambience.casual"]=='True',
                          kids_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Good.for.Kids"]=='True',
                          dinner_true = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Good.For.dinner"]=='True',
                          attire_casual = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Attire"]=='casual',
                          noise_level_average = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Noise.Level"]=='average',
                          alcohol_full_bar = dfFilteredBusiness[which(dfFilteredBusiness$business_id== i),"attributes.Alcohol"]=='full_bar'
                          
      )
     
     
      tempVect <- rbind(tempVect, newRow)
    }
    setTxtProgressBar(pb, count)
    count <- count + 1 
  }
  close(pb)
  return(tempVect)
}


# dfFilteredReview = 2004-10-12 to 2015-01-08
log <- data.frame()
log <- aggregate(dfFilteredReview$date, by = list(month = substr(dfFilteredReview$date, 1, 7)), FUN = length)
barplot(log$x, names.arg =c(1:122),ylim=c(0,20000), ylab="number of reviews")


#cumsum(test1$num_reviews)[2018]

train <-getTimeVectorsBeforeDt("2013-03-01")
tbIDs <- train[,c("business_id")]

index_bus_test = sample(1:nrow(train), size=0.7*nrow(train))
train_set=train[index_bus_test,]
test_set=train[-index_bus_test,]

########################### TRAINING SET ###########################
modFeatures <- train_set[!rowSums(is.na(train_set)), ]
model_t <- svm(num_reviews~. , modFeatures, kernel = "linear", cross = 10)
m <- predict(model_t, newdata = modFeatures)

x <- error_pc(model_t$residuals, modFeatures$num_reviews)
mean(x) 

################ TESTING SET #################
modFeatures1 <- test_set[!rowSums(is.na(test_set)), ]
#modFeatures1$business_id = factor(modFeatures1$business_id,levels=tbIDs)  
tunedModelY <- predict(model_t, newdata = modFeatures1)

x <- error_pc(modFeatures1$num_reviews-tunedModelY, modFeatures1$num_reviews)
mean(x)

#cumsum(log$x)[100]
#target date = "2013-03-01" , month 100



# predictedY <- predict(model, timeFeatureVect)
# model$residuals

# modFeatures <- timeFeatureVect[!rowSums(is.na(timeFeatureVect)), ]
# model <- svm(num_reviews~. , modFeatures)
# m <- predict(model, newdata = modFeatures)
#plot(modFeatures$num_reviews, m, col = "red", pch=4)

# 
# ggplot(as.data.frame(res$cluster), aes(x=factor(1), fill=factor(res$cluster))) + 
#   geom_bar(width=1) + 
#   coord_polar(theta="y") + 
#   theme(panel.background=element_blank(),
#         axis.title=element_blank(), 
#         axis.ticks=element_blank(), 
#         panel.grid=element_blank(), 
#         axis.text.y=element_blank(),
#         legend.position="none", 
#         axis.text.x=element_blank()) + 
#   ylab("") + 
#   xlab("")
# 
# clusplot(userFeatureVectors[,2:9], res$cluster, color=TRUE, shade=TRUE, labels=2, lines=0)
# plot(res)
# clusplot(userFeatureVectors[,2:9], res$cluster, main='2D representation of the Cluster solution',
#          color=TRUE, shade=TRUE,
#          labels=2, lines=0)
# 
# 
# for(i in 1:6){
#   print(res$size[i]*100/sum(res$size))
# }


# # Generating Heat Maps
# vegas_map <- get_map(location = "Las Vegas", maptype = "satellite", zoom = 12)
# 
# ggmap(vegas_map, extent = "device") + geom_point(aes(x = longitude, y = latitude), colour = "red", alpha = 0.1, size = 2, data = dfFilteredBusiness)
# 
# vegas_map_g_str <- get_map(location = "Las Vegas", zoom = 13)
# 
# ggmap(vegas_map_g_str, extent = "device") + geom_density2d(data = dfFilteredBusiness, aes(x = longitude, y = latitude), size = 0.3) + stat_density2d(data = dfFilteredBusiness, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)


