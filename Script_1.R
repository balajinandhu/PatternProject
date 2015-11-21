library(dplyr)

csvDataBusiness <- read.csv("~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_academic_dataset_business.csv", na.strings="?")
csvDataReview <- read.csv("~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_academic_dataset_review.csv", na.strings="?")
csvDataUser <- read.csv("~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_academic_dataset_user.csv", na.strings="?")
csvDataCheckIn <- read.csv("~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_academic_dataset_checkin.csv", na.strings="?")


#select only businesses in Las Veags
csvFilteredBusinesses <- csvDataBusiness[which(csvDataBusiness$city == "Las Vegas"),]

# make these businesses a data frame and collect these ID's
dfFilteredBusiness <- as.data.frame(csvFilteredBusinesses)
dfFilteredBusiness <- filter(dfFilteredBusiness, grepl('Restaurants', categories))

#rm(test)
businessIds <- dfFilteredBusiness[,c("business_id")]

# Convert review csv to data frame and filter based on business ids
dfReview <- as.data.frame(csvDataReview)
csvFilteredReview <- dfReview[dfReview$business_id %in% businessIds,] 
dfFilteredReview <- as.data.frame(csvFilteredReview)

# Based on the user id's from filtered Review, filter users from the user table
userIds <- dfFilteredReview[,c("user_id")]
dfUser <- as.data.frame(csvDataUser)
csvFilteredUser <- dfUser[dfUser$user_id %in% userIds,]
dfFilteredUser <- as.data.frame(csvFilteredUser)

# Filter the checkIn's based on the business ids
dfCheckIn <- as.data.frame(csvDataCheckIn)
csvFilteredCheckIn <- dfCheckIn[dfCheckIn$business_id %in% businessIds,]
dfFilteredCheckIn <- as.data.frame(csvFilteredCheckIn)

# Time dependent features
noOfReviews <- nrow(dfFilteredReview)

#---finished running filter on restaurants, city---

e <- new.env(hash = TRUE, parent=emptyenv(), size=100L)
 count <- 0
for (i in businessIds){
  my_key <- i
  assign(my_key, subset(trainFilteredReview,business_id==i), e)
   print(count)
   count <- count + 1
}

# store the computed time dependent features in a data frame
timeFeatureVect <- data.frame()
 count <- 0
for(i in businessIds){
  dfTemp <- get(i, e)
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
    max_reviews_per_day = as.numeric(max(table(dfTemp$date)))
  )
   print(count)
   count <- count + 1
  timeFeatureVect <- rbind(timeFeatureVect, newRow)
}

 # Splitting the data into Training and TEST for all the data i.e. business, user, reviews, checkins 
indexes = sample(1:nrow(dfFilteredBusiness), size=0.7*nrow(dfFilteredBusiness))
trainFilteredBusiness=dfFilteredBusiness[indexes,]
testFilteredBusiness=dfFilteredBusiness[-indexes,]
write.csv(file="~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_train_dataset_business.csv", x=trainFilteredBusiness)

indexes = sample(1:nrow(dfFilteredUser), size=0.7*nrow(dfFilteredUser))
trainFilteredUser=dfFilteredUser[indexes,]
testFilteredUser=dfFilteredUser[-indexes,]

indexes = sample(1:nrow(dfFilteredReview), size=0.7*nrow(dfFilteredReview))
trainFilteredReview=dfFilteredReview[indexes,]
testFilteredReview=dfFilteredReview[-indexes,]
write.csv(file="~/Downloads/yelp_dataset_challenge_academic_dataset/csv/yelp_train_dataset_review.csv", x=trainFilteredReview)

indexes = sample(1:nrow(dfFilteredCheckIn), size=0.7*nrow(dfFilteredCheckIn))
trainFilteredCheckIn=dfFilteredCheckIn[indexes,]
testFilteredCheckIn=dfFilteredCheckIn[-indexes,]


# Number of reviews
# Average number of stars
# Number of “Funny” votes
# Number of “Useful” votes
# Number of “Cool” votes
# Date of first review
# Date of last review
# Days between first and last review

# # Generating Heat Maps
# vegas_map <- get_map(location = "Las Vegas", maptype = "satellite", zoom = 12)
# 
# ggmap(vegas_map, extent = "device") + geom_point(aes(x = longitude, y = latitude), colour = "red", alpha = 0.1, size = 2, data = dfFilteredBusiness)
# 
# vegas_map_g_str <- get_map(location = "Las Vegas", zoom = 13)
# 
# ggmap(vegas_map_g_str, extent = "device") + geom_density2d(data = dfFilteredBusiness, aes(x = longitude, y = latitude), size = 0.3) + stat_density2d(data = dfFilteredBusiness, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..), size = 0.01, bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + scale_alpha(range = c(0, 0.3), guide = FALSE)


