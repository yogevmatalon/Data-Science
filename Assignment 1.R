#ID 1: 201390408
#ID 2: 305634271

## Download all relevant packages and libraries:
#install.packages("ggplot2")
#install.packages("corrgram")
#install.packages("reshape2")
#install.packages("DMwR")
#install.packages("lubridate")
#install.packages("dplyr") 
#install.packages("lubridate")

library("ggplot2")

# 1.a. Download and extract the data from Moodle into a local folder designated for this assignment.

# 1.b. Set your working directory to be your assignment folder for easy access. 
# From now on, if needed, do not use the full path, but only the name of the file within this path.
################################
setwd("/Users/yogevmatalon/Downloads/Assignment1")

# 1.c. Import the CSV file " chicago_taxi_data.csv " into R and save it by the name data. 
# Notice the data in the file has row numbers that are redundant (so pay attention to the function arguments).
################################
data <- read.csv("chicago_taxi_data.csv",header = TRUE)[,2:21]

# 1.d Make sure the data was loaded properly by showing the first few rows of the data.
################################
head(data, n=5)

# 2.a Sample 10000 rows from the dataset without replacement. This file will be our dataset throughout the exercise. 
# Before you sample, set your random seed to be 1. 
################################
set.seed(1)
sample.data <- data[sample(nrow(data),10000),]

# 2.b We will not use any of geographical columns (pickup/ dropoff - longtitude/ latitude). Delete these columns.
################################
sample.data <- subset(sample.data, select=-c(pickup_longitude,dropoff_latitude, dropoff_longitude, pickup_latitude))

# 2.c Show the names and the data type of all the features.
################################
str(sample.data)

# 2.d. The Column pickup_census_tract has only NA values. Create a verification check for this claim. 
#Delete the column pickup_census_tract and dropoff_census_tract from the dataset.
#Could we have known in advanced that this column is problematic? Tip: use your answer the previous question.
################################
  nrow(sample.data) == length(is.na(sample.data['pickup_census_tract'])) #all values are NA
  sample.data <- subset(sample.data, select=-c(dropoff_census_tract,pickup_census_tract))
  #ANSWER: yes, last question (str function) we saw NA values in this column

# 2.e What's your opinion about the current type of the column 'company'? Is it adequate?
#If yes, explain why. It not, explain why not and change the type of this column and other similar columns.
################################
  #ANSWER: 'comapny' column seems like a categorical feautre. Therefore we need to convery it to considered as 'factor'
  sample.data['company'] <- lapply(sample.data['company'], factor)
  #check column cardinality - number of unique values in the column
  length(unique(sample.data['dropoff_community_area'])[,1])
  length(unique(sample.data['pickup_community_area'])[,1])
    #->since cardinality is very low, we will consider these two columns as factors
    # In addition, area code are usually not ordinal. Area 1 can be closer to area 4 than area 3. 
  sample.data['dropoff_community_area'] <- lapply(sample.data['dropoff_community_area'], factor)
  sample.data['pickup_community_area'] <- lapply(sample.data['pickup_community_area'], factor)
  str(sample.data) #verity results

# 2.f. Create a summary statistics of the dataset (using one-line command). 
# What is the difference between the output for the numerical columns and the non-numeric columns?
################################
  summary(sample.data)
  #ANSWER: for numerical values we can see quartiles & means analysis (such as in box plot), 
  #        Factorial column analysis counts the number of appearances for each value in the column

# 3.a. Calculate the percentage of rows with at least one missing value (NA)
################################
cat("Percentage of rows with at least one missing value: ",sum(!complete.cases(sample.data)) / nrow(sample.data))

# 3.b. Delete all rows with more than 1 missing value (NA)
################################
  sample.data <- sample.data[rowSums(is.na(sample.data))<2,]
  cat(nrow(sample.data)," rows left")

# 3.c. Create a histogram of a categorical column (to your choice). Explain the findings of the chart. 
# Pay attention that some histogram functions work only with numerical values, so a transformation is needed.
################################
  barplot(table(sample.data$payment_type))
  #ANSWER: most of the payment done by cash or credit card (99%). Most common paying method is cash. 
  #Any other payment method can consider as outlier


# 3.d. Choose and implement the best way to deal with the missing values in the dataset, in your opinion (according to your previous findings). 
# As for the columns: [trip_seconds, trip_miles, trip_total] , deal with 0's (zeros) as if they were NA's .
#Pay attention - you can decide to delete specific rows or columns, while impute some other remaining missing values. Explain all of your choices.
################################
  #ANSWER: first let's check if trip_seconds=0 -> trip_miles=0 (or NA). 
  #trip_seconds column
    #It seems that trip_seconds=0 -> trip_miles=0 (or NA). Also the droppoff area = pickup area.
    #Possible explanation is that the ride was canceled, instered by mistake, etc. These rides are not considered valid.
    #Therefore, we will remove them.
     sample.data <- sample.data[sample.data$trip_seconds!=0,]
  #trip_miles column ???
     #we assume than seconds and miles are linear correlated. Let's check this assumption
     data.pos.miles = sample.data[sample.data$trip_miles>0,]
     cat("number of rows with trip_miles>0: ",nrow(data.pos.miles))
     #build linear regression model, without the suspicious rows (with miles=0)
     sec.miles.lm <- lm(trip_miles~trip_seconds, data.pos.miles) 
     #show model anova results
     summary(sec.miles.lm)
     #model seems statistically significant, so we can use the linear regression in order to replace the zero values
     data.zero.miles <- unique(sample.data[sample.data$trip_miles==0,]['trip_seconds'])
     # The data to predict:
     predict.data <- sample.data[sample.data$trip_miles==0,]['trip_seconds']
     head(predict.data) #all positive seconds 
     #We will use the coeffs to predict new values - replace zero values (trip_miles==0) with predictions 
     #some values are negative (model isn't perfect), then we'll use max(prediction,0), so these values will be set to 0
     sample.data[sample.data$trip_miles==0,]['trip_miles'] <- apply(sample.data[sample.data$trip_miles==0,]['trip_seconds'], 1, function(x) max(coef(sec.miles.lm)[1] + x*coef(sec.miles.lm)[2],0))
     cat('Number of rows with trip_miles==0: ',nrow(sample.data[sample.data$trip_seconds==0,]))
   
  #trip_total
     sample.data[sample.data$trip_total==0,]  #only 4 rows -> we'll remove them 
     sample.data <- sample.data[sample.data$trip_total!=0,]
  
  #Company feature:
     sum(is.na(sample.data$company))
     #We can see that 40% of the data is NA in this feature, so we can handle it with few approches:
      #- keep it as NA--> maybe NA means "other company" which doesn't contained in our data ("off the book rides", or Uber).
      #- Fill it by uniform distribution--> create unifrom distribution to the feature and sampling from it for each missing value.
      #- By taxi_id--> looking at the drivers in the data and see to which campany s/he belongs.
     #In the real world, we'll ask our domain expert for advise. Back to our story, the data period is half month, so we'll fill the company feature by the taxi_id column and the unifrom distribution if needed.
      #the chance that taxi_id company has changed in this little period is low, so for all taxi_id with no company at all in the data we'll fill "other". 
     sample.data[is.na(sample.data$company),]['company'] <- apply(sample.data[is.na(sample.data$company),], 1, function(x) sample(sample.data$company[sample.data$taxi_id==x['taxi_id']],1))
     summary(sample.data$company)
     #30% is still missing--> set as "other"
     levels(sample.data$company) <- c(levels(sample.data$company), "other")
     sample.data$company[is.na(sample.data$company) == TRUE] <- 'other'
     summary(sample.data)
     
  #more features need to deal with NAs: taxi_id (2 NA's), pickup_community_area (11 NA's), dropoff_community_area (34 NA's)
     #since the ammount is low, we'll just remove these missing records
     for (feature in c('taxi_id', 'pickup_community_area', 'dropoff_community_area')){
       sample.data <- sample.data[!is.na(sample.data[feature]),]
     }
     summary(sample.data)
     
     
# 4.a. Make a Q-Q plot for each of the following columns: [trip_seconds, trip_miles, trip_total]. 
# Explain what we can learn from a Q-Q plot about the distribution of the data.
################################
  qqnorm(sample.data$trip_seconds)
  qqnorm(sample.data$trip_miles)
  qqnorm(sample.data$trip_total)
  #as it seems in the qqplots, all features are not(!) normal distributed

  
# 4.b. (7) According to the Q-Q plots ,do we need to normalize these features? Which normalization function should we use for each feature, if any?
# For each feature, in case you decided to normalize it, create a new normalized column of the feature (eg. norm.trip_seconds).
################################
  # All the feaures above are clearly not normal distributed. Their values spread on a broad range of positive values
  #Therefore we will use scaling, instead log transformation (or any other transformation used to make features to be normal distributed).
  sample.data$norm.trip_seconds <- scale(sample.data$trip_seconds,center=TRUE,scale=TRUE)
  sample.data$norm.trip_miles <- scale(sample.data$trip_miles,center=TRUE,scale=TRUE)
  sample.data$norm.trip_total <- scale(sample.data$trip_total,center=TRUE,scale=TRUE)

# 5.a. Create a boxplot of the normalized trip_miles column (or the original column in case you chose not to normalize) Remove the column's 
# outliers from the data based on the box plot. Hint: use the boxplot object.
################################
  #normalized column
  boxplot(sample.data$norm.trip_miles,data=sample.data,use.cols = TRUE, main="Trip miles data - normalized", ylab="Miles")
  outliers <- boxplot(sample.data$norm.trip_miles, plot=FALSE)$out
  outliersTF <- sample.data$norm.trip_miles %in% outliers
  sample.data$outlier <- outliersTF
  sample.data <- subset(sample.data, outlier!=TRUE)
  sample.data <- subset( sample.data, select = -outlier )
  #Thank to the outliers removal, we can see more 'clear' boxplot now
  boxplot(sample.data$trip_miles,data=sample.data,use.cols = TRUE, main="Trip miles data", ylab="Miles")

  
# 5.b. Implement a min-max transformation on the normalized columns of [trip_seconds, trip_miles, trip_total] 
# (or the original columns in case you chose not to normalize). 
# Create new column with the transformed data (eg. minmax.trip_seconds) 
################################
  for (feature in c('trip_seconds', 'trip_miles', 'trip_total')){
    name <- paste("minmax", toString(feature), sep=".")
    col_min <- min(sample.data[feature])
    col_max <- max(sample.data[feature])
    sample.data[name] <- apply(sample.data[feature], 1, function(x) (x-col_min)/(col_max-col_min))
  }
  #validate results
  summary(sample.data)

#5.c. Using the 3 columns you created, you will use a hierarchical-clustering method, followed by density-based method.  
#First, use hierarchical-clustering method to evaluate the probability of each instance to be an outlier. 
#Exclude all instances with 0.75 chance or higher. Hint: use "DMwR" package. 
#Then, using LOF, pick k=10 and remove all instances that their LOF score is above 1.4. Hint: use "Rlof" package.
  library(DMwR)
  featuresForhclust<-c("minmax.trip_seconds","minmax.trip_miles","minmax.trip_total")
  clusters <- hclust(dist(sample.data[,featuresForhclust]))
  plot(clusters)
  ## using hierarchical-clustering method to evaluate the probability of each instance to be an outlier
  o <- outliers.ranking(data=sample.data[,featuresForhclust],test.data=NULL,
                        clus=list(dist='euclidean',alg='hclust',meth='average'))
  # The outlyingness factor of the instances
  o$prob.outliers
  # remove instances with prob>=0.75
  hclust_outliers <- o$prob.outliers >= 0.75
  sample.data <- sample.data[hclust_outliers==FALSE,]
  summary(sample.data)
  
  ##using LOF, pick k=10 and remove all instances that their LOF score is above 1.4. 
  featuresForLOF<-c("minmax.trip_seconds","minmax.trip_miles","minmax.trip_total")
  lof.scores<-lofactor(sample.data[,featuresForLOF], k=10)
  Lof_outliers <- lof.scores > 1.4
  sample.data <- sample.data[Lof_outliers==FALSE,]
  summary(sample.data) 
  
#6.a. Create a correlation matrix of all the relevant numerical features. In addition, Display a correlation plot for this matrix. 
# Write 3 business insights we can learn from the correlation matrix.
################################
  #we remove the 'tolls' column since it is a fixed column (=0 for all rows) because of the outliers removal in 5.c.
  cor(sample.data[,c('minmax.trip_seconds','minmax.trip_miles','fare','tips','extras','minmax.trip_total')], use="complete.obs", method="pearson")  #all.obs since we don't have any missing values
  library(corrgram)
  corrgram(sample.data[,c('minmax.trip_seconds','minmax.trip_miles','fare','tips','extras','minmax.trip_total')], #using blue color map
           order=NULL, lower.panel=panel.shade,
           upper.panel=panel.pts, text.panel=panel.txt,
           main="Correlation matrix")
  #Insights: 
    #1. Time (seconds) and distance are correlated with the trip_total. It may suggest about the billing method - combination of distance and time. 
        #We bare in mind that some of the values of trip_miles were calculated using linear regression, therefore, this finding may be less surprising.
    #2. The fare is the most significant part in the total price of the ride. The tips and extra are less correlated.
    #3. The trip_seconds and the trip's fare are quite correlated. It may suggest that in rush hours/ heavy traffic hours, the fare is higher. It may also explain insight #1 and #2
    #4. Tips are quite correlated with the trip_total, as one may expected. However, the correlation is medium, suggesting that people pay smaller tip when the total price get higher.
    #5. Tips are not correlated (significally at least) with time and/or distance. That means that longer drives are not neccesarily increase the tip (even though the driver and the passenger interacting for a longer period of time)


#6.b. Create 5 different statistical outputs based on the dataset. Visualize at least 3 of them. Add an explanation. Try to be creative.
#Examples:
#  1.	A bar chart that displays the average and median amount of trip_total, for each payment_type. 
#  2. Density plots of trip_second - one for each day.
################################

#1. The 5 companies with the most drivers 
  library("dplyr")
  #unique table of drivers and companies
  df1 <- distinct(sample.data[,c("taxi_id", "company")])
  #count number of drivers
  df1$taxi_id <- 1
  df2 <- setNames(aggregate(df1$taxi_id ~ df1$company, FUN=sum), c("Company", "Num.Drivers"))
  #sort
  attach(df2)
  df2 <- df2[order(Num.Drivers, decreasing = TRUE),]
  #show 5 top companies in number of drivers
  df2[1:5,]
  detach(df2)

  
  #2. fare vs hours in day
  library(ggplot2)
  library(lubridate)
  theme_set(theme_bw())
  #convert time feature to timestamp
  df <- sample.data[, c("trip_start_timestamp", "fare")]
  df['trip_start_timestamp'] <- lapply(df['trip_start_timestamp'], as.character)
  df$date <- strptime(df$trip_start_timestamp, format = "%m/%d/%Y %H:%M")
  #extract hour and day features
  df$hour <- hour(df$date)
  df$day <- weekdays(df$date)
  #build aggregation
  df2 <- setNames(aggregate(df$fare ~ df$hour, FUN=mean), c("hours", "Mean.fare"))
  #plot
  ggplot(df2, aes(hours, Mean.fare)) + geom_col(aes(group=df2$Mean.fare, fill=df2$Mean.fare)) + theme(legend.title = element_blank()) + ggtitle('Mean fare by the hour')
  
  
#3. tips ratio (in % from the trip_total) distribution vs payment type -> we can learn which payment type holds higher tips (good for drivers :])
  #build the tip ratio
  sample.data$tip_rate <- apply(sample.data[,c('tips','trip_total')], 1, function(x) x[1]/x[2])
  #calculate mean tip_rate for each payment_type
  df3 <- setNames(aggregate(sample.data$tip_rate ~ sample.data$payment_type, FUN=mean), c("payment_type", "tip_rate"))
  #remove irrelevant payment methods
  df3 <- subset(df3, payment_type %in% c("Prcard","Credit Card","Cash", "Unknown"))
  #plot
  ggplot(df3, aes(payment_type, tip_rate)) + geom_col(aes(group=df3$tip_rate, fill=df3$payment_type)) + theme(legend.title = element_blank()) + ggtitle('Tip rate by payment type')
  
  
#4. tips ratio (in % from the trip_total) distribution vs time in day (morning, noon, evening, night)
  df$tip_rate <- apply(sample.data[,c('tips','trip_total')], 1, function(x) x[1]/x[2])
  levels(df$hour) <- c(levels(df$hour), "Night", "Evening", "Morning", "After Noon")
  df$hour.range[df$hour >= 21 & df$hour <= 24] <- 'Night'
  df$hour.range[df$hour >= 0 & df$hour <= 6] <- 'Night'
  df$hour.range[df$hour >= 7 & df$hour <= 11] <- 'Morning' 
  df$hour.range[df$hour >= 12 & df$hour <= 16] <- 'After Noon' 
  df$hour.range[df$hour >= 17 & df$hour <= 21] <- 'Evening' 
  head(df)
  df4 <- setNames(aggregate(df$tip_rate ~ df$hour.range, FUN=mean), c("time_of_day", "tip_rate"))
  ggplot(df4, aes(time_of_day, tip_rate)) + geom_col(aes(group=df4$tip_rate, fill=df4$time_of_day)) + theme(legend.title = element_blank()) + ggtitle('Tip rate by part of day')
  
  

#5 visualize the contact mixing pattern (pickup vs dropoff), using area codes
  library(plyr)
  counts <- ddply(sample.data, .(sample.data$pickup_community_area, sample.data$dropoff_community_area), nrow)
  names(counts) <- c("pickup_community_area", "dropoff_community_area", "Freq")
  attach(counts)
  df <- counts[order(Freq, decreasing = TRUE),]
  detach(counts)
  #10 most common travels
  head(df, n=10)
  
  #plot all areas
  l <- ggplot(counts, aes(pickup_community_area, dropoff_community_area))
  l + geom_tile(aes(fill = counts$Freq)) + theme(legend.title = element_blank()) + ggtitle('Contact mixing pattern Heatmap')
  
  #most of the information seems in areas 1-10
  #->create matrix and heatmap (+ dendrogram)
  attach(counts)
  df <- counts[order(pickup_community_area, dropoff_community_area),]
  detach(counts)
  counts.mat <- matrix(0, 77, 77)
  #iterate over the records in the frequency data frame and edit the correspondent values in the matrix 
  for(i in 1:nrow(counts)) {
    row <- counts[i,1]
    col <- counts[i,2]
    val <- counts[i,3]
    counts.mat[row,col] <- val
  }
  counts.mat2 <- counts.mat[1:10,1:10]
  heatmap(counts.mat2, main="Contact mixing pattern heatmap", xlab = "pickup area", ylab="dropoff area", keep.dendro = FALSE)

  ##Insights from 6b:
    #1. Most of the drivers belonging to comapnies which aren't exist in our data (probably 3-4 companies). beside it, '107' is the bigger company.
    #2. The average fare per an hour is around 8, and the higher fare is taken in the early morning (4-6).
    #3. Most customers are paying the fare in cash (65%), it's rare to see a tip by cash payment.
      #tip in Prcard and credit card is the prefer way as we can see.
    #4. The average tip rate is around 5.2%, and there is no a big difference in the part of the day. at evening we can see a little decrease in the rate.
    #5. We can see a lot of movement in the 8 area, but also a high movement from 8 to 6/7.
  