library(ggplot2)

#BASIC SUMMARY STATISTICS FOR COFFEE RATINGS

#I added code to read in the csv file. 
Project1_coffee = read.csv("Project1_coffee.csv")

#I also think this should be an R markdown file and not an R file. 

# Calculate the mean rating
mean(Project1_coffee$Rating, na.rm=TRUE)
# Calculate the median rating
median(Project1_coffee$Rating, na.rm=TRUE)
# Find the minimum rating
min(Project1_coffee$Rating, na.rm=TRUE)
# Find the maximum rating
max(Project1_coffee$Rating, na.rm=TRUE)
# These four functions summarize the distribution of the Coffee Rating variable

#ALTITUDE ANALYSIS
# Calculate the mean, median, minimum, and maximum altitude
mean(Project1_coffee$Altitude, na.rm=TRUE)
median(Project1_coffee$Altitude, na.rm=TRUE)
min(Project1_coffee$Altitude, na.rm=TRUE)
max(Project1_coffee$Altitude, na.rm=TRUE)

# Check the correlation between altitude and coffee rating
cor(Project1_coffee$Altitude, Project1_coffee$Rating, use="complete.obs")
# This shows whether higher altitudes are associated with higher (or lower) ratings

# Run a simple linear regression to predict Rating from Altitude
lm(Rating ~ Altitude, data=Project1_coffee)
# This model tests whether altitude significantly predicts rating

#CREATE ALTITUDE GROUPS

# Classify coffees into altitude ranges: Low, Medium, or High
Project1_coffee$AltitudeGroup = ifelse(Project1_coffee$Altitude < 800, "Low (<800m)",ifelse(Project1_coffee$Altitude < 1500, "Medium (800-1499m)", "High (1500m+)"))

# Convert the altitude group variable into a factor and specify order of categories
Project1_coffee$AltitudeGroup = factor(Project1_coffee$AltitudeGroup,levels=c("Low (<800m)", "Medium (800-1499m)", "High (1500m+)"))

# Create a boxplot comparing coffee ratings by altitude group
ggplot(Project1_coffee, aes(x=AltitudeGroup, y=Rating, fill=AltitudeGroup)) + geom_boxplot() + labs(title="Coffee Ratings by Altitude Group", x="Altitude Group", y="Rating")
# This visualizes how ratings differ across altitude categories

#PROCESSING METHOD ANALYSIS

# Count how many coffees use each processing method
table(Project1_coffee$ProcessingMethod)

# Compute average rating for each processing method
aggregate(Rating ~ ProcessingMethod, data=Project1_coffee, mean, na.rm=TRUE)

# Create a boxplot showing rating distribution by processing method
ggplot(Project1_coffee, aes(x=ProcessingMethod, y=Rating, fill=ProcessingMethod)) + geom_boxplot() + labs(title="Ratings by Processing Method", x="Processing Method", y="Rating")

#PRODUCTION SIZE ANALYSIS (BAGS PRODUCED)

# Calculate summary statistics for BagsProduced
mean(Project1_coffee$BagsProduced, na.rm=TRUE)
median(Project1_coffee$BagsProduced, na.rm=TRUE)
min(Project1_coffee$BagsProduced, na.rm=TRUE)
max(Project1_coffee$BagsProduced, na.rm=TRUE)

# Check correlation between number of bags produced and coffee rating
cor(Project1_coffee$BagsProduced, Project1_coffee$Rating, use="complete.obs")
# This indicates whether production size affects quality (ratings)

# Run a linear model predicting rating from bag production size
lm(Rating ~ BagsProduced, data=Project1_coffee)

