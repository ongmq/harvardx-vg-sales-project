# Project Submission: Video Game Sales

# Section 1: Introduction
# This project is part of the final course of the HarvardX Data Science Professional Certificate program.
# The aim is to create a machine learning algorithm to predict video game sales using the Kaggle Video Games Sales dataset.
# For each video game released, the dataset contains information on the platform (console on which the game is running), genre (game's category), ESRB ratings, and year of release.
# The dataset also contains information on the critic score (aggregate score compiled by Metacritic) and user score (score by Metacritic's subscribers), as well as the publisher and developer (partly responsible for creating the game).
# We will use these variables to predict the global sales of video games, which are measured in million of units.

# Section 1.1: Loading R packages
# We start by loading all the required R packages, , namely the tidyverse, caret, and randomForest packages.
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(randomForest)

# Section 1.2: Importing & cleaning dataset
# We then read the Kaggle Video Game Sales dataset which we uploaded on GitHub.
# The csv file contains a lot of blank, "NA", and "N/A" entries. We will use the na.strings argument to convert all those entries into na.
url <- "https://raw.githubusercontent.com/ongmq/harvardx-vg-sales-project/main/Video_Games_Sales_as_at_22_Dec_2016.csv"
vg_sales <- read.csv(url, na.strings = c("", " ", "NA", "N/A"))
rm(url)

# We will remove all the na entries in the dataset.
vg_sales <- na.omit(vg_sales)

# We now see that our dataset does not contain any na entry.
colSums(is.na(vg_sales))

# We will start to inspect our dataset.
# After removing all the na entries, we are left with 6825 observations of 16 variables.
str(vg_sales)

# We note that the user score variable is stored as a character variable instead of numeric. We will change its type and make it the same scale as the critic score variable.
vg_sales$User_Score <- as.numeric(vg_sales$User_Score)
vg_sales$User_Score <- vg_sales$User_Score * 10

# We now have all the variables in the correct type.
str(vg_sales)

# We will continue to inspect and clean up our dataset.
# We see that there is only 1 entry for some rating categories. We will merge these with the other rating categories. 
vg_sales %>% count(Rating)

# We will merge the "adult only" category with the "mature" category.
vg_sales <- vg_sales %>%
  mutate(Rating = ifelse(Rating == "AO", "M", Rating))
# We will merge the "kids to adults" category with the "everyone" category.
vg_sales <- vg_sales %>%
  mutate(Rating = ifelse(Rating == "K-A", "E", Rating))
# We will merge the "rating pending" category with the "everyone 10+" category.
vg_sales <- vg_sales %>%
  mutate(Rating = ifelse(Rating == "RP", "E10+", Rating))
# We now have only 4 unique rating categories.
unique(vg_sales$Rating)

# We also notice that the platform variable has many categories, and could be regrouped into 5 main ones.
vg_sales <- vg_sales %>%
  mutate(Platform = case_when(
    Platform %in% c("Wii", "WiiU", "DS", "3DS", "GC", "GBA") ~ "Nintendo",
    Platform %in% c("X360", "XB", "XOne") ~ "XBox",
    Platform %in% c("PS", "PS2", "PS3", "PS4", "PSP", "PSV") ~ "PS",
    Platform == "PC" ~ "PC",
    Platform == "DC" ~ "Sega"
  ))
unique(vg_sales$Platform)

# Here is a snapshot of our dataset after cleaning up.
head(vg_sales)

# Section 2: Analysis
# We will now explore the cleaned Kaggle Video Game Sales dataset.

# Section 2.1: Distribution of video games
# We start by exploring the distribution of video games released across the key variables (i) platform, (ii) genre, (iii) rating, (iv) year of release, (v) critic score, (vi) user score, (vii) publisher, and (viii) developer.

# (i) Platform: Most of the games released are PS games, followed by Nintendo, XBox, PC and lastly Sega.
vg_sales %>% 
  group_by(Platform) %>%
  count() %>%
  ggplot(aes(Platform, n)) +
  geom_bar(stat = "identity") +
  xlab("platform") +
  ylab("no. of games")

# (ii) Genre: The number of games released differ greatly across genres, with action games being the most popular and puzzle games being the least popular.
vg_sales %>% 
  group_by(Genre) %>%
  count() %>%
  ggplot(aes(Genre, n)) +
  geom_bar(stat = "identity") +
  xlab("genre") +
  ylab("no. of games") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# (iii) Rating: Most of the games released are rated "Teen", following by "Everyone", "Mature", and lastly "Everyone 10+".
vg_sales %>% 
  group_by(Rating) %>%
  count() %>%
  ggplot(aes(Rating, n)) +
  geom_bar(stat = "identity") +
  xlab("rating") +
  ylab("no. of games")

# (iv) Year of release: Bulk of the games were released during 2000-2008. Beyond that, there is a steady decline in the number of games released each year.
vg_sales %>% 
  group_by(Year_of_Release) %>%
  count() %>%
  ggplot(aes(Year_of_Release, n)) +
  geom_line() +
  xlab("year of release") +
  ylab("no. of games")

# (v) Critic score & (vi) User score: Both critic score and user score have similar distributions, with user score having a slightly higher mean and median than critic score.
vg_sales %>% 
  ggplot(aes(Critic_Score)) +
  geom_histogram(binwidth = 5, col = "black") +
  xlab("critic score") +
  ylab("no. of games")
summary(vg_sales$Critic_Score)

vg_sales %>% 
  ggplot(aes(User_Score)) +
  geom_histogram(binwidth = 5, col = "black") +
  xlab("user score") +
  ylab("no. of games")
summary(vg_sales$User_Score)

# (vii) Publisher & (viii) Developer: There are many different game publishers and developers. Below are the top 10 publishers and developers based on the number of games released.
vg_sales %>% 
  group_by(Publisher) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(Publisher, n)) +
  geom_bar(stat = "identity") +
  xlab("publisher") +
  ylab("no. of games") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

vg_sales %>% 
  group_by(Developer) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(10) %>%
  ggplot(aes(Developer, n)) +
  geom_bar(stat = "identity") +
  xlab("developer") +
  ylab("no. of games") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Section 2.2: Distribution of global sales
# We now zoom in to global sales, which is what we want to predict using our algorithm.
# We can see that global sales differ significantly across games. (Note: We are using log scale for the x-axis as the variable is highly skewed.)
vg_sales %>% 
  ggplot(aes(Global_Sales)) +
  geom_histogram(bins = 30, col = "black") +
  scale_x_log10() +
  xlab("global sales") +
  ylab("no. of games")

# (i) Global sales by platform: Global sales differ significantly across platform, with PS games having the highest sales, followed by Nintendo, Xbox, PC, and lastly Sega.
vg_sales %>%
  mutate(Platform = reorder(Platform, Global_Sales, FUN = sum)) %>%
  ggplot(aes(Platform, Global_Sales)) +
  geom_bar(stat = "identity") +
  xlab("platform") +
  ylab("global sales")

# (ii) Global sales by genre: Global sales also vary significantly across genre, with action games having the highest sales, followed by sports etc., and strategy games having the least sales.
vg_sales %>%
  mutate(Genre = reorder(Genre, Global_Sales, FUN = sum)) %>%
  ggplot(aes(Genre, Global_Sales)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("genre") +
  ylab("global sales")

# (iii) Global sales by rating: Global sales seem to be affected by rating category as well, with games rated "Everyone" having the highest sales, followed by "Mature", "Teen", and "Everyone 10+".
vg_sales %>%
  mutate(Rating = reorder(Rating, Global_Sales, FUN = sum)) %>%
  ggplot(aes(Rating, Global_Sales)) +
  geom_bar(stat = "identity") +
  xlab("rating") +
  ylab("global sales")

# (iv) Global sales by year of release: Global sales of video games took off since 2000, and started seeing a steady decline since 2008.
vg_sales %>%
  ggplot(aes(Year_of_Release, Global_Sales)) +
  geom_bar(stat = "identity") +
  xlab("year of release") +
  ylab("global sales")

# (v) Global sales by critic score & (vi) user score: Global sales appear to be determined by critic score and user score, with games having a score of above 70 accounting for bulk of the sales.
vg_sales %>%
  ggplot(aes(Critic_Score, Global_Sales)) +
  geom_bar(stat = "identity") +
  xlab("critic score") +
  ylab("global sales")

vg_sales %>%
  ggplot(aes(User_Score, Global_Sales)) +
  geom_bar(stat = "identity") +
  xlab("user score") +
  ylab("global sales")

# (vii) Global sales by publisher & (viii) developer: Global sales vary significantly across publisher and developer. Below are the global sales of the top 10 publishers and developers.
vg_sales %>%
  group_by(Publisher) %>%
  summarise(sales = sum(Global_Sales)) %>%
  arrange(desc(sales)) %>%
  head(10) %>%
  ggplot(aes(reorder(Publisher, sales), sales)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("publisher") +
  ylab("global sales")

vg_sales %>%
  group_by(Developer) %>%
  summarise(sales = sum(Global_Sales)) %>%
  arrange(desc(sales)) %>%
  head(10) %>%
  ggplot(aes(reorder(Developer, sales), sales)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("developer") +
  ylab("global sales")

# Section 3: Results
# We will now build our machine learning algorithms.
# Our analysis in Section 2 suggests that global sales is affected by the variables (i) platform, (ii) genre, (iii) rating, (iv) year of release, (v) critic score, (vi) user score, (vii) publisher, and (viii) developer.

# Section 3.1: Splitting the Kaggle Video Game Sales dataset
# We start by splitting our dataset into (i) validation, (ii) training, and (iii) testing sets.
# The validation set will be 10% of the data. 
# This is the final hold-out set that we will use to evaluate our final algorithm. It will not be used for developing our algorithms. 
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(vg_sales$Global_Sales, p = 0.1, list = F)
temp <- vg_sales[test_index,]
remainder <- vg_sales[-test_index,]

# We need to make sure all possible values of the categorical variables in the validation set are also in the remainder set.
length(unique(vg_sales$Platform))
length(unique(vg_sales$Genre))
length(unique(vg_sales$Rating))
length(unique(vg_sales$Publisher))
length(unique(vg_sales$Developer))

# In particular, we need to make sure the publishers and developers are in both sets, as these two variables have many unique categories.
validation_set <- temp %>% 
  semi_join(remainder, by = "Publisher") %>%
  semi_join(remainder, by = "Developer")

# We then add the rows removed from the validation set back into the remainder set.
removed <- anti_join(temp, validation_set)
remainder <- rbind(remainder, removed)

# We now repeat the above process to split the remainder set into training and testing sets.
# Likewise, the test set will be 10% of the remainder data.
set.seed(1, sample.kind = "Rounding")
test_index <- createDataPartition(remainder$Global_Sales, p = 0.1, list = F)
temp <- remainder[test_index,]
train_set <- remainder[-test_index,]

test_set <- temp %>% 
  semi_join(train_set, by = "Publisher") %>%
  semi_join(train_set, by = "Developer")

removed <- anti_join(temp, test_set)
train_set <- rbind(train_set, removed)

# We now remove the datasets that we no longer need.
rm(test_index, remainder, temp, removed)

# We are left with the following 3 datasets to train, test, and validate our algorithms.
str(train_set)
str(test_set)
str(validation_set)

# Section 3.2: Linear regression
# We then build a linear regression model, predicting global sales based on (i) platform, (ii) genre, (iii) rating, (iv) year of release, (v) critic score, (vi) user score, (vii) publisher, and (viii) developer.
# Note: Remember to take the log of global sales, as discussed in Section 2.2.
# The performance of the algorithm will be determined by its root mean square error (RMSE), with smaller RMSE indicating better performance.
model_lm <- train(log(Global_Sales) ~ Platform + Genre +
                    Rating + Year_of_Release +
                    Critic_Score + User_Score +
                    Publisher + Developer,
                  method = "lm",
                  data = train_set)
model_lm

# The linear regression model has a RMSE of 1.13.
y_hat_lm <- predict(model_lm, test_set)
rmse_results <- data.frame(Method = "Linear regression", 
                           RMSE = RMSE(log(test_set$Global_Sales), y_hat_lm))
rmse_results

# Section 3.3: k-nearest neighbour (knn)
# Next, we build a knn model with cross validation to see if we can improve our predictions.
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 10, p = 0.9)
model_knn <- train(log(Global_Sales) ~ Platform + Genre +
                     Rating + Year_of_Release +
                     Critic_Score + User_Score +
                     Publisher + Developer,
                   method = "knn",
                   trControl = control,
                   tuneGrid = data.frame(k = seq(3, 31, 4)),
                   data = train_set)
model_knn

# The predict function will use the best-performing model.
# The knn model has a RMSE higher than linear regression model, suggesting that knn might not be very suitable for our dataset.
model_knn$bestTune
y_hat_knn <- predict(model_knn, test_set)
rmse_results <- bind_rows(rmse_results,
                          data.frame(Method = "knn",
                                     RMSE = RMSE(log(test_set$Global_Sales), y_hat_knn)))
rmse_results

# Section 3.4: Random forest
# Finally, we build a random forest model with cross validation to see if we can improve our predictions.
set.seed(1, sample.kind = "Rounding")
control <- trainControl(method = "cv", number = 5)
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
model_rf <- train(log(Global_Sales) ~ Platform + Genre +
                    Rating + Year_of_Release +
                    Critic_Score + User_Score +
                    Publisher + Developer,
                  method = "rf",
                  trControl = control,
                  tuneGrid = grid,
                  data = train_set)
model_rf

# The predict function will use the best-performing model.
# The random forest model has a RMSE much smaller than the linear regression and knn models.
model_rf$bestTune
y_hat_rf <- predict(model_rf, test_set)
rmse_results <- bind_rows(rmse_results,
                          data.frame(Method = "Random forest",
                                     RMSE = RMSE(log(test_set$Global_Sales), y_hat_rf)))
rmse_results

# Section 3.5: Testing the final model using the validation set
# Our above results show that the random forest model has the best performance (i.e., the smallest RMSE).
# We will now test the model using the validation set, where we obtain a RMSE of around 0.92.
y_hat_final <- predict(model_rf, validation_set)
RMSE(log(validation_set$Global_Sales), y_hat_final)

# Section 4: Conclusion
# We have successfully trained a machine learning algorithm to predict video game sales, using (i) platform, (ii) genre, (iii) rating, (iv) year of release, (v) critic score, (vi) user score, (vii) publisher, and (viii) developer as the predictors.
# Our final model is random forest with cross validation, which performed better than linear regression and knn with cross validation.
# However, this model used the whole range of critic score and user score in its predictions, as well as the whole list of publishers and developers.
# It could be that higher critic score and user score, and top publishers and developers, have greater effect on video game sales.
# Future work can dive deeper into these aspects to refine the model.
# Future work can also look into the relative importance of each predictor to improve the model further.

# Section 5: References
# Being new to R and machine learning, I have consulted the following materials when doing up this video game sales project.
# https://rafalab.github.io/dsbook/index.html
# https://www.kaggle.com/datasets/rush4ratio/video-game-sales-with-ratings
# https://www.kaggle.com/code/yonatanrabinovich/video-games-sales-regression-techniques?scriptVersionId=55716523
# https://www.kaggle.com/code/najibmozahem/video-game-sales-and-reviews-with-r 