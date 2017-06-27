# kaggle-animal-shelter-outcome-prediction.R - R code to create an outcome prediction for animal shelter data

# R packages needed - to be installed using Tools -> Install Packages from RStudio menu 

library(ggplot2) # visualization
library(ggthemes) # visualization
library(dplyr) # data manipulation
library(lubridate) # dates
library(rpart) # rpart for imputation
library(mice) # mice for imputation
library(randomForest) # classification algorithm

library(plyr)    # data processing
library(dplyr)   # data processing
library(stringr) # text/string processing
library(caret) # predictive analytics / classification algorithms

#set the working directory where your code and data is located
setwd("C:/TEMP/DATA/kaggle-animal-shelter-outcomes/DATA")

# Read the data
#26730 lines: 1 header + 26729 data entries with target vars OutcomeType (plus optionally OutcomeSubtype)
train <- read.csv('train.csv', stringsAsFactors = F)
#11457 lines: 1 header + 11456 data entries, without target vars columns
test <- read.csv('test.csv', stringsAsFactors = F)

cat("train.csv data set contains :", length(train), "columns:", names(train), "\n")
str(train)
cat("test.csv data set contains :", length(test), "columns:", names(test), "\n")
str(test)

# change ID in test to character
test$ID <- as.character(test$ID)

# Combine test & training data
full_animal_data_set <- bind_rows(train, test)

#profile data
table(full_animal_data_set$OutcomeType)
barplot(table(full_animal_data_set$OutcomeType))

table(full_animal_data_set$OutcomeSubtype)
barplot(table(full_animal_data_set$OutcomeSubtype))

full_animal_data_set$species <- as.factor(full_animal_data_set$AnimalType)

#see how outcomes are distributed depending on pet type: cats and dogs
cat("Total pets in the shelter", nrow(full_animal_data_set), ", of which: ", nrow(full_animal_data_set[full_animal_data_set$species == 'Cat', ]), " cats and ", nrow(full_animal_data_set[full_animal_data_set$species == 'Dog', ]), " dogs\n")

#outcomes <- full_animal_data_set[1:26729, ] %>%
#  group_by(species, OutcomeType) %>%
#  summarise(num_animals=n())

# Plot
# ggplot(outcomes, aes(x = full_animal_data_set$species, y = num_animals, fill = full_animal_data_set$OutcomeType)) +
#   geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
#   coord_flip() +
#   labs(y = 'Percentage animals', 
#   x = 'Pet type',
#   title = 'Outcomes per pet type') +
#   theme_few()

#cats and dogs are commonly adopted or transferred (cats more often than dogs)
#dogs are much more likely to be returned to their owners than cats
#overall, very few animals die or get euthanasia, cats more often than dogs

#list the number of factors for specific variables, e.g. AgeuponOutcome
cat("Nr levels for AgeuponOutcome: ", nlevels(factor(full_animal_data_set$AgeuponOutcome)),":\n")
factor(full_animal_data_set$AgeuponOutcome)[1:nlevels(factor(full_animal_data_set$AgeuponOutcome))]

#transform AgeuponOutcome into a uniform age in days by breaking up into two numeric value and unit of time 
full_animal_data_set$TimeValue <- sapply(full_animal_data_set$AgeuponOutcome, function(x) strsplit(x, split = ' ')[[1]][1])

# pre-process unit of time:
full_animal_data_set$UnitofTime <- sapply(full_animal_data_set$AgeuponOutcome,  function(x) strsplit(x, split = ' ')[[1]][2])
full_animal_data_set$UnitofTime <- gsub('s', '', full_animal_data_set$UnitofTime)

full_animal_data_set$TimeValue  <- as.numeric(full_animal_data_set$TimeValue)
full_animal_data_set$UnitofTime <- as.factor(full_animal_data_set$UnitofTime)

#function to use as multiplier for time units to calculate pet ages in days
multiplier <- ifelse(full_animal_data_set$UnitofTime == 'day', 1,
                     ifelse(full_animal_data_set$UnitofTime == 'week', 7,
                            ifelse(full_animal_data_set$UnitofTime == 'month', 30, # approximation
                                   ifelse(full_animal_data_set$UnitofTime == 'year', 365, NA))))

# Apply our multiplier
full_animal_data_set$age_in_days <- full_animal_data_set$TimeValue * multiplier

summary(full_animal_data_set$age_in_days)
#plot distribution of age_in_days
boxplot(full_animal_data_set$age_in_days)

# Replace blank names with "Nameless"
full_animal_data_set$Name <- ifelse(nchar(full_animal_data_set$Name)==0, 'Nameless', full_animal_data_set$Name)

# Make a variable has_given_name if pet was nameless (lost?)
full_animal_data_set$has_given_name[full_animal_data_set$Name == 'Nameless'] <- 0
full_animal_data_set$has_given_name[full_animal_data_set$Name != 'Nameless'] <- 1

# Replace blank sex with most common
full_animal_data_set$SexuponOutcome <- ifelse(nchar(full_animal_data_set$SexuponOutcome)==0, 'Spayed Female', full_animal_data_set$SexuponOutcome)

# Extract time variables from date (uses the "lubridate" package)
#library(lubridate)

full_animal_data_set$DateTime <- as.POSIXct(full_animal_data_set$DateTime)
full_animal_data_set$Hour    <- hour(full_animal_data_set$DateTime)
full_animal_data_set$Weekday <- wday(full_animal_data_set$DateTime)
full_animal_data_set$Month   <- month(full_animal_data_set$DateTime)
full_animal_data_set$Year    <- year(full_animal_data_set$DateTime)

# Time of day
full_animal_data_set$TimeofDay <- ifelse(full_animal_data_set$Hour > 5 & full_animal_data_set$Hour < 11, 'morning',
                         ifelse(full_animal_data_set$Hour > 10 & full_animal_data_set$Hour < 16, 'midday',
                                ifelse(full_animal_data_set$Hour > 15 & full_animal_data_set$Hour < 20, 'lateday', 'night')))

# Factor TimeOfDay
full_animal_data_set$TimeofDay <- factor(full_animal_data_set$TimeofDay, 
                         levels = c('morning', 'midday',
                                    'lateday', 'night'))


#check if TimeofDay influences outcome. 
# Reshape
#daytimes <- full_animal_data_set[1:26729, ] %>%
#   group_by(species, TimeofDay, OutcomeType) %>%
#   summarise(num_animals = n())
 
# Plot daytimes
#ggplot(daytimes, aes(x = TimeofDay, y = num_animals, fill = OutcomeType)) + geom_bar(stat = 'identity', position = 'fill', colour = 'black') + facet_wrap(~species) + coord_flip() + labs(y = 'Percentage of Animals',  x = 'Animal', title = 'Outcomes by Time of Day: Cats & Dogs') + theme_few()

#from the plot, it can be derived that dogs are euthanasiated in the night and morning, adopted in the afternoon 


#Breed variable has too many levels, reducing the mixed ones
cat("Initial number of animal breeds is ", nlevels(factor(full_animal_data_set$Breed)),", of which: ",length(unique(full_animal_data_set[full_animal_data_set$AnimalType=="Dog",]$Breed))," dog breeds and ",length(unique(full_animal_data_set[full_animal_data_set$AnimalType=="Cat",]$Breed))," cat breeds\n")

# Take a look as some of the levels
levels(factor(full_animal_data_set$Breed))[1:10]

# Split on "/" and remove " Mix" to simplify Breed
is_mixed_breed <- function(data) {
  as.factor(unlist(lapply(tolower(data$Breed),
                          function(x) {
                            ifelse(grepl("mix",x),"mutt",
                                   ifelse(grepl("/",x),"dual","pure"))
                          }
  )
  )
  )
}

#full_animal_data_set$IsMixBreed <- ifelse(grepl('Mix', full_animal_data_set$Breed), 1, 0)
full_animal_data_set$IsMixBreed <- is_mixed_breed(full_animal_data_set)

full_animal_data_set$SimplifiedBreed <- sapply(full_animal_data_set$Breed, 
                           function(x) gsub(' Mix', '', strsplit(x, split = '/')[[1]][1]))

cat("Reduced number of animal breeds is ", nlevels(factor(full_animal_data_set$SimplifiedBreed)),"\n")

# Get simplified_colour from animal Color description
full_animal_data_set$simplified_colour <- sapply(full_animal_data_set$Color, function(x) strsplit(x, split = '/| ')[[1]][1])

cat("Animal descriptions contains ", nlevels(factor(full_animal_data_set$simplified_colour))," colours:", levels(factor(full_animal_data_set$simplified_colour)),"\n")

#create feature about neutering/castration, based on presence of word "Intact"
full_animal_data_set$is_not_castrated <- ifelse(grepl('Intact', full_animal_data_set$SexuponOutcome), 1, ifelse(grepl('Unknown', full_animal_data_set$SexuponOutcome), 'Unknown', 0))

# Cleanup information about animal gender
full_animal_data_set$Sex <- ifelse(grepl('Male', full_animal_data_set$SexuponOutcome), 'Male', ifelse(grepl('Unknown', full_animal_data_set$Sex), 'Unknown', 'Female'))


#Check if castration is a significant factor in determining shelter animal outcome
is_not_castrated <- full_animal_data_set[1:26729, ] %>%
  group_by(species, is_not_castrated, OutcomeType) %>%
  summarise(num_animals = n())

# Plot
ggplot(is_not_castrated, aes(x = is_not_castrated, y = num_animals, fill = OutcomeType)) +
  geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
  facet_wrap(~species) +
  coord_flip() +
  labs(y = 'Percentage of Animals', 
       x = 'Animal',
       title = 'Outcomes by Intactness: Cats & Dogs') +
  theme_few()


#age_in_days has 24 missing values
ageless <- which(full_animal_data_set$AgeuponOutcome == "")
length(ageless)

#Alternatives: 1. drop data entries with missing age; OR 2. assign values; OR 3. fill in with pre-defined unknown value; selected option 3
#option 1
#full_animal_data_set <- full_animal_data_set[-ageless,]

# option2: Use rpart to impute predicted age values where missing
#age_fit <- rpart(age_in_days ~ species + Sex + is_not_castrated + SimplifiedBreed + has_given_name, 
#                 data = full_animal_data_set[!is.na(full_animal_data_set$age_in_days), ], 
#                 method = 'anova')
#full_animal_data_set$age_in_days[is.na(full_animal_data_set$age_in_days)] <- predict(age_fit, full_animal_data_set[is.na(full_animal_data_set$age_in_days), ])

#option 3
full_animal_data_set$age_in_days[is.na(full_animal_data_set$age_in_days)] <- -1

#check that missing values for age_in_days were imputed
sum(is.na(full_animal_data_set$age_in_days))

# test whether age makes a distinction between young/baby pets and adult ones
full_animal_data_set$is_adult[full_animal_data_set$age_in_days < 365] <- 'baby'
full_animal_data_set$is_adult[full_animal_data_set$age_in_days >= 365] <- 'adult'

full_animal_data_set$is_adult <- factor(full_animal_data_set$is_adult)

#Plot the relationship between pet is_adult andoutcome
ggplot(full_animal_data_set[1:26729, ], aes(x = is_adult, fill = OutcomeType)) + 
  geom_bar(position = 'fill', colour = 'black') +
  labs(y = 'Percentage', title = 'Animal Outcome: Babies versus Adults') + theme_few()


cat("Full data set contains ",nrow(full_animal_data_set)," columns:", names(full_animal_data_set),"\n")

#Important: factorize the other variables for fitting a model to the data and making a prediction.
factorVars <- c('Name','OutcomeType','OutcomeSubtype','species',
                'SexuponOutcome','AgeuponOutcome','SimplifiedBreed','simplified_colour',
                'has_given_name','IsMixBreed','is_not_castrated','Sex','TimeofDay','is_adult')

full_animal_data_set[factorVars] <- lapply(full_animal_data_set[factorVars], function(x) as.factor(x))


#Let us fit a randomForest model predicting OutcomeType 

# Split up train and test data
train <- full_animal_data_set[1:26729, ]
test  <- full_animal_data_set[26730:nrow(full_animal_data_set), ]
#train <- full_animal_data_set[1:2729, ]
#test  <- full_animal_data_set[2730:nrow(full_animal_data_set), ]

# Set a random seed
set.seed(731)

# Build randomForest model
print(now())

system.time(rf_mod <- randomForest(OutcomeType ~ species+age_in_days+is_not_castrated+has_given_name+Hour+Weekday+TimeofDay+simplified_colour+IsMixBreed+Sex+Month, 
                        data = train,  ntree = 100,  importance = TRUE))


library(VGAM)
model_logistic_regression <- vglm(OutcomeType ~ species+age_in_days+is_not_castrated+has_given_name+Hour+Weekday+TimeofDay+simplified_colour+IsMixBreed+Sex+Month,family=multinomial,data=train)
print(now())
cat(" finished fiting the model using training data set")


# Show model error
plot(rf_mod, ylim=c(0,1))
legend('topright', colnames(rf_mod$err.rate), col=1:6, fill=1:6)

#let us see variables importance using MeanDecreaseGini
importance    <- importance(rf_mod)
varImportance <- data.frame(Variables = row.names(importance), Importance = round(importance[ ,'MeanDecreaseGini'],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>% mutate(Rank = paste0('#',dense_rank(desc(Importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                           y = Importance)) +
  geom_bar(stat='identity', colour = 'black') +
  geom_text(aes(x = Variables, y = 0.5, label = Rank),
            hjust=0, vjust=0.55, size = 4, colour = 'lavender',
            fontface = 'bold') +
  labs(x = 'Variables', title = 'Relative Variable Importance') +
  coord_flip() + 
  theme_few()


#most important variables for predicting the outcomes of shelter animals are: age_in_days and is_not_castrated, simplified_colour, arrival_time

#prediction using multiclass classification with caret
library(caret)
set.seed(731)

# Predict using the test set
#prediction <- predict(rf_mod, test, type = 'vote')
prediction <- predict(rf_mod, test, type = 'prob')
table(prediction)

# Save the solution to a dataframe
solution <- data.frame('ID' = test$ID, prediction)

# Write predicted solution to file
write.csv(solution, 'rf_solution.csv', row.names = T)

# Accuracy 
predAccuracy <- mean(prediction == test)
probs_rf <- prediction

library(plyr)
library(dplyr)
maxn <- function(n) function(x) order(x, decreasing = TRUE)[n]

system.time(nrstop1<-lapply(probs_rf,maxn(1)))
system.time(nrstop2<-lapply(probs_rf,maxn(2)))
system.time(nrstop3<-lapply(probs_rf,maxn(3)))
system.time(nrstop4<-lapply(probs_rf,maxn(4)))
system.time(nrstop5<-lapply(probs_rf,maxn(5)))

system.time(valstop1<- lapply(probs_rf,function(x)x[maxn(1)(x)]))
system.time(valstop2<- lapply(probs_rf,function(x)x[maxn(2)(x)]))
system.time(valstop3<- lapply(probs_rf,function(x)x[maxn(3)(x)]))
system.time(valstop4<- lapply(probs_rf,function(x)x[maxn(4)(x)]))
system.time(valstop5<- lapply(probs_rf,function(x)x[maxn(5)(x)]))

output_tab1 <-cbind(nrstop1, nrstop2, nrstop3, nrstop4, nrstop5,
                    valstop1, valstop2, valstop3, valstop4, valstop5)
output_tab1 <- as.data.frame(output_tab1)

#######

confusionMatrix <- confusionMatrix(prediction, test)
print(confusionMatrix$overall)
#table(prediction, test$outcome)

confusionMatrix(prediction, test$OutcomeType)
cm = as.matrix(table(Actual = test$OutcomeType, Predicted = prediction)) # create the confusion matrix
cm

#control=trainControl(method="repeatedcv",
#                     number=5,
#                     repeats=10,
#                     classProbs=TRUE,
#                     savePredictions=TRUE,
#                     summaryFunction=LogLosSummary)

control <- trainControl(
  method = "cv",
  number = 10,
  p = 0.2, # train op 20% van de data (10% overlap dus bij 10 cv-stukjes)
  repeats = 2, # met welke mtry begint die dan?
  allowParallel = TRUE
)

rf_mod <- randomForest(OutcomeType ~ species+age_in_days+is_not_castrated+has_given_name+Hour+Weekday+TimeofDay+simplified_colour+IsMixBreed+Sex+Month, 
                       data = train,  ntree = 100,  importance = TRUE)

model=train("OutcomeType ~ ",method="",metric="LogLoss",trControl=control)

### submission using XGBoost library
targets <- train$OutcomeType
train$OutcomeType <- NULL

library(xgboost)

set.seed(121)
a <- as.numeric(data.matrix(train))
           
full_train_matrix <- matrix(as.numeric(data.matrix(train)), ncol=306)
test_matrix <- matrix(as.numeric(data.matrix(test)), ncol=306)

full_targets_train <- as.numeric(targets)-1

# Run xgb on full train set
xgb_model_test = xgboost(data=full_train_matrix, 
                         label=full_targets_train, 
                         nrounds=125, 
                         verbose=1, 
                         eta=0.2, 
                         max_depth=6, 
                         subsample=0.75, 
                         colsample_bytree=0.85,
                         objective="multi:softprob", 
                         eval_metric="mlogloss",
                         num_class=5)


test_preds <- predict(xgb_model_test, test_matrix)
test_preds_frame <- data.frame(matrix(test_preds, ncol = 5, byrow=TRUE))
colnames(test_preds_frame) <- levels(targets)

submission <- cbind(data.frame(ID=test_ID), test_preds_frame)

write.csv(submission , "shelter_animals_submission.csv", row.names=FALSE)
