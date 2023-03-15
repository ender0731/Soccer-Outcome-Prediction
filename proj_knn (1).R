rm(list=ls())
require(stringr)
require(dplyr)
library(rpart)
library(rpart.plot)
library(e1071)
clubs<-read.csv('clubs.csv',stringsAsFactors=TRUE)
games<-read.csv('games.csv',stringsAsFactors=TRUE)
competitions<-read.csv('competitions.csv',stringsAsFactors=TRUE)

games$date<-as.Date(games$date)

games<-games[games$date>"2018-01-01",]
games$result<-unlist(lapply(games$aggregate, 
                            function (x) 
                            {ifelse((as.numeric(str_split(x,':',simplify=TRUE)[1,1])>as.numeric(str_split(x,':',simplify=TRUE)[1,2])),'Home Win',ifelse((as.numeric(str_split(x,':',simplify=TRUE)[1,1])==as.numeric(str_split(x,':',simplify=TRUE)[1,2])),'Not Home Win','Not Home Win'))
                            }))

games$result<-as.factor(games$result)

#Adding home and away clubs stats to the games data frame

home_clubs<-clubs
colnames(home_clubs)<-paste(colnames(home_clubs),'home',sep='_')

away_clubs<-clubs
colnames(away_clubs)<-paste(colnames(away_clubs),'away',sep='_')

games<-left_join(games,home_clubs,by=c('home_club_id'='club_id_home'))
games<-left_join(games,away_clubs,by=c('away_club_id'='club_id_away'))

#adding competition data
games<-left_join(games,competitions,by=c('competition_id'='competition_id'))

#dropping unnecessary columns
colnames(games)
games<-games[,!grepl('url',colnames(games))]
games<-games[,!grepl('aggregate',colnames(games))]
games<-games[,!grepl('goals',colnames(games))]
games<-select(games,-c("game_id","name_home","pretty_name_home","domestic_competition_id_home","stadium_seats_home","name_away","pretty_name_away","domestic_competition_id_away","stadium_seats_away","name","country_id","domestic_league_code","confederation","country_latitude","country_longitude"))
games <- select(games,-c('home_club_id','away_club_id','attendance'))

#svm cant handle nulls well; performing complete cases
games <- games[complete.cases(games), ]

#Separating testing and training data
train<-games[games$date<"2022-01-01",]
#svm cant handle missing values in test data; it'll just drop them
test<-games[games$date>="2022-01-01",]

#--------------------------------------------------------------------------------------------
# KNN prediction
library(class)
# choosing best k
vk <- seq(1, 301, 5)
accuracy <- vk
for (i in 1:length(vk)) {
  games.knn <- knn(scale(train[, sapply(train, is.numeric)]), 
                   scale(test[, sapply(test, is.numeric)]), 
                   train$result, k = i)
  accuracy[i] <- mean(test$result == games.knn)
}
plot(vk, accuracy, xlab = "k", ylab = "test accuracy", col = "blue")
accuracy
#Confusion Matrix
str(train)
games.knn <- knn(scale(train[, sapply(train, is.numeric)]), 
                 scale(test[, sapply(test, is.numeric)]), 
                 train$result, k = 165)
table(test$result, games.knn)
summary(games.knn)
table(test$result, games.knn)[2,]

#accuracy
(table(test$result, games.knn)[1,1]+table(test$result, games.knn)[2,2])/nrow(test)

#win prediction sensitivity
table(test$result, games.knn)[1,1]/(table(test$result, games.knn)[1,1]+table(test$result, games.knn)[1,2])

#win prediction precision
table(test$result, games.knn)[1,1]/(table(test$result, games.knn)[1,1]+table(test$result, games.knn)[2,1])

#loss prediction precision
table(test$result, games.knn)[2,2]/(table(test$result, games.knn)[2,2]+table(test$result, games.knn)[1,2])
