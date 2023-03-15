#Remove # for installing packages
#install.packages('stringr')\
#install.packages('rpart.plot')

require(stringr)
require(dplyr)
library(rpart)
library(rpart.plot)
library(e1071)


setwd("C:\\Users\\91995\\Documents\\R Scripts\\proj_sem2")

clubs<-read.csv('clubs.csv',stringsAsFactors=TRUE)
games<-read.csv('games.csv',stringsAsFactors=TRUE)
competitions<-read.csv('competitions.csv',stringsAsFactors=TRUE)

games$date<-as.Date(games$date)

games<-games[games$date>"2018-01-01",]

#Variable generation ideas

#create FORM variable = ratio of matches won in prev last 6 games for both home and away teams

#need to determine observation window size. for now working on data after 2018. need to cross validate test results based on differing start dates

#creating y var as home win, draw or loss flag

games$result<-unlist(lapply(games$aggregate, 
                            function (x) 
                            {ifelse((as.numeric(str_split(x,':',simplify=TRUE)[1,1])>as.numeric(str_split(x,':',simplify=TRUE)[1,2])),'Home Win','Not Home Win')
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
#games<-games[,!grepl('position',colnames(games))]
#selecting only numeric cols


#svm cant handle nulls well; performing complete cases
games <- games[complete.cases(games), ]

write.csv(games,'C:\\Users\\91995\\Documents\\R Scripts\\proj_sem2\\games_final.csv')

#Separating testing and training data
train<-games[games$date<"2022-01-01",]
#svm cant handle missing values in test data; it'll just drop them
test<-games[games$date>="2022-01-01",]

colnames(train)

#finding best values for gamma and cost
#tuning an svm model

# tune.out=tune.svm(result~.,data=train, kernel="radial",type="C-classification",gamma = 10^(-5:-1), cost = 10^(-3:1))
# 
# tune.out$best.parameters

#building svm
#choosing best kernel
for (x in c('linear','polynomial','radial','sigmoid')) {
model<-svm(result~.,data=train,method="C-classification", kernel=x)
test$x<-predict(model,test)
print(x)
print(sum(test$result==test$x)/nrow(test))
}


#https://opendatascience.com/multi-class-support-vector-machine-r/

model<-svm(result~.,data=train,method="C-classification", kernel='radial',gamma=0.09,cost=11)
#test$t<-predict(model,test)

#confusion matrix
test$t<-predict(model,test)
table(test$result,test$t)

#accuracy
sum(test$result==test$t)/nrow(test)

#win prediction sensitivity
1080/(1080+240+185)

#win prediction precision
1080/(1080+342+391)

#loss prediction precision
597/(597+2*240)

#https://cran.r-project.org/web/packages/e1071/e1071.pdf
plot(model,train,average_age_home~average_age_away)











