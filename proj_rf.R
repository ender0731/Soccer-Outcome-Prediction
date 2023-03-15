#Remove # for installing packages
#install.packages('stringr')\
#install.packages('rpart.plot')
#install.packages('randomForest')

require(stringr)
require(dplyr)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)

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

#write.csv(games,"C:\\Users\\91995\\Documents\\R Scripts\\games_consol.csv")


#!!!!!!!!!!!!!!!!need to refactor levels to a lesser than 53 quantity
#!!!TODOapplying a mass function over all columns of df to infreq,freq,rare


for (x in colnames(games)){
  if ((is.factor(games[2,x]))&(nrow(unique(games[x]))>53)){
    
    print(x)
    
    rep=as.data.frame(dplyr::count(games,games[x],sort=TRUE))
    freq=rep[rep$n>100,][1]
    infreq=rep[((rep$n<=100)&(rep$n>25)),][1]

    
    replacer<-function (l){
      if (l %in%  unlist(freq)){
        result<-'Frequent'
      } else if (l %in%  unlist(infreq)){
        result<-'Infrequent'
      } else {
        result<-'Rare'
      }
      return(result)
    }
    
    #replacer
    games[x]<-unlist(apply(games[x],1,replacer))
    
  }
}

colnames(games)









#Separating testing and training data
train<-games[games$date<"2022-01-01",]
#svm cant handle missing values in test data; it'll just drop them
test<-games[games$date>="2022-01-01",]

colnames(train)



model<-randomForest(result~.,data=train,proximity=TRUE)

summary(model)

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











