

require(fastDummies)
require(rattle)
require(stringr)
require(dplyr)
library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)
library(caret)
library(xgboost)

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
                            {ifelse((as.numeric(str_split(x,':',simplify=TRUE)[1,1])>as.numeric(str_split(x,':',simplify=TRUE)[1,2])),1,0)
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
  if ((is.factor(games[2,x]))&(nrow(unique(games[x]))>53)&(x!='round')){
    
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
    
    #why the fuck isnt this working
    games[x]<-unlist(apply(games[x],1,replacer))
    
  }
}

nums <- unlist(lapply(games, is.numeric), use.names = FALSE)

num_data=games[,nums]

cat_data=games[,(!nums)]
r=cat_data['result']
d=cat_data['date']

#creating dummy cols
cat_data=subset(cat_data,select=-c(date,result))
cat_data=dummy_cols(cat_data)

games=cbind(num_data,cat_data,d,r)


#Separating testing and training data
train<-games[games$date<"2022-01-01",]
train<-subset(train,select=-c(date))
#svm cant handle missing values in test data; it'll just drop them
test<-games[games$date>="2022-01-01",]
test<-subset(test,select=-c(date))

#define predictor and response variables in training set
train_x = data.matrix(train[, -261])
train_y = as.numeric(as.character(train[,261]))

#define predictor and response variables in testing set
test_x = data.matrix(test[, -261])
test_y =as.numeric(as.character(test[, 261]))

#define final training and testing sets
xgb_train = xgb.DMatrix(data = train_x, label = train_y)
xgb_test = xgb.DMatrix(data = test_x, label = test_y)

watchlist = list(train=xgb_train, test=xgb_test)


#building tree
model = xgb.train(data = xgb_train,  max_depth = 2, eta = 1, nthread = 2, nrounds = 2, objective = "binary:logistic")


#confusion matrix
test$t<-ifelse(predict(model,xgb_test)>0.5,1,0)
table(test$result,test$t)

#accuracy
sum(test$result==test$t)/nrow(test)

fancyRpartPlot(model)

#win prediction sensitivity
1080/(1080+240+185)

#win prediction precision
1080/(1080+342+391)

#loss prediction precision
597/(597+2*240)

#https://cran.r-project.org/web/packages/e1071/e1071.pdf
plot(model,train,average_age_home~average_age_away)










