#get file names
path <- 'C:\\temp\\PremierLeague\\'

#load data
model.data <- readRDS(paste0(path,"modelData.rds"))

#rattle to create model
rattle()

#Split data in test and training data set
set.seed(crv$seed) 
crs$dataset <- model.data
crs$nobs <- nrow(crs$dataset) # 2926 observations 
crs$sample <- crs$train <- model.data[model.data$Season!="2014/2015",] #2625
crs$validate <- model.data[model.data$Season=="2014/2015",] #301

# The following variable selections have been noted.
crs$input <- c("Home.pts", "Home.Last5Games.P.mean", "Home.Last5Games.Goals.Scored", "Home.Last5Games.Goals.LetIn",
               "Home.Last5Games.Shots", "Home.Last5Games.Shots.LetIn", "Home.Last5Games.Shots.On.Target", "Home.Last5Games.Shots.On.Target.LetIn",
               "Home.Last5Games.Fouls.Comitted", "Home.Last5Games.Fouls.Comitted.Against", "Home.Last5Games.Yellow.Cards", "Home.Last5Games.Red.Cards",
               "Home.position", "Away.pts", "Away.Last5Games.P.mean", "Away.Last5Games.Goals.Scored",
               "Away.Last5Games.Goals.LetIn", "Away.Last5Games.Shots", "Away.Last5Games.Shots.LetIn", "Away.Last5Games.Shots.On.Target",
               "Away.Last5Games.Shots.On.Target.LetIn", "Away.Last5Games.Fouls.Comitted", "Away.Last5Games.Yellow.Cards", "Away.Last5Games.Red.Cards",
               "Away.position", "home.team.last.year.position", "away.team.last.year.position", "diff.win.ratio.home.away",
               "diff.draw.ratio.home.away", "month", "Merseyside.Derby", "Manchester.Derby",
               "TottenArsenal.Derby", "TyneWear.Derby", "diff.Goals.LetIn.ratio", "diff.Shots.LetIn.ratio",
               "diff.Shots.On.Target.ratio", "diff.Shots.On.Target.LetIn.ratio", "diff.Corners.ratio", "diff.Fouls.Comitteds.ratio",
               "diff.Fouls.Comitted.Against.ratio", "diff.Yellow.Cards.ratio", "diff.Yellow.Cards.Against.ratio", "diff.Red.Cards.ratio",
               "diff.Red.Cards.Against.ratio", "diff.HalfTime.W.ratio", "diff.HalfTime.D.ratio", "diff.Goals.Scored.ratio",
               "diff.Shots.ratio", "HomeFlag")


# Create model using cross validation
library(caret)
library(Hmisc)
library(gbm)
library(caret)
library(nnet)
library(pROC)
library(ada)
library(party)
library(ROCR)

#Set training grid
fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 2,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

#Set up grid for random forest
grid_rf <- expand.grid(.mtry = c(2))

#Execute model
rf_model.HomeFlag<-train(HomeFlag~.,data=crs$sample[,crs$input],
                         method="rf",
                         trControl=fitControl,
                         tuneGrid = grid_rf,
                         ntree=500,
                         importance=TRUE,
                         prox=TRUE,
                         ## Specify which metric to optimize
                         metric = "ROC")
                         #preProc = c("center", "scale"))

svm_model.HomeFlag<- train(HomeFlag~.,data=crs$sample[,crs$input], method = "svmLinear", trControl = fitControl)

#Check lift curve
predict.data <- predict(rf_model.HomeFlag,newdata=crs$validate[,crs$input],type="prob",na.action = na.pass)
roc <- roc(crs$validate[,c("HomeFlag")],predict.data$yes,plot=TRUE)

#Plot lift
perf <- performance(prediction(predict.data$yes,crs$validate[,c("HomeFlag")]),"lift","rpp")
plot(perf, main="lift curve", colorize=T)

#Postition two seasons ago
seasons <- unique(table.by.season.team$Season)
model.data$home.team.table.position.2.years.ago <- mapply(get.season.end.league.position,seasons[match(model.data$Season,seasons)-2],model.data$Home.team)
model.data$away.team.table.position.2.years.ago <- mapply(get.season.end.league.position,seasons[match(model.data$Season,seasons)-2],model.data$Away.team)

#Flatten model.data to check leage position
model.data.long <- melt(model.data, measure.vars = c("Home.team", "Away.team"),
                             variable_name = "loc")
model.data.long <- rename(model.data.long, c(value = "team"))
model.data.long.limited <- model.data.long[,c("team","Season", "DateConverted", "Home.position", "Away.position", "loc")]
model.data.long.limited$position <- ifelse(model.data.long.limited$loc=='Home.team', model.data.long.limited$Home.position, model.data.long.limited$Away.position)
model.data.long <- model.data.long.limited[order(model.data.long.limited$Season,model.data.long.limited$team,model.data.long.limited$DateConverted),]
position<- ddply(model.data.long, .(team,Season), transform, 
                 avg.position.5.last.games=rollapply(as.numeric(position), 5, function(x) mean(x), partial = TRUE, align = "right"),
                 min.position.5.last.games=rollapply(as.numeric(position), 5, function(x) max(x), partial = TRUE, align = "right"),
                 max.position.5.last.games=rollapply(as.numeric(position), 5, function(x) min(x), partial = TRUE, align = "right"))

#Rename merging variables
home<-rename(position,
       c("avg.position.5.last.games"="home.avg.position.5.last.games",
       "min.position.5.last.games"="home.min.position.5.last.games",
       "max.position.5.last.games"="home.max.position.5.last.games"))

#link the data back to model data again
model.data.merged <- merge( model.data,home[,c("team","DateConverted", "Season",
                                                  "home.avg.position.5.last.games",
                                                  "home.min.position.5.last.games",
                                                  "home.max.position.5.last.games")], 
                           by.x=c("Home.team","DateConverted","Season"),
                           by.y=c("team","DateConverted", "Season") )

#rename for away team merging
away<-rename(position,
             c("avg.position.5.last.games"="away.avg.position.5.last.games",
               "min.position.5.last.games"="away.min.position.5.last.games",
               "max.position.5.last.games"="away.max.position.5.last.games"))

#link the data back to model data again on away team
model.data.merged <- merge( model.data.merged,away[,c("team","DateConverted", "Season",
                                               "away.avg.position.5.last.games",
                                               "away.min.position.5.last.games",
                                               "away.max.position.5.last.games")], 
                            by.x=c("Away.team","DateConverted","Season"),
                            by.y=c("team","DateConverted", "Season") )

#create ratios and recode data 
model.data.merged$away.team.table.position.2.years.ago <- as.factor(model.data.merged$away.team.table.position.2.years.ago)
model.data.merged$home.team.table.position.2.years.ago <- as.factor(model.data.merged$home.team.table.position.2.years.ago)

#create ratios of min max and avg position
model.data.merged$diff.home.current.position.avg.poistion.last5 <- as.numeric(model.data.merged$Home.position)- model.data.merged$home.avg.position.5.last.games
model.data.merged$diff.away.current.position.avg.poistion.last5 <- as.numeric(model.data.merged$Away.position)- model.data.merged$away.avg.position.5.last.games
model.data.merged$diff.home.away.min.position.last5 <- model.data.merged$home.min.position.5.last.games - model.data.merged$away.min.position.5.last.games
model.data.merged$diff.home.away.max.position.last5 <- model.data.merged$home.max.position.5.last.games - model.data.merged$away.max.position.5.last.games
model.data.merged$diff.home.current.position.min.position.last5 <- as.numeric(model.data.merged$Home.position)-model.data.merged$home.min.position.5.last.games
model.data.merged$diff.away.current.position.min.position.last5 <- as.numeric(model.data.merged$Away.position)-model.data.merged$away.min.position.5.last.games

crs$input <- c("Home.pts", "Home.Last5Games.P.mean", "Home.Last5Games.Goals.Scored", "Home.Last5Games.Goals.LetIn",
               "Home.Last5Games.Shots", "Home.Last5Games.Shots.LetIn", "Home.Last5Games.Shots.On.Target", "Home.Last5Games.Shots.On.Target.LetIn",
               "Home.Last5Games.Fouls.Comitted", "Home.Last5Games.Fouls.Comitted.Against", "Home.Last5Games.Yellow.Cards", "Home.Last5Games.Red.Cards",
               "Home.position", "Away.pts", "Away.Last5Games.P.mean", "Away.Last5Games.Goals.Scored",
               "Away.Last5Games.Goals.LetIn", "Away.Last5Games.Shots", "Away.Last5Games.Shots.LetIn", "Away.Last5Games.Shots.On.Target",
               "Away.Last5Games.Shots.On.Target.LetIn", "Away.Last5Games.Fouls.Comitted", "Away.Last5Games.Yellow.Cards", "Away.Last5Games.Red.Cards",
               "Away.position", "home.team.last.year.position", "away.team.last.year.position", "diff.win.ratio.home.away",
               "diff.draw.ratio.home.away", "month", "Merseyside.Derby", "Manchester.Derby",
               "TottenArsenal.Derby", "TyneWear.Derby", "diff.Goals.LetIn.ratio", "diff.Shots.LetIn.ratio",
               "diff.Shots.On.Target.ratio", "diff.Shots.On.Target.LetIn.ratio", "diff.Corners.ratio", "diff.Fouls.Comitteds.ratio",
               "diff.Fouls.Comitted.Against.ratio", "diff.Yellow.Cards.ratio", "diff.Yellow.Cards.Against.ratio", "diff.Red.Cards.ratio",
               "diff.Red.Cards.Against.ratio", "diff.HalfTime.W.ratio", "diff.HalfTime.D.ratio", "diff.Goals.Scored.ratio",
               "diff.Shots.ratio", "HomeFlag","away.team.table.position.2.years.ago", "home.team.table.position.2.years.ago",
               "diff.home.current.position.avg.poistion.last5","diff.away.current.position.avg.poistion.last5", "diff.home.away.min.position.last5",
               "diff.home.away.max.position.last5","diff.home.current.position.min.position.last5","diff.away.current.position.min.position.last5")

rf_model.HomeFlag<-train(HomeFlag~.,data=crs$sample[,crs$input],
                         method="rf",
                         trControl=fitControl,
                         tuneGrid = grid_rf,
                         ntree=500,
                         importance=TRUE,
                         prox=TRUE,
                         ## Specify which metric to optimize
                         metric = "ROC")
#preProc = c("center", "scale"))

rf_model.HomeFlag <- readRDS(paste0(path, "radomForestModel.rds"))
rf_model.DrawFlag <-  readRDS(paste0(path, "radomForestModelDrawFlag.rds"))
rf_model.AwayFlag<-  readRDS(paste0(path, "radomForestModelAwayFlag.rds"))
model.data <- model.data[model.data$Season=="2014/2015",] #301

#Predict scoring data 
predict.data.home <- predict(rf_model.HomeFlag,newdata=model.data,type="prob",na.action = na.pass)
predict.data.draw <- predict(rf_model.DrawFlag,newdata=model.data,type="prob",na.action = na.pass)
predict.data.away <- predict(rf_model.AwayFlag,newdata=model.data,type="prob",na.action = na.pass)
data.predicted <- cbind(model.data,predict.data.home$yes,predict.data.draw$yes,predict.data.away$yes)

#Take out odds and prdicted
strategy <- data.predicted[,c("HomeFlag", "AwayFlag", "DrawFlag", "DateConverted","Home.team","Away.team","predict.data.home$yes","predict.data.draw$yes","predict.data.away$yes","BbAvH","BbAvD","BbAvA")]

#Test strategies
#Single home games > 0,6
over60.home <- strategy[predict.data.home$yes>=0.6,]
over60.home$WonAmount <- 50*ifelse(over60.home$HomeFlag=='yes',1,0)*over60.home$BbAvH
profit.over60.single <- sum(over60.home$WonAmount)-length(over60.home$WonAmount)*50  

#Test strategies
#Double/Triple/Fourth Home.Games > 0,6*0,6
over60double <- ddply(strategy[predict.data.home$yes>=0.5,], .(DateConverted), summarize,
                              Accumulated.Odds=prod(BbAvH),
                              WinRatio=sum(HomeFlag=="yes")/length(HomeFlag),
                              Games=length(HomeFlag))
over60double.games.more.than.one <- over60double[over60double$Games>1,]

over60double.games.more.than.one$WonAmount <- 50*ifelse(over60double.games.more.than.one$WinRatio==1,1,0)*over60double.games.more.than.one$Accumulated.Odds
profit.over60double.home <- sum(over60double.games.more.than.one$WonAmount)-length(over60double.games.more.than.one$WonAmount)*50  

#Test Away, Draw, Home solution 
overaway60double <- ddply(strategy[predict.data.away$yes>=0.4,], .(DateConverted), summarize,
                      Accumulated.Odds=prod(BbAvA),
                      WinRatio=sum(AwayFlag=="yes")/length(AwayFlag),
                      Games=length(AwayFlag))
over60double.games.more.than.one.away <- overaway60double[overaway60double$Games>1,]
over60double.games.more.than.one.away$WonAmount <- 50*ifelse(over60double.games.more.than.one.away$WinRatio==1,1,0)*over60double.games.more.than.one.away$Accumulated.Odds
profit.over60double.away <- sum(over60double.games.more.than.one.away$WonAmount)-length(over60double.games.more.than.one.away$WonAmount)*50  

#Test Draw strategy
overdraw60double <- ddply(strategy[predict.data.draw$yes>=0.26,], .(DateConverted), summarize,
                          Accumulated.Odds=prod(BbAvD),
                          WinRatio=sum(DrawFlag=="yes")/length(DrawFlag),
                          Games=length(DrawFlag))
over60double.games.more.than.one.draw <- overdraw60double[overdraw60double$Games>0,]
over60double.games.more.than.one.draw$WonAmount <- 50*ifelse(over60double.games.more.than.one.draw$WinRatio==1,1,0)*over60double.games.more.than.one.draw$Accumulated.Odds
profit.over60double.draw <- sum(over60double.games.more.than.one.draw$WonAmount)-length(over60double.games.more.than.one.draw$WonAmount)*50  

#Test strategy with model probability overgoing odds and probability over 0.5
odds.better.than.risk<- strategy[strategy$'predict.data.home$yes'>0.68 & (predict.data.home$yes-(1/strategy$BbAvH))>0 ,]
odds.better.than.risk$WonAmount <- 50*ifelse(odds.better.than.risk$HomeFlag=='yes',1,0)*odds.better.than.risk$BbAvH
profit.odds.better.than.risk  <- sum(odds.better.than.risk$WonAmount)-length(odds.better.than.risk$WonAmount)*50
win.ratio <- sum(ifelse(odds.better.than.risk$HomeFlag=='yes',1,0))/length(ifelse(odds.better.than.risk$HomeFlag=='yes',1,0))

#Test strategy with model probability overgoing odds and probability over 0.5
odds.better.than.risk<- strategy[strategy$'predict.data.away$yes'>0.4 & (predict.data.away$yes-(1/strategy$BbAvH))>-0.2 ,]
odds.better.than.risk$WonAmount <- 50*ifelse(odds.better.than.risk$AwayFlag=='yes',1,0)*odds.better.than.risk$BbAvA
profit.odds.better.than.risk  <- sum(odds.better.than.risk$WonAmount)-length(odds.better.than.risk$WonAmount)*50  
win.ratio <- sum(ifelse(odds.better.than.risk$AwayFlag=='yes',1,0))/length(ifelse(odds.better.than.risk$AwayFlag=='yes',1,0))

