#Clear workspace
rm(list=ls())

#assign libraries
library(gtools)
library(reshape)
library(plyr)
library(lubridate)
library(rattle)
library(zoo)

#set path
path <- 'C:\\temp\\PremierLeague\\'

#download latest table file !Remember to change this when season ends 
download.file(url = "http://www.football-data.co.uk/mmz4281/1415/E0.csv", destfile = paste0(path,"E0.csv"))

#set file list 
files = list.files(path=path,pattern="*.csv")

#exlude files not containing E0 
files <- grep("^E0", files, value = TRUE)

#read om data
dfPremierLeague = do.call("smartbind", lapply(files, function(x){
  y<-read.csv(paste0(path,x), stringsAsFactors = FALSE)
  y$Season <- paste0(format(min(as.Date(y$Date, "%d/%m/%y")), "%Y"),'/', format(max(as.Date(y$Date, "%d/%m/%y")), "%Y"))
  y
} ))

#check the data 
NROW(dfPremierLeague)
str(dfPremierLeague)

#convert string date to date 
dfPremierLeague$DateConverted <- as.Date(dfPremierLeague$Date, "%d/%m/%y")
summary(dfPremierLeague$DateConverted)

#create outcome flag for Home
dfPremierLeague$HomeFlag <- as.factor(ifelse(dfPremierLeague$FTR=='H','yes','no'))
dfPremierLeague$DrawFlag <- as.factor(ifelse(dfPremierLeague$FTR=='D','yes','no'))
dfPremierLeague$AwayFlag <- as.factor(ifelse(dfPremierLeague$FTR=='A','yes','no'))

#sort the data set by game
dfPremierLeague.Sorted <- dfPremierLeague[order(dfPremierLeague$DateConverted),] 

#List of variables

#Div = League Division
#Date = Match Date (dd/mm/yy)
#HomeTeam = Home Team
#AwayTeam = Away Team
#FTHG = Full Time Home Team Goals
#FTAG = Full Time Away Team Goals
#FTR = Full Time Result (H=Home Win, D=Draw, A=Away Win)
#HTHG = Half Time Home Team Goals
#HTAG = Half Time Away Team Goals
#HTR = Half Time Result (H=Home Win, D=Draw, A=Away Win)

#Match Statistics (where available)
#Attendance = Crowd Attendance
#Referee = Match Referee
#HS = Home Team Shots
#AS = Away Team Shots
#HST = Home Team Shots on Target
#AST = Away Team Shots on Target
#HHW = Home Team Hit Woodwork
#AHW = Away Team Hit Woodwork
#HC = Home Team Corners
#AC = Away Team Corners
#HF = Home Team Fouls Committed
#AF = Away Team Fouls Committed
#HO = Home Team Offsides
#AO = Away Team Offsides
#HY = Home Team Yellow Cards
#AY = Away Team Yellow Cards
#HR = Home Team Red Cards
#AR = Away Team Red Cards
#HBP = Home Team Bookings Points (10 = yellow, 25 = red)
#ABP = Away Team Bookings Points (10 = yellow, 25 = red)

#BbMxH = Betbrain maximum home win odds
#BbAvH = Betbrain average home win odds
#BbMxD = Betbrain maximum draw odds
#BbAvD = Betbrain average draw win odds
#BbMxA = Betbrain maximum away win odds
#BbAvA = Betbrain average away win odds

#Identify seasons
seasons <- unique(dfPremierLeague.Sorted$Season)

#Create longformat for league table
dfPremierLeague.long <- melt(dfPremierLeague.Sorted, measure.vars = c("HomeTeam", "AwayTeam"),
                             variable_name = "loc")
dfPremierLeague.long <- rename(dfPremierLeague.long, c(value = "team"))

#sort Data Set by season, date converted and team 
dfPremierLeague.long<- dfPremierLeague.long[order(dfPremierLeague.long$Season ,dfPremierLeague.long$DateConverted , dfPremierLeague.long$team),]
head(dfPremierLeague.long)
dfPremierLeague.long$GameCount <- 1

#calculate points for winning, drawing and loosing
dfPremierLeague.long$pts <- as.integer(with(dfPremierLeague.long, ifelse((loc ==
                                                                            "HomeTeam" & FTR == "H"), 3, ifelse((loc == "AwayTeam" &
                                                                                                                   FTR == "A"), 3, ifelse(FTR == "D", 1, 0)))))

#calculate points at half time
dfPremierLeague.long$halftime.pts <- as.integer(with(dfPremierLeague.long, ifelse((loc ==
                                                                                     "HomeTeam" & HTR  == "H"), 3, ifelse((loc == "AwayTeam" &
                                                                                                                             HTR  == "A"), 3, ifelse(HTR  == "D", 1, 0)))))
#calculate rolling means and game stats for each team pr. season
table.by.season.team <- ddply(dfPremierLeague.long, .(Season,team), transform, 
                              P = cumsum(pts), 
                              G=cumsum(GameCount), 
                              W =cumsum(pts==3), 
                              D=cumsum(pts==1), 
                              L=cumsum(pts==0), 
                              Goals.Scored=cumsum(ifelse(loc == "HomeTeam",FTHG,FTAG)),
                              Goals.LetIn=cumsum(ifelse(loc == "HomeTeam",FTAG,FTHG)),
                              Shots=cumsum(ifelse(loc == "HomeTeam",HS,AS)),
                              Shots.LetIn=cumsum(ifelse(loc == "HomeTeam",AS,HS)),
                              Shots.On.Target=cumsum(ifelse(loc == "HomeTeam",HST,AST)),
                              Shots.On.Target.LetIn=cumsum(ifelse(loc == "HomeTeam",AST,HST)),
                              Corners = cumsum(ifelse(loc == "HomeTeam", HC,AC)),
                              Corners.LetIn = cumsum(ifelse(loc == "HomeTeam", AC,HC)),
                              Fouls.Comitted= cumsum(ifelse(loc == "HomeTeam", HF,AF)),
                              Fouls.Comitted.Against= cumsum(ifelse(loc == "HomeTeam", AF,HF)),
                              Yellow.Cards=cumsum(ifelse(loc=="HomeTeam", HY, AY)),
                              Yellow.Cards.Against=cumsum(ifelse(loc=="HomeTeam", AY, HY)),
                              Red.Cards=cumsum(ifelse(loc=="HomeTeam", HR, AR)),
                              Red.Cards.Against=cumsum(ifelse(loc=="HomeTeam", AR, HR)),
                              Booking.Points=cumsum(ifelse(loc=="HomeTeam", HR*25+HY*10, AR*25+AY*10)),
                              HalfTimeGoals.Scored = cumsum(ifelse(loc=="HomeTeam", HTHG, HTAG)),
                              HalfTimeGoals.LetIn = cumsum(ifelse(loc=="HomeTeam", HTAG, HTHG)),
                              HalfTime.P= cumsum(halftime.pts), 
                              HalfTime.W =cumsum(halftime.pts==3), 
                              HalfTime.D=cumsum(halftime.pts==1),
                              HalfTime.L=cumsum(halftime.pts==0),
                              Last2Games.P.mean=rollmeanr(pts,2,fill=NA),
                              Last3Games.P.mean=rollmeanr(pts,3,fill=NA),
                              Last4Games.P.mean=rollmeanr(pts,4,fill=NA),
                              Last5Games.P.mean=rollmeanr(pts,5,fill=NA),
                              Last2Games.Goals.Scored=rollmeanr(ifelse(loc == "HomeTeam",FTHG,FTAG),2,fill=NA),
                              Last3Games.Goals.Scored=rollmeanr(ifelse(loc == "HomeTeam",FTHG,FTAG),3,fill=NA),
                              Last4Games.Goals.Scored=rollmeanr(ifelse(loc == "HomeTeam",FTHG,FTAG),4,fill=NA),
                              Last5Games.Goals.Scored=rollmeanr(ifelse(loc == "HomeTeam",FTHG,FTAG),5,fill=NA),
                              Last2Games.Goals.LetIn=rollmeanr(ifelse(loc == "HomeTeam",FTAG,FTHG),2,fill=NA),
                              Last3Games.Goals.LetIn=rollmeanr(ifelse(loc == "HomeTeam",FTAG,FTHG),3,fill=NA),
                              Last4Games.Goals.LetIn=rollmeanr(ifelse(loc == "HomeTeam",FTAG,FTHG),4,fill=NA),
                              Last5Games.Goals.LetIn=rollmeanr(ifelse(loc == "HomeTeam",FTAG,FTHG),5,fill=NA),
                              Last2Games.Shots=rollmeanr(ifelse(loc == "HomeTeam",HS,AS),2,fill=NA),
                              Last3Games.Shots=rollmeanr(ifelse(loc == "HomeTeam",HS,AS),3,fill=NA),
                              Last4Games.Shots=rollmeanr(ifelse(loc == "HomeTeam",HS,AS),4,fill=NA),
                              Last5Games.Shots=rollmeanr(ifelse(loc == "HomeTeam",HS,AS),5,fill=NA),  
                              Last2Games.Shots.LetIn=rollmeanr(ifelse(loc == "HomeTeam",AS,HS),2,fill=NA),
                              Last3Games.Shots.LetIn=rollmeanr(ifelse(loc == "HomeTeam",AS,HS),3,fill=NA),
                              Last4Games.Shots.LetIn=rollmeanr(ifelse(loc == "HomeTeam",AS,HS),4,fill=NA),
                              Last5Games.Shots.LetIn=rollmeanr(ifelse(loc == "HomeTeam",AS,HS),5,fill=NA),  
                              Last2Games.Shots.On.Target=rollmeanr(ifelse(loc == "HomeTeam",HST,AST),2,fill=NA),
                              Last3Games.Shots.On.Target=rollmeanr(ifelse(loc == "HomeTeam",HST,AST),3,fill=NA),
                              Last4Games.Shots.On.Target=rollmeanr(ifelse(loc == "HomeTeam",HST,AST),4,fill=NA),
                              Last5Games.Shots.On.Target=rollmeanr(ifelse(loc == "HomeTeam",HST,AST),5,fill=NA),
                              Last2Games.Shots.On.Target.LetIn=rollmeanr(ifelse(loc == "HomeTeam",AST,HST),2,fill=NA),
                              Last3Games.Shots.On.Target.LetIn=rollmeanr(ifelse(loc == "HomeTeam",AST,HST),3,fill=NA),
                              Last4Games.Shots.On.Target.LetIn=rollmeanr(ifelse(loc == "HomeTeam",AST,HST),4,fill=NA),
                              Last5Games.Shots.On.Target.LetIn=rollmeanr(ifelse(loc == "HomeTeam",AST,HST),5,fill=NA),
                              Last2Games.Corners=rollmeanr(ifelse(loc == "HomeTeam", HC,AC),2,fill=NA),
                              Last3Games.Corners=rollmeanr(ifelse(loc == "HomeTeam", HC,AC),3,fill=NA),
                              Last4Games.Corners=rollmeanr(ifelse(loc == "HomeTeam", HC,AC),4,fill=NA),
                              Last5Games.Corners=rollmeanr(ifelse(loc == "HomeTeam", HC,AC),5,fill=NA),
                              Last2Games.Corners.LetIn=rollmeanr(ifelse(loc == "HomeTeam", AC,HC),2,fill=NA),
                              Last3Games.Corners.LetIn=rollmeanr(ifelse(loc == "HomeTeam", AC,HC),3,fill=NA),
                              Last4Games.Corners.LetIn=rollmeanr(ifelse(loc == "HomeTeam", AC,HC),4,fill=NA),
                              Last5Games.Corners.LetIn=rollmeanr(ifelse(loc == "HomeTeam", AC,HC),5,fill=NA),
                              Last2Games.Fouls.Comitted=rollmeanr(ifelse(loc == "HomeTeam", HF,AF),2,fill=NA),
                              Last3Games.Fouls.Comitted=rollmeanr(ifelse(loc == "HomeTeam", HF,AF),3,fill=NA),
                              Last4Games.Fouls.Comitted=rollmeanr(ifelse(loc == "HomeTeam", HF,AF),4,fill=NA),
                              Last5Games.Fouls.Comitted=rollmeanr(ifelse(loc == "HomeTeam", HF,AF),5,fill=NA),
                              Last2Games.Fouls.Comitted.Against=rollmeanr(ifelse(loc == "HomeTeam", AF,HF),2,fill=NA),
                              Last3Games.Fouls.Comitted.Against=rollmeanr(ifelse(loc == "HomeTeam", AF,HF),3,fill=NA),
                              Last4Games.Fouls.Comitted.Against=rollmeanr(ifelse(loc == "HomeTeam", AF,HF),4,fill=NA),
                              Last5Games.Fouls.Comitted.Against=rollmeanr(ifelse(loc == "HomeTeam", AF,HF),5,fill=NA),
                              Last2Games.Yellow.Cards=rollmeanr(ifelse(loc=="HomeTeam", HY, AY),2,fill=NA),
                              Last3Games.Yellow.Cards=rollmeanr(ifelse(loc=="HomeTeam", HY, AY),3,fill=NA),
                              Last4Games.Yellow.Cards=rollmeanr(ifelse(loc=="HomeTeam", HY, AY),4,fill=NA),
                              Last5Games.Yellow.Cards=rollmeanr(ifelse(loc=="HomeTeam", HY, AY),5,fill=NA),
                              Last2Games.Yellow.Cards.Against=rollmeanr(ifelse(loc=="HomeTeam", AY, HY),2,fill=NA),
                              Last3Games.Yellow.Cards.Against=rollmeanr(ifelse(loc=="HomeTeam", AY, HY),3,fill=NA),
                              Last4Games.Yellow.Cards.Against=rollmeanr(ifelse(loc=="HomeTeam", AY, HY),4,fill=NA),
                              Last5Games.Yellow.Cards.Against=rollmeanr(ifelse(loc=="HomeTeam", AY, HY),5,fill=NA),
                              Last2Games.Red.Cards=rollmeanr(ifelse(loc=="HomeTeam", HR, AR),2,fill=NA),
                              Last3Games.Red.Cards=rollmeanr(ifelse(loc=="HomeTeam", HR, AR),3,fill=NA),
                              Last4Games.Red.Cards=rollmeanr(ifelse(loc=="HomeTeam", HR, AR),4,fill=NA),
                              Last5Games.Red.Cards=rollmeanr(ifelse(loc=="HomeTeam", HR, AR),5,fill=NA),
                              Last2Games.Red.Cards.Against=rollmeanr(ifelse(loc=="HomeTeam", AR, HR),2,fill=NA),
                              Last3Games.Red.Cards.Against=rollmeanr(ifelse(loc=="HomeTeam", AR, HR),3,fill=NA),
                              Last4Games.Red.Cards.Against=rollmeanr(ifelse(loc=="HomeTeam", AR, HR),4,fill=NA),
                              Last5Games.Red.Cards.Against=rollmeanr(ifelse(loc=="HomeTeam", AR, HR),5,fill=NA),
                              Last2Games.Halftime.Pts=rollmeanr(halftime.pts,2,fill=NA),
                              Last3Games.Halftime.Pts=rollmeanr(halftime.pts,3,fill=NA),
                              Last4Games.Halftime.Pts=rollmeanr(halftime.pts,4,fill=NA),
                              Last5Games.Halftime.Pts=rollmeanr(halftime.pts,5,fill=NA)   
                              #HTHG = Half Time Home Team Goals
                              #HTAG = Half Time Away Team Goals
                              #HTR = Half Time Result (H=Home Win, D=Draw, A=Away Win)
                              #Days since last game
                              #
)

#fill in gaps for rolling series last 2 games
table.by.season.team$Last2Games.P.mean <- ifelse(is.na(table.by.season.team$Last2Games.P.mean), table.by.season.team$P, table.by.season.team$Last2Games.P.mean)
table.by.season.team$Last3Games.P.mean <- ifelse(is.na(table.by.season.team$Last3Games.P.mean), table.by.season.team$Last2Games.P.mean,table.by.season.team$Last3Games.P.mean)
table.by.season.team$Last4Games.P.mean <- ifelse(is.na(table.by.season.team$Last4Games.P.mean), table.by.season.team$Last3Games.P.mean,table.by.season.team$Last4Games.P.mean)
table.by.season.team$Last5Games.P.mean <- ifelse(is.na(table.by.season.team$Last5Games.P.mean), table.by.season.team$Last4Games.P.mean,table.by.season.team$Last5Games.P.mean)

table.by.season.team$Last2Games.Goals.Scored <- ifelse(is.na(table.by.season.team$Last2Games.Goals.Scored), table.by.season.team$Goals.Scored, table.by.season.team$Last2Games.Goals.Scored)
table.by.season.team$Last3Games.Goals.Scored <- ifelse(is.na(table.by.season.team$Last3Games.Goals.Scored), table.by.season.team$Last2Games.Goals.Scored, table.by.season.team$Last3Games.Goals.Scored)
table.by.season.team$Last4Games.Goals.Scored <- ifelse(is.na(table.by.season.team$Last4Games.Goals.Scored), table.by.season.team$Last3Games.Goals.Scored, table.by.season.team$Last4Games.Goals.Scored)
table.by.season.team$Last5Games.Goals.Scored <- ifelse(is.na(table.by.season.team$Last5Games.Goals.Scored), table.by.season.team$Last4Games.Goals.Scored, table.by.season.team$Last5Games.Goals.Scored)

table.by.season.team$Last2Games.Goals.LetIn <- ifelse(is.na(table.by.season.team$Last2Games.Goals.LetIn), table.by.season.team$Goals.LetIn, table.by.season.team$Last2Games.Goals.LetIn)
table.by.season.team$Last3Games.Goals.LetIn <- ifelse(is.na(table.by.season.team$Last3Games.Goals.LetIn), table.by.season.team$Last2Games.Goals.LetIn , table.by.season.team$Last3Games.Goals.LetIn )
table.by.season.team$Last4Games.Goals.LetIn <- ifelse(is.na(table.by.season.team$Last4Games.Goals.LetIn), table.by.season.team$Last3Games.Goals.LetIn , table.by.season.team$Last4Games.Goals.LetIn )
table.by.season.team$Last5Games.Goals.LetIn <- ifelse(is.na(table.by.season.team$Last5Games.Goals.LetIn), table.by.season.team$Last4Games.Goals.LetIn , table.by.season.team$Last5Games.Goals.LetIn )

table.by.season.team$Last2Games.Shots <- ifelse(is.na(table.by.season.team$Last2Games.Shots), table.by.season.team$Shots, table.by.season.team$Last2Games.Shots)
table.by.season.team$Last3Games.Shots <- ifelse(is.na(table.by.season.team$Last3Games.Shots), table.by.season.team$Last2Games.Shots, table.by.season.team$Last3Games.Shots)
table.by.season.team$Last4Games.Shots <- ifelse(is.na(table.by.season.team$Last4Games.Shots), table.by.season.team$Last3Games.Shots, table.by.season.team$Last4Games.Shots)
table.by.season.team$Last5Games.Shots <- ifelse(is.na(table.by.season.team$Last5Games.Shots), table.by.season.team$Last4Games.Shots, table.by.season.team$Last5Games.Shots)

table.by.season.team$Last2Games.Shots.LetIn <- ifelse(is.na(table.by.season.team$Last2Games.Shots.LetIn), table.by.season.team$Shots.LetIn, table.by.season.team$Last2Games.Shots.LetIn)
table.by.season.team$Last3Games.Shots.LetIn <- ifelse(is.na(table.by.season.team$Last3Games.Shots.LetIn), table.by.season.team$Last2Games.Shots.LetIn, table.by.season.team$Last3Games.Shots.LetIn)
table.by.season.team$Last4Games.Shots.LetIn <- ifelse(is.na(table.by.season.team$Last4Games.Shots.LetIn), table.by.season.team$Last3Games.Shots.LetIn, table.by.season.team$Last4Games.Shots.LetIn)
table.by.season.team$Last5Games.Shots.LetIn <- ifelse(is.na(table.by.season.team$Last5Games.Shots.LetIn), table.by.season.team$Last4Games.Shots.LetIn, table.by.season.team$Last5Games.Shots.LetIn)

table.by.season.team$Last2Games.Shots.On.Target <- ifelse(is.na(table.by.season.team$Last2Games.Shots.On.Target), table.by.season.team$Shots.On.Target, table.by.season.team$Last2Games.Shots.On.Target)
table.by.season.team$Last3Games.Shots.On.Target <- ifelse(is.na(table.by.season.team$Last3Games.Shots.On.Target), table.by.season.team$Last2Games.Shots.On.Target, table.by.season.team$Last3Games.Shots.On.Target)
table.by.season.team$Last4Games.Shots.On.Target <- ifelse(is.na(table.by.season.team$Last4Games.Shots.On.Target), table.by.season.team$Last3Games.Shots.On.Target, table.by.season.team$Last4Games.Shots.On.Target)
table.by.season.team$Last5Games.Shots.On.Target <- ifelse(is.na(table.by.season.team$Last5Games.Shots.On.Target), table.by.season.team$Last4Games.Shots.On.Target, table.by.season.team$Last5Games.Shots.On.Target)

table.by.season.team$Last2Games.Shots.On.Target.LetIn <- ifelse(is.na(table.by.season.team$Last2Games.Shots.On.Target.LetIn), table.by.season.team$Shots.On.Target.LetIn, table.by.season.team$Last2Games.Shots.On.Target.LetIn)
table.by.season.team$Last3Games.Shots.On.Target.LetIn <- ifelse(is.na(table.by.season.team$Last3Games.Shots.On.Target.LetIn), table.by.season.team$Last2Games.Shots.On.Target.LetIn, table.by.season.team$Last3Games.Shots.On.Target.LetIn)
table.by.season.team$Last4Games.Shots.On.Target.LetIn <- ifelse(is.na(table.by.season.team$Last4Games.Shots.On.Target.LetIn), table.by.season.team$Last3Games.Shots.On.Target.LetIn, table.by.season.team$Last4Games.Shots.On.Target.LetIn)
table.by.season.team$Last5Games.Shots.On.Target.LetIn <- ifelse(is.na(table.by.season.team$Last5Games.Shots.On.Target.LetIn), table.by.season.team$Last4Games.Shots.On.Target.LetIn, table.by.season.team$Last5Games.Shots.On.Target.LetIn)

table.by.season.team$Last2Games.Corners <- ifelse(is.na(table.by.season.team$Last2Games.Corners), table.by.season.team$Corners, table.by.season.team$Last2Games.Corners)
table.by.season.team$Last3Games.Corners <- ifelse(is.na(table.by.season.team$Last3Games.Corners), table.by.season.team$Last2Games.Corners, table.by.season.team$Last3Games.Corners)
table.by.season.team$Last4Games.Corners <- ifelse(is.na(table.by.season.team$Last4Games.Corners), table.by.season.team$Last3Games.Corners, table.by.season.team$Last4Games.Corners)
table.by.season.team$Last5Games.Corners <- ifelse(is.na(table.by.season.team$Last5Games.Corners), table.by.season.team$Last4Games.Corners, table.by.season.team$Last5Games.Corners)

table.by.season.team$Last2Games.Corners.LetIn <- ifelse(is.na(table.by.season.team$Last2Games.Corners.LetIn), table.by.season.team$Corners.LetIn, table.by.season.team$Last2Games.Corners.LetIn)
table.by.season.team$Last3Games.Corners.LetIn <- ifelse(is.na(table.by.season.team$Last3Games.Corners.LetIn ), table.by.season.team$Last2Games.Corners.LetIn , table.by.season.team$Last3Games.Corners.LetIn)
table.by.season.team$Last4Games.Corners.LetIn  <- ifelse(is.na(table.by.season.team$Last4Games.Corners.LetIn ), table.by.season.team$Last3Games.Corners.LetIn , table.by.season.team$Last4Games.Corners.LetIn )
table.by.season.team$Last5Games.Corners.LetIn  <- ifelse(is.na(table.by.season.team$Last5Games.Corners.LetIn ), table.by.season.team$Last4Games.Corners.LetIn , table.by.season.team$Last5Games.Corners.LetIn )

table.by.season.team$Last2Games.Fouls.Comitted <- ifelse(is.na(table.by.season.team$Last2Games.Fouls.Comitted), table.by.season.team$Fouls.Comitted, table.by.season.team$Last2Games.Fouls.Comitted)
table.by.season.team$Last3Games.Fouls.Comitted <- ifelse(is.na(table.by.season.team$Last3Games.Fouls.Comitted ), table.by.season.team$Last2Games.Fouls.Comitted , table.by.season.team$Last3Games.Fouls.Comitted)
table.by.season.team$Last4Games.Fouls.Comitted  <- ifelse(is.na(table.by.season.team$Last4Games.Fouls.Comitted ), table.by.season.team$Last3Games.Fouls.Comitted , table.by.season.team$Last4Games.Fouls.Comitted )
table.by.season.team$Last5Games.Fouls.Comitted <- ifelse(is.na(table.by.season.team$Last5Games.Fouls.Comitted ), table.by.season.team$Last4Games.Fouls.Comitted , table.by.season.team$Last5Games.Fouls.Comitted )

table.by.season.team$Last2Games.Fouls.Comitted.Against <- ifelse(is.na(table.by.season.team$Last2Games.Fouls.Comitted.Against), table.by.season.team$Fouls.Comitted.Against, table.by.season.team$Last2Games.Fouls.Comitted.Against)
table.by.season.team$Last3Games.Fouls.Comitted.Against <- ifelse(is.na(table.by.season.team$Last3Games.Fouls.Comitted.Against ), table.by.season.team$Last2Games.Fouls.Comitted.Against , table.by.season.team$Last3Games.Fouls.Comitted.Against)
table.by.season.team$Last4Games.Fouls.Comitted.Against  <- ifelse(is.na(table.by.season.team$Last4Games.Fouls.Comitted.Against ), table.by.season.team$Last3Games.Fouls.Comitted.Against , table.by.season.team$Last4Games.Fouls.Comitted.Against )
table.by.season.team$Last5Games.Fouls.Comitted.Against <- ifelse(is.na(table.by.season.team$Last5Games.Fouls.Comitted.Against ), table.by.season.team$Last4Games.Fouls.Comitted.Against , table.by.season.team$Last5Games.Fouls.Comitted.Against )

table.by.season.team$Last2Games.Yellow.Cards <- ifelse(is.na(table.by.season.team$Last2Games.Yellow.Cards), table.by.season.team$Yellow.Cards, table.by.season.team$Last2Games.Yellow.Cards)
table.by.season.team$Last3Games.Yellow.Cards <- ifelse(is.na(table.by.season.team$Last3Games.Yellow.Cards ), table.by.season.team$Last2Games.Yellow.Cards , table.by.season.team$Last3Games.Yellow.Cards)
table.by.season.team$Last4Games.Yellow.Cards  <- ifelse(is.na(table.by.season.team$Last4Games.Yellow.Cards ), table.by.season.team$Last3Games.Yellow.Cards , table.by.season.team$Last4Games.Yellow.Cards )
table.by.season.team$Last5Games.Yellow.Cards <- ifelse(is.na(table.by.season.team$Last5Games.Yellow.Cards ), table.by.season.team$Last4Games.Yellow.Cards, table.by.season.team$Last5Games.Yellow.Cards )

table.by.season.team$Last2Games.Yellow.Cards.Against <- ifelse(is.na(table.by.season.team$Last2Games.Yellow.Cards.Against), table.by.season.team$Yellow.Cards.Against, table.by.season.team$Last2Games.Yellow.Cards.Against)
table.by.season.team$Last3Games.Yellow.Cards.Against <- ifelse(is.na(table.by.season.team$Last3Games.Yellow.Cards.Against ), table.by.season.team$Last2Games.Yellow.Cards.Against , table.by.season.team$Last3Games.Yellow.Cards.Against)
table.by.season.team$Last4Games.Yellow.Cards.Against  <- ifelse(is.na(table.by.season.team$Last4Games.Yellow.Cards.Against ), table.by.season.team$Last3Games.Yellow.Cards.Against , table.by.season.team$Last4Games.Yellow.Cards.Against )
table.by.season.team$Last5Games.Yellow.Cards.Against <- ifelse(is.na(table.by.season.team$Last5Games.Yellow.Cards.Against ), table.by.season.team$Last4Games.Yellow.Cards.Against, table.by.season.team$Last5Games.Yellow.Cards.Against )

table.by.season.team$Last2Games.Red.Cards <- ifelse(is.na(table.by.season.team$Last2Games.Red.Cards), table.by.season.team$Red.Cards, table.by.season.team$Last2Games.Red.Cards)
table.by.season.team$Last3Games.Red.Cards<- ifelse(is.na(table.by.season.team$Last3Games.Red.Cards ), table.by.season.team$Last2Games.Red.Cards , table.by.season.team$Last3Games.Red.Cards)
table.by.season.team$Last4Games.Red.Cards  <- ifelse(is.na(table.by.season.team$Last4Games.Red.Cards ), table.by.season.team$Last3Games.Red.Cards , table.by.season.team$Last4Games.Red.Cards )
table.by.season.team$Last5Games.Red.Cards<- ifelse(is.na(table.by.season.team$Last5Games.Red.Cards ), table.by.season.team$Last4Games.Red.Cards, table.by.season.team$Last5Games.Red.Cards )

table.by.season.team$Last2Games.Red.Cards.Against <- ifelse(is.na(table.by.season.team$Last2Games.Red.Cards.Against), table.by.season.team$Red.Cards.Against, table.by.season.team$Last2Games.Red.Cards.Against)
table.by.season.team$Last3Games.Red.Cards.Against <- ifelse(is.na(table.by.season.team$Last3Games.Red.Cards.Against  ), table.by.season.team$Last2Games.Red.Cards.Against  , table.by.season.team$Last3Games.Red.Cards.Against )
table.by.season.team$Last4Games.Red.Cards.Against   <- ifelse(is.na(table.by.season.team$Last4Games.Red.Cards.Against  ), table.by.season.team$Last3Games.Red.Cards.Against  , table.by.season.team$Last4Games.Red.Cards.Against  )
table.by.season.team$Last5Games.Red.Cards.Against <- ifelse(is.na(table.by.season.team$Last5Games.Red.Cards.Against  ), table.by.season.team$Last4Games.Red.Cards.Against , table.by.season.team$Last5Games.Red.Cards.Against  )

table.by.season.team$Last2Games.Halftime.Pts <- ifelse(is.na(table.by.season.team$Last2Games.Halftime.Pts), table.by.season.team$HalfTime.P, table.by.season.team$Last2Games.Halftime.Pts)
table.by.season.team$Last3Games.Halftime.Pts <- ifelse(is.na(table.by.season.team$Last3Games.Halftime.Pts   ), table.by.season.team$Last2Games.Halftime.Pts   , table.by.season.team$Last3Games.Halftime.Pts  )
table.by.season.team$Last4Games.Halftime.Pts <- ifelse(is.na(table.by.season.team$Last4Games.Halftime.Pts   ), table.by.season.team$Last3Games.Halftime.Pts   , table.by.season.team$Last4Games.Halftime.Pts  )
table.by.season.team$Last5Games.Halftime.Pts <- ifelse(is.na(table.by.season.team$Last5Games.Halftime.Pts   ), table.by.season.team$Last4Games.Halftime.Pts  , table.by.season.team$Last5Games.Halftime.Pts  )


#identify league position at the end of each season 
final.league.table <- table.by.season.team[table.by.season.team$G==38,]
final.league.table <- final.league.table[order(final.league.table$Season ,-final.league.table$P , -(final.league.table$Goals.Scored-final.league.table$Goals.LetIn)),]
#final.league.table[final.league.table$Season=="2013/2014", c("team", "P", "G","W", "D", "L","Goals.Scored","Goals.LetIn" )]
final.league.table <- ddply(final.league.table, .(Season), transform, 
                            position=cumsum(G==38))

#function to obtain league position last year  
get.season.end.league.position <- function(Season="2008/2009", team="Arsenal"){
  position <- final.league.table[final.league.table$Season==Season & final.league.table$team==team, c("position") ]
  if (length(position)==0){
    position="Promotion"
  }
  return(as.character(position))
}

#function to obtain current leage position and stats  
get.last.game.stats<- function(PriorToDate="2014-08-31", team="Arsenal", Season="2014/2015", PreviousSeason="2013/2014", Prefix="Home."){
  
  #Get the latest league position
  highest<-by(table.by.season.team[table.by.season.team$DateConverted<PriorToDate,], table.by.season.team[table.by.season.team$DateConverted<PriorToDate,c("team")], tail, n=1)
  
  #Remove 0 elements 
  #highest<-highest[-(which(sapply(highest,is.null) ,arr.ind=TRUE))]
  highest<-highest[!sapply(highest, is.null)]
  
  #Merge the data frames together 
  merged.data.frame = Reduce(function(...) merge(..., all=T), highest)
  
  merged.data.frame<-merged.data.frame[merged.data.frame$Season==Season, ]
  
  #Sort table 
  merged.data.frame <- merged.data.frame[order(merged.data.frame$Season ,-merged.data.frame$P , -(merged.data.frame$Goals.Scored-merged.data.frame$Goals.LetIn)),]
  
  #Calculate rank 
  merged.data.frame <- ddply(merged.data.frame, .(Season), transform, 
                             position=cumsum(Season==Season))
  
  #Prefix column variables 
  merged.data.frame <- merged.data.frame[merged.data.frame$team==team,] 
  
  #if 1st game, then take results from last year 
  if (NROW(merged.data.frame )==0){
    merged.data.frame.previous <- final.league.table[final.league.table$Season==PreviousSeason & final.league.table$team==team,]
    #identify number of games since last game
    merged.data.frame.previous$days.since.last.game <- as.numeric(as.Date(PriorToDate) - merged.data.frame.previous$DateConverted)
    colnames(merged.data.frame.previous) <- paste0(Prefix,colnames(merged.data.frame.previous)) 
    return(merged.data.frame.previous)
    
  }
  
  #identify number of games since last game
  merged.data.frame$days.since.last.game <- as.numeric(as.Date(PriorToDate) - merged.data.frame$DateConverted)
  
  #prefix columns
  colnames(merged.data.frame) <- paste0(Prefix,colnames(merged.data.frame)) 
  
  #Returnere datasettet 
  return(merged.data.frame)
}

#download latest fixtures
download.file(url = "http://www.football-data.co.uk/fixtures.csv", destfile = paste0(path,"fixtures.csv"))

#read fixtures
fixtures<-read.csv(paste0(path,"fixtures.csv"), stringsAsFactors = FALSE)
#fixtures <-read.csv2(paste0(path,"fixtures3.csv"),sep=";")

#subset fixtures
fixtures <- fixtures[fixtures$Div=='E0',]

#Convert date 
#fixtures$DateConverted <- as.Date(as.character(fixtures$Date), "%d.%m.%Y")
fixtures$DateConverted <- as.Date(fixtures$Date, "%d/%m/%y")

#Loop through games and run model
#set data frame to store model data in
scoring.data <- NULL

#get current and last season 
current.season <- max(dfPremierLeague.Sorted$Season)
previous.season <- max(dfPremierLeague.Sorted$Season[dfPremierLeague.Sorted$Season!=current.season])

#Loop through fixtures   
for(j in 1:NROW(fixtures)){
    
  #get home team last year position
  home.team.last.year.position <- get.season.end.league.position(Season = previous.season,team = gsub(" $","", fixtures[j,c("HomeTeam")], perl=T))
    
  #get away team last year position
  away.team.last.year.position <- get.season.end.league.position(Season = previous.season,team = gsub(" $","", fixtures[j,c("AwayTeam")], perl=T))
    
  #get home team stats prior to current game
  df.home.team.stats.prior <- get.last.game.stats(PriorToDate = fixtures[j,c("DateConverted")],
                                                    team=gsub(" $","", fixtures[j,c("HomeTeam")], perl=T), 
                                                    Season=current.season,
                                                    PreviousSeason = previous.season,
                                                    Prefix="Home.")
    
  #get away team stats prior to current game 
  df.away.team.stats.prior <- get.last.game.stats(PriorToDate = fixtures[j,c("DateConverted")],
                                                    team=gsub(" $","", fixtures[j,c("AwayTeam")], perl=T), 
                                                    Season=current.season,
                                                    PreviousSeason = previous.season,
                                                    Prefix="Away.")
    
  if(NROW(df.home.team.stats.prior)==1 & NROW(df.away.team.stats.prior)==1){
    #bind the results together
    df.game.stats <- cbind(df.home.team.stats.prior,df.away.team.stats.prior)
        
    #set last year position
    df.game.stats$home.team.last.year.position <- home.team.last.year.position
    df.game.stats$away.team.last.year.position <- away.team.last.year.position
      
    #set flag for UEFA or CL participation
    df.game.stats$home.UEFA <- ifelse(home.team.last.year.position<7 & home.team.last.year.position>4,1,0)
    df.game.stats$home.CL <- ifelse(home.team.last.year.position<5 ,1,0)
    df.game.stats$away.UEFA <- ifelse(away.team.last.year.position<7 & away.team.last.year.position>4,1,0)
    df.game.stats$away.CL <- ifelse(away.team.last.year.position<5 ,1,0)
      
    #calculate position difference between home team and away team
    df.game.stats$diff.postion.home.away <- df.game.stats$Home.position - df.game.stats$Away.position 
      
    #calculate position difference last season between home and away team
    df.game.stats$diff.postion.last.year.home.away <- as.numeric(home.team.last.year.position) - as.numeric(away.team.last.year.position)
      
    #calculate difference in win ratio
    df.game.stats$diff.win.ratio.home.away <- (df.game.stats$Home.W/df.game.stats$Home.G) - (df.game.stats$Away.W/df.game.stats$Away.G)
      
    #calculate difference in draw ratio
    df.game.stats$diff.draw.ratio.home.away <- (df.game.stats$Home.D/df.game.stats$Home.G) - (df.game.stats$Away.D/df.game.stats$Away.G)
      
    #calculate difference in loss ratio
    df.game.stats$diff.loss.ratio.home.away <- (df.game.stats$Home.L/df.game.stats$Home.G) - (df.game.stats$Away.L/df.game.stats$Away.G)
      
    #set outcome of game
    df.game.stats$FTR <- NULL
    df.game.stats$FTHG <- NULL
    df.game.stats$FTAG <- NULL
    df.game.stats$HomeFlag <- NULL
    df.game.stats$DrawFlag <- NULL
    df.game.stats$AwayFlag <- NULL
      
    #set season and game date
    df.game.stats$Season <- current.season
    df.game.stats$DateConverted <- as.Date(fixtures[j, c("DateConverted")])
      
    #set the odds of the current game
    df.game.stats <- cbind(df.game.stats,fixtures[j, c('BbMxH' ,'BbAvH' ,'BbMxD' ,'BbAvD' ,'BbMxA' ,'BbAvA')])
      
    #set month
    df.game.stats$month <- month(as.Date(fixtures[j, c("DateConverted")]))
      
    #append result to a new data frame
    scoring.data<- rbind(scoring.data,df.game.stats )
    
    }
    
    #print.output
    print(paste0(NROW(scoring.data), " ", current.season, " ", fixtures[j,c("DateConverted")]) )
  }
  

#create home position and away position as factor
scoring.data$Home.position <- as.factor(scoring.data$Home.position)
scoring.data$Away.position <- as.factor(scoring.data$Away.position)
scoring.data$month <- as.factor(scoring.data$month)

#create flag for Merseyside derby
scoring.data$Merseyside.Derby <- ifelse(scoring.data$Home.team =='Liverpool' & scoring.data$Away.team =='Everton', 1 , 0)
scoring.data$Merseyside.Derby <- ifelse(scoring.data$Home.team =='Everton' & scoring.data$Away.team =='Liverpool', 1 , scoring.data$Merseyside.Derby)

#create flag for Manchester derby
scoring.data$Manchester.Derby <- ifelse(scoring.data$Home.team =='Man United' & scoring.data$Away.team =='Man City', 1 , 0)
scoring.data$Manchester.Derby <- ifelse(scoring.data$Home.team =='Man City' & scoring.data$Away.team =='Man United', 1 , scoring.data$Manchester.Derby)

#crete flag for Liverpool United derby 
scoring.data$ManLiverpool.Derby <- ifelse(scoring.data$Home.team =='Liverpool' & scoring.data$Away.team =='Man United', 1 , 0)
scoring.data$ManLiverpool.Derby <- ifelse(scoring.data$Home.team =='Man United' & scoring.data$Away.team =='Liverpool', 1 , scoring.data$ManLiverpool.Derby)

#create flag Tottenham and Arsenal flag
scoring.data$TottenArsenal.Derby <- ifelse(scoring.data$Home.team =='Tottenham' & scoring.data$Away.team =='Arsenal', 1 , 0)
scoring.data$TottenArsenal.Derby <- ifelse(scoring.data$Home.team =='Arsenal' & scoring.data$Away.team =='Tottenham', 1 , scoring.data$TottenArsenal.Derby)

#create flag for Newcastle/Sunderland derby
scoring.data$TyneWear.Derby  <- ifelse(scoring.data$Home.team =='Sunderland' & scoring.data$Away.team =='Newcastle', 1 , 0)
scoring.data$TyneWear.Derby <- ifelse(scoring.data$Home.team =='Newcastle' & scoring.data$Away.team =='Sunderland', 1 , scoring.data$TyneWear.Derby)

#Create ratios of all variables
#Home team ratio
scoring.data$Home.Goals.Scored.ratio <- scoring.data$Home.Goals.Scored/scoring.data$Home.G
scoring.data$Home.Goals.LetIn.ratio <- scoring.data$Home.Goals.LetIn/scoring.data$Home.G
scoring.data$Home.Shots.ratio <- scoring.data$Home.Shots/scoring.data$Home.G
scoring.data$Home.Shots.LetIn.ratio <- scoring.data$Home.Shots.LetIn/scoring.data$Home.G
scoring.data$Home.Shots.On.Target.ratio <- scoring.data$Home.Shots.On.Target/scoring.data$Home.G
scoring.data$Home.Shots.On.Target.LetIn.ratio <- scoring.data$Home.Shots.On.Target.LetIn/scoring.data$Home.G
scoring.data$Home.Corners.ratio <- scoring.data$Home.Corners/scoring.data$Home.G
scoring.data$Home.Fouls.Comitteds.ratio <- scoring.data$Home.Fouls.Comitted/scoring.data$Home.G
scoring.data$Home.Fouls.Comitted.Against.ratio <- scoring.data$Home.Fouls.Comitted.Against/scoring.data$Home.G
scoring.data$Home.Yellow.Cards.ratio <- scoring.data$Home.Yellow.Cards/scoring.data$Home.G
scoring.data$Home.Yellow.Cards.Against.ratio <- scoring.data$Home.Yellow.Cards.Against/scoring.data$Home.G
scoring.data$Home.Red.Cards.ratio <- scoring.data$Home.Red.Cards/scoring.data$Home.G
scoring.data$Home.Red.Cards.Against.ratio <- scoring.data$Home.Red.Cards.Against/scoring.data$Home.G
scoring.data$Home.HalfTime.W.ratio <- scoring.data$Home.HalfTime.W/scoring.data$Home.G
scoring.data$Home.HalfTime.D.ratio <- scoring.data$Home.HalfTime.D/scoring.data$Home.G

#Away team ratio
scoring.data$Away.Goals.Scored.ratio <- scoring.data$Away.Goals.Scored/scoring.data$Away.G
scoring.data$Away.Goals.LetIn.ratio <- scoring.data$Away.Goals.LetIn/scoring.data$Away.G
scoring.data$Away.Shots.ratio <- scoring.data$Away.Shots/scoring.data$Away.G
scoring.data$Away.Shots.LetIn.ratio <- scoring.data$Away.Shots.LetIn/scoring.data$Away.G
scoring.data$Away.Shots.On.Target.ratio <- scoring.data$Away.Shots.On.Target/scoring.data$Away.G
scoring.data$Away.Shots.On.Target.LetIn.ratio <- scoring.data$Away.Shots.On.Target.LetIn/scoring.data$Away.G
scoring.data$Away.Corners.ratio <- scoring.data$Away.Corners/scoring.data$Away.G
scoring.data$Away.Fouls.Comitteds.ratio <- scoring.data$Away.Fouls.Comitted/scoring.data$Away.G
scoring.data$Away.Fouls.Comitted.Against.ratio <- scoring.data$Away.Fouls.Comitted.Against/scoring.data$Away.G
scoring.data$Away.Yellow.Cards.ratio <- scoring.data$Away.Yellow.Cards/scoring.data$Away.G
scoring.data$Away.Yellow.Cards.Against.ratio <- scoring.data$Away.Yellow.Cards.Against/scoring.data$Away.G
scoring.data$Away.Red.Cards.ratio <- scoring.data$Away.Red.Cards/scoring.data$Away.G
scoring.data$Away.Red.Cards.Against.ratio <- scoring.data$Away.Red.Cards.Against/scoring.data$Away.G
scoring.data$Away.HalfTime.W.ratio <- scoring.data$Away.HalfTime.W/scoring.data$Away.G
scoring.data$Away.HalfTime.D.ratio <- scoring.data$Away.HalfTime.D/scoring.data$Away.G

#Take difference between each ration
scoring.data$diff.Goals.Scored.ratio <- scoring.data$Home.Goals.Scored.ratio -scoring.data$Away.Goals.Scored.ratio
scoring.data$diff.Goals.LetIn.ratio <- scoring.data$Home.Goals.LetIn.ratio-scoring.data$Away.Goals.LetIn.ratio
scoring.data$diff.Shots.ratio <- scoring.data$Home.Shots.ratio -scoring.data$Away.Shots.ratio 
scoring.data$diff.Shots.LetIn.ratio <- scoring.data$Home.Shots.LetIn.ratio-scoring.data$Away.Shots.LetIn.ratio 
scoring.data$diff.Shots.On.Target.ratio <- scoring.data$Home.Shots.On.Target.ratio-scoring.data$Away.Shots.On.Target.ratio
scoring.data$diff.Shots.On.Target.LetIn.ratio <- scoring.data$Home.Shots.On.Target.LetIn.ratio-scoring.data$Away.Shots.On.Target.LetIn.ratio 
scoring.data$diff.Corners.ratio <- scoring.data$Home.Corners.ratio-scoring.data$Away.Corners.ratio
scoring.data$diff.Fouls.Comitteds.ratio <- scoring.data$Home.Fouls.Comitteds.ratio-scoring.data$Away.Fouls.Comitteds.ratio
scoring.data$diff.Fouls.Comitted.Against.ratio <- scoring.data$Home.Fouls.Comitted.Against.ratio-scoring.data$Away.Fouls.Comitted.Against.ratio
scoring.data$diff.Yellow.Cards.ratio <- scoring.data$Home.Yellow.Cards.ratio-scoring.data$Away.Yellow.Cards.ratio
scoring.data$diff.Yellow.Cards.Against.ratio <- scoring.data$Home.Yellow.Cards.Against.ratio-scoring.data$Away.Yellow.Cards.Against.ratio
scoring.data$diff.Red.Cards.ratio <- scoring.data$Home.Red.Cards.ratio-scoring.data$Away.Red.Cards.ratio
scoring.data$diff.Red.Cards.Against.ratio <- scoring.data$Home.Red.Cards.Against.ratio-scoring.data$Away.Red.Cards.Against.ratio
scoring.data$diff.HalfTime.W.ratio <- scoring.data$Home.HalfTime.W.ratio-scoring.data$Away.HalfTime.W.ratio 
scoring.data$diff.HalfTime.D.ratio <- scoring.data$Home.HalfTime.D.ratio-scoring.data$Away.HalfTime.D.ratio

#Read model data
library(reshape2)
library(caret)
rf_model.HomeFlag <- readRDS(paste0(path, "radomForestModel.rds"))
rf_model.DrawFlag <-  readRDS(paste0(path, "radomForestModelDrawFlag.rds"))
rf_model.AwayFlag<-  readRDS(paste0(path, "radomForestModelAwayFlag.rds"))

#Predict scoring data 
predict.data.home <- predict(rf_model.HomeFlag,newdata=scoring.data,type="prob",na.action = na.pass)
predict.data.draw <- predict(rf_model.DrawFlag,newdata=scoring.data,type="prob",na.action = na.pass)
predict.data.away <- predict(rf_model.AwayFlag,newdata=scoring.data,type="prob",na.action = na.pass)

data.predicted <- cbind(scoring.data,predict.data.home$yes,predict.data.draw$yes,predict.data.away$yes)
write.csv(data.predicted,paste0(path,Sys.Date(),"-ScoredData.csv"))

#Take out odds and prdicted
games <- data.predicted[,c("Home.team","Away.team","predict.data.home$yes","predict.data.draw$yes","predict.data.away$yes","BbAvH","BbAvD","BbAvA")]
games$HomeDiffOdds <- games$"predict.data.home$yes" - 1/games$BbAvH
games$DrawDiffOdds <- games$"predict.data.draw$yes" - 1/games$BbAvD
games$AwayDiffOdds <- games$"predict.data.away$yes" - 1/games$BbAvA 
games

