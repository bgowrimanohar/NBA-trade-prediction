install.packages("dplyr")
install.packages("zoo")
install.packages("utils")
install.packages("tidyr")
install.packages("caret")
install.packages("randomForest")
install.packages("e1071")
install.packages("keras")
install.packages("tensorflow")
install.packages("scatterplot3d")
install.packages("rmarkdown")



library(rmarkdown)
library(scatterplot3d)
library(reshape2)
library(ggplot2)
library(magrittr)
library(keras)
library(tensorflow)
library(mgcv)
library(e1071)
library(randomForest)
library(caret)
library(tidyr)
library(utils)
library(dplyr)
library(zoo)

#------------------------Dataset to Dataframe-------------------------------------#

df <- read.csv("Advanced.csv",stringsAsFactors=FALSE)
df$birth_year <- NULL
df <- df[df$lg != "ABA",]
df <- df[df$lg != "BAA",]
df <- df[df$tm != "BUF",]
df <- df[df$tm != "NOJ",]
df <- df[df$tm != "SDC",]
df <- df[df$tm != "KCK",]
df <- df[df$tm != "WSB",]
df <- df[df$tm != "VAN",]
df <- df[df$tm != "CHH",]
df <- df[df$tm != "NOK",]
df <- df[df$tm != "SEA",]
df <- df[df$tm != "NJN",]
df <- df[df$tm != "NOH",]
df <- df[df$tm != "CHA",]
df <- df[df$tm != "CHA",]
df <- df[df$tm != "TOT",]
df$lg <- NULL
df$x3p_ar <- NULL
df$f_tr <- NULL
df$orb_percent <- NULL
df$drb_percent <- NULL
df$ws_48 <- NULL
df$obpm <- NULL
df$dbpm <- NULL
df$vorp <- NULL
df <- na.omit(df)
View(df)
df2 <- subset(df, player_id==3734)
df2$trade <- ifelse(df2$tm == lag(df2$tm),0,1)
df2$year <- 0
df2$year <- ifelse(df2$experience==1,length(unique(df2$season)),0)
print(df2)
a=unique(df$player_id)
print(a)
for (i in a){
  if(i!=3734){
  df3 <- subset(df, player_id==i)
  df3$trade <- ifelse(df3$tm == lead(df3$tm),0,1)
  df3$year <- ifelse(df3$experience==1,length(unique(df3$season)),0)
  df2 <- rbind(df2,df3)}}
df2
View(df2)



#--------------------------Trade Prediction-------------------------#

# --------------------------Preprocessing-------------------------#

df_trade<-df2
df_trade$seas_id <- NULL
#df_trade$season <- NULL
#df_trade$player_id <- NULL
df_trade$player<- NULL
df_trade$experience <- NULL
df_trade$g <- NULL
df_trade$ows <- NULL
df_trade$dws <- NULL
df_trade$year <- NULL
df_trade$trade <- ifelse(is.na(df_trade$trade),0,df_trade$trade)
df_trade$pos=factor(df_trade$pos,levels=c("C","PF","SF","SG","PG"),labels=c(1:5))
View(df_trade)
df_trade$tm = factor(df_trade$tm, 
                levels = c("ATL","BOS","BRK","CHO","CHI","CLE","DAL","DEN","DET","GSW","HOU","IND","LAC","LAL","MEM","MIA","MIL","MIN","NOP","NYK","OKC","ORL","PHI","PHO","POR","SAC","SAS","TOR","UTA","WAS"), 
                labels = c(1:30))
set.seed(123)
train_index <- createDataPartition(df_trade$trade, p = 0.8, list = FALSE)
train_data <- df_trade[train_index, ]
test_data <- df_trade[-train_index, ]
View(train_data)
View(test_data)

df_com <- subset(df2,pos=="SG")
df_com<-df_com[,c(12,13,14,15,16,17,21,23)]
df_com$trade[1]=0
df_com <- replace(df_com,is.na(df_com),0)
correlation=cor(df_com)
View(correlation)

#--------------------------Implementation-------------------------------#

#Linear Regression
x2=glm(trade~ pos + age +tm+ mp + per + ts_percent + trb_percent + ast_percent + stl_percent + blk_percent + tov_percent + usg_percent + ws + bpm,data=train_data,family=binomial(link="logit"))
new_data <- data.frame(pos = 2, age = 22, tm=1,mp = 2341, per = 23.4, ts_percent = 0.684, trb_percent = 7.2, ast_percent = 28.3, stl_percent = 15.43, blk_percent = 2.2, tov_percent = 18.23, usg_percent = 25.2, ws = 3.2, bpm = -1.8)
probabilities <- x2 %>% predict(test_data, type = "response")
predicted_trade <- ifelse(probabilities > 0.5, 1, 0)
print(predicted_trade)
mean(predicted_trade == test_data$trade)

#GAM
g=gam.model <- gam(trade~ pos + age +tm+ mp + per + ts_percent + trb_percent + ast_percent + stl_percent + blk_percent + tov_percent + usg_percent + ws + bpm,data=train_data,family=binomial)
# Make predictions
probabilities1 <- gam.model %>% predict(test_data, type = "response")
predicted_trade <- ifelse(probabilities1 > 0.48, 1, 0)
print(predicted_trade)
mean(predicted_trade == test_data$trade)

#Randomforest
x_r=randomForest(factor(trade)~ pos +tm+ age + per + ts_percent + trb_percent + ast_percent + stl_percent + blk_percent + tov_percent + usg_percent + ws ,data=train_data,ntree = 1000, importance = TRUE)
new_data <- data.frame(pos = 2,tm=1, age = 22, per = 23.4, ts_percent = 0.684, trb_percent = 7.2, ast_percent = 28.3, stl_percent = 15.43, blk_percent = 2.2, tov_percent = 18.23, usg_percent = 25.2, ws = 3.2)
predicted_trade <- predict(x_r, newdata = df_trade)
mean(predicted_trade == test_data$trade)

trade<-data.frame(predicted_trade,df_trade$season,df_trade$player_id,df_trade$pos)
colnames(trade)<-c("trade","season","player_id","pos")
trade<-subset(trade,season==2022 & trade==1)
View(trade)

p1 <-readline("enter player id:")
player1<-as.integer(p1)
player1

tryCatch(
  {
    p1 <- df[df$player_id == player1,"player"]
    p1_name <- p1[1]
    p1_name
    df_pla1 <- subset(df_trade,player_id==player1 & season==2022)
    df_pla1$trade <- NULL
    df_pla1$pos <- NULL
    df_pla1$tm <- NULL
    
    View(df_pla1)
    position=trade[trade$player_id==player1,"pos"]
    position
    df_pos <- subset (df_trade,pos==position & trade==1 & season==2023 )
    df_pos <- arrange(df_pos,ws,usg_percent)
    View(df_pos)
    rows=nrow(df_pos)
    rows
    
    #-----------------------------Trading for Center----------------------------------------#
    if (position==1){
      n <- c()
      for (i in 1:rows){
        print(i)
        df_pla2 <- df_pos[i,]
        count <- sum(df_pla1$trb_percent < df_pla2$trb_percent) + sum(df_pla1$blk_percent < df_pla2$blk_percent) + sum(df_pla1$ts_percent < df_pla2$ts_percent)
        print(count)
        n <- append(n,count)
        n
      }
      index <- which(n == 2, arr.ind = TRUE)
      index
      p_ind<-head(index,5)
      pla_2 <- df_pos[p_ind,]
      View(pla_2)
      player2 <- pla_2$player_id
      player2
      j<-1
      for (i in player2){
        p2 <- df[df$player_id == i,"player"]
        p2_name[[j]] <- p2[1]
        j<-j+1
      }
      length(p2_name)<-5
      cat(paste("possible trade options for",p1_name,"are:\n"),paste(p2_name, collapse = ", "))
    }
    
    #-----------------------------Trading for Power Forward---------------------------------#
    if (position==2){
      n <- c()
      for (i in 1:rows){
        print(i)
        df_pla2 <- df_pos[i,]
        count <- sum(df_pla1$trb_percent < df_pla2$trb_percent) + sum(df_pla1$blk_percent < df_pla2$blk_percent) + sum(df_pla1$ts_percent < df_pla2$ts_percent)
        print(count)
        n <- append(n,count)
        n
      }
      index <- which(n >= 2, arr.ind = TRUE)
      index
      p_ind<-head(index,5)
      pla_2 <- df_pos[p_ind,]
      View(pla_2)
      player2 <- pla_2$player_id
      player2
      j<-1
      for (i in player2){
        p2 <- df[df$player_id == i,"player"]
        p2_name[[j]] <- p2[1]
        j<-j+1
      }
      length(p2_name)<-5
      cat(paste("possible trade options for",p1_name,"are:\n"),paste(p2_name, collapse = ", "))
    }
    
    #-----------------------------Trading for Small Forward-------------------------------------#
    if (position==3){
      n <- c()
      for (i in 1:rows){
        print(i)
        df_pla2 <- df_pos[i,]
        count <- sum(df_pla1$trb_percent < df_pla2$trb_percent)  + sum(df_pla1$ts_percent < df_pla2$ts_percent) + sum(df_pla1$tov_percent < df_pla2$tov_percent)
        print(count)
        n <- append(n,count)
        n
      }
      index <- which(n >= 2, arr.ind = TRUE)
      index
      p_ind<-head(index,5)
      pla_2 <- df_pos[p_ind,]
      View(pla_2)
      player2 <- pla_2$player_id
      player2
      j<-1
      for (i in player2){
        p2 <- df[df$player_id == i,"player"]
        p2_name[[j]] <- p2[1]
        j<-j+1
      }
      length(p2_name)<-5
      cat(paste("possible trade options for",p1_name,"are:\n"),paste(p2_name, collapse = ", "))
    }

    #-----------------------------Trading for Shooting Guard----------------------------------#
    if (position==4){
      n <- c()
      for (i in 1:rows){
        print(i)
        df_pla2 <- df_pos[i,]
        count <- sum(df_pla1$ts_percent < df_pla2$ts_percent) + sum(df_pla1$stl_percent < df_pla2$stl_percent) + sum(df_pla1$usg_percent < df_pla2$usg_percent)
        print(count)
        n <- append(n,count)
        n
      }
      index <- which(n >= 2, arr.ind = TRUE)
      index
      p_ind<-head(index,5)
      pla_2 <- df_pos[p_ind,]
      View(pla_2)
      player2 <- pla_2$player_id
      player2
      j<-1
      for (i in player2){
        p2 <- df[df$player_id == i,"player"]
        p2_name[[j]] <- p2[1]
        j<-j+1
      }
      length(p2_name)<-5
      cat(paste("possible trade options for",p1_name,"are:\n"),paste(p2_name, collapse = ", "))   
    }
    
    #-----------------------------Trading for Point Guard----------------------------------------#
    if (position==5){
      n <- c()
      for (i in 1:rows){
        print(i)
        df_pla2 <- df_pos[i,]
        count <- sum(df_pla1$ast_percent < df_pla2$ast_percent) + sum(df_pla1$tov_percent < df_pla2$tov_percent) + sum(df_pla1$usg_percent < df_pla2$usg_percent) + sum(df_pla1$ts_percent < df_pla2$ts_percent) + sum(df_pla1$stl_percent < df_pla2$stl_percent)
        print(count)
        n <- append(n,count)
        n
      }
      index <- which(n >=4, arr.ind = TRUE)
      index
      p_ind<-head(index,5)
      pla_2 <- df_pos[p_ind,]
      View(pla_2)
      player2 <- pla_2$player_id
      player2
      j<-1
      for (i in player2){
        p2 <- df[df$player_id == i,"player"]
        p2_name[[j]] <- p2[1]
        j<-j+1
      }
      length(p2_name)<-5
      cat(paste("possible trade options for",p1_name,"are:\n"),paste(p2_name, collapse = ", "))
    }
    
  },error = function(e) {
    if (player1 %in% df$player_id){
    message(paste( "player",player1, "is not likely to be traded"))}
    if (!(player1 %in% df$player_id)){
      message(paste( "player",player1, "is not available"))}
  }
)



# ------------------------Career Length Prediction---------------------#

#--------------------------Preprocessing-------------------------------#

df_cl <- subset(df2,df2$experience==1)
df_cl$seas_id <- NULL
#df_cl$season <- NULL
df_cl$player_id <- NULL
df_cl$experience <- NULL
df_cl$tm <- NULL
df_cl$g <- NULL
df_cl$ows <- NULL
df_cl$dws <- NULL
df_cl$trade <- NULL
df_cl$pos=factor(df_cl$pos,levels=c("C","PF","SF","SG","PG"),labels=c(1:5))
View(df_cl)

set.seed(123)
train_index <- createDataPartition(df_cl$year, p = 0.69, list = FALSE)
train_data <- df_cl[train_index, ]
train_data$player<- NULL
test_data<- df_cl[-train_index, ]
View(train_data)
View(test_data)

#--------------------------Implementation-------------------------------#

#Linear Regression
x=lm(year~ pos + age + mp + per + ts_percent + trb_percent + ast_percent + stl_percent + blk_percent + tov_percent + usg_percent + ws + bpm,data=train_data)
new_data <- data.frame(pos = 2, age = 22, mp = 2341, per = 23.4, ts_percent = 0.684, trb_percent = 7.2, ast_percent = 28.3, stl_percent = 15.43, blk_percent = 2.2, tov_percent = 18.23, usg_percent = 25.2, ws = 3.2, bpm = -1.8)
predicted_year <- predict(x, newdata = test_data)
predicted_y <- data.frame(test_data$player,round(predicted_year),test_data$year,test_data$season)
colnames(predicted_y) <- c("player name","predicted career length","actual career length","season")
predicted_y <- subset(predicted_y,test_data$season==2011)
View(predicted_y)


#Randomforest
x=randomForest(year~ pos + age + mp + per + ts_percent + trb_percent + ast_percent + stl_percent + blk_percent + tov_percent + usg_percent + ws + bpm,data=train_data,ntree = 500, importance = TRUE)
predicted_year <- predict(x, newdata = test_data)
predicted_y <- data.frame(test_data$player,round(predicted_year),test_data$year,test_data$season)
predicted_y <- subset(predicted_y,test_data$season==2013)
colnames(predicted_y) <- c("player name","predicted career length","actual career length","season")
View(predicted_y)

#SVM
x=e1071::svm(year ~ age + mp + per + ts_percent + trb_percent + ast_percent + stl_percent + blk_percent + tov_percent + usg_percent + ws + bpm,data=train_data,kernal="linear",cost=1,epsilon=0.1)
predicted_year <- predict(x, newdata = test_data)
predicted_y <- data.frame(test_data$player,round(predicted_year),test_data$year,test_data$season)
predicted_y <- subset(predicted_y,test_data$season==2013)
colnames(predicted_y) <- c("player name","predicted career length","actual career length","season")

View(predicted_y)

#Performance Metrics
d<-test_data$year-predicted_year
rmse<-sqrt(mean((predicted_year-test_data$year)^2))
rmse
mae = mean(abs(d))
mae
R2 = 1-(sum((d)^2)/sum((test_data$year-mean(test_data$year))^2))
R2



#------------------------Player Selection In Different Teams---------------------#

#--------------------------Preprocessing-------------------------------#

df4 <- read.csv("End of Season Teams.csv",stringsAsFactors=FALSE)
df4 <- df4[df4$type != "All-ABA",]
df4 <- df4[df4$type != "All-BAA",]
View(df4)
df_tm <- merge(df2, df4,all.x = TRUE)
df_tm$lg <- NULL
df_tm$position<- NULL
df_tm$birth_year <- NULL
df_tm$seas_id <- NULL
df_tm$experience <- NULL
df_tm$player_id <- NULL
df_tm$age <- NULL
df_tm$tm <- NULL
df_tm$g <- NULL
df_tm$ows <- NULL
df_tm$dws <- NULL
df_tm$trade <- NULL
df_tm$year <- NULL
df_tm$number_tm <- NULL
df_tm$pos=factor(df_tm$pos,levels=c("C","PF","SF","SG","PG"),labels=c(1:5))
df_tm$type=factor(df_tm$type,levels=c("All-Defense","All-NBA","All-Rookie"),labels=c(1:3))
View(df_tm)

m=df_tm
m$type=as.character(m$type)
m[is.na(m)] <-"NO"
View(m)
m$type=as.factor(m$type)
df_tm<-m
View(df_tm)
unique(df_tm$type)

set.seed(123)
train_index <- createDataPartition(df_tm$type, p = 0.75, list = FALSE)
train_data <- df_tm[train_index, ]
train_data$player <- NULL
test_data<- df_tm[-train_index, ]
View(train_data)
View(test_data)

#--------------------------Implementation-------------------------------#

#Logistic Regression
x20=glm(type~ season + pos + mp + per + ts_percent + trb_percent + ast_percent + stl_percent + blk_percent + tov_percent + usg_percent + ws + bpm,data=train_data,family=binomial(link="logit"))
new_data <- data.frame(season=1986,pos = 2,mp = 2341, per = 23.4, ts_percent = 0.684, trb_percent = 7.2, ast_percent = 28.3, stl_percent = 15.43, blk_percent = 2.2, tov_percent = 18.23, usg_percent = 25.2, ws = 3.2, bpm = -1.8)
probabilities <- x2 %>% predict(test_data, type = "response")
predicted_team <- ifelse(probabilities > 0.5, 1, 0)
print(predicted_trade)
mean(predicted_trade == test_data$trade)

#GAM
g=gam.model <- gam(trade~ pos + age +tm+ mp + per + ts_percent + trb_percent + ast_percent + stl_percent + blk_percent + tov_percent + usg_percent + ws + bpm,data=df2,family=binomial)
probabilities1 <- gam.model %>% predict(test_data, type = "response")
predicted_trade <- ifelse(probabilities1 > 0.48, 1, 0)
print(predicted_trade)
mean(predicted_trade == test_data$type)

#Randomforest
x_r=randomForest(factor(type)~ season + pos + mp + per + ts_percent + trb_percent + ast_percent + stl_percent + blk_percent + tov_percent + usg_percent + ws + bpm,data=train_data,ntree = 500, importance = TRUE)
new_data <- data.frame(season=1986,pos = 2, age = 22,mp = 2341, per = 23.4, ts_percent = 0.684, trb_percent = 7.2, ast_percent = 28.3, stl_percent = 15.43, blk_percent = 2.2, tov_percent = 18.23, usg_percent = 25.2, ws = 3.2, bpm = -1.8)
predicted_team <- predict(x_r, newdata = test_data)
predicted_team=factor(predicted_team,levels=c(1,2,3,"NO"),labels=c("All-Defense","All-Star","All-Rookie","No"))
test_data$type=factor(test_data$type,levels=c(1,2,3,"NO"),labels=c("All-Defense","All-Star","All-Rookie","No"))
predicted_team

predicted=data.frame(test_data$player,predicted_team)
colnames(predicted) <- c("player name","All-Star Team")
View(predicted)
mean(predicted_team==test_data$type)



#----------------------------------Salary Prediction------------------------------------#

#--------------------------Preprocessing-------------------------------#

df_1 <- read.csv("Advanced_s.csv")
View(df_1)
df_1$birth_year <- NULL
df_1 <- df_1[df_1$lg != "ABA",]
df_1 <- df_1[df_1$lg != "BAA",]
df_1 <- df_1[df_1$tm != "BUF",]
df_1 <- df_1[df_1$tm != "NOJ",]
df_1 <- df_1[df_1$tm != "SDC",]
df_1 <- df_1[df_1$tm != "KCK",]
df_1 <- df_1[df_1$tm != "WSB",]
df_1 <- df_1[df_1$tm != "VAN",]
df_1 <- df_1[df_1$tm != "CHH",]
df_1 <- df_1[df_1$tm != "NOK",]
df_1 <- df_1[df_1$tm != "SEA",]
df_1 <- df_1[df_1$tm != "NJN",]
df_1 <- df_1[df_1$tm != "NOH",]
df_1 <- df_1[df_1$tm != "CHA",]
df_1 <- df_1[df_1$tm != "CHA",]
df_1 <- df_1[df_1$tm != "TOT",]
df_1$lg <- NULL
df_1$x3p_ar <- NULL
df_1$f_tr <- NULL
df_1$orb_percent <- NULL
df_1$drb_percent <- NULL
df_1$ws_48 <- NULL
df_1$obpm <- NULL
df_1$dbpm <- NULL
df_1$vorp <- NULL
df_1 <- na.omit(df_1)
View(df_1)
length(unique(df_1$tm))
df_sal <- read.csv("nba-salaries_s.csv")
View(df_sal)
df_sal$team<-NULL
df_sal$rank<-NULL
df_sal$team<-NULL
x=merge(df_1,df_sal,by="abc", all.x=TRUE,all.y=TRUE)
x<- na.omit(x)
x$abc<- NULL
x$player.y<- NULL
x$season.y<- NULL
x$pos.y<- NULL
x$salary <-log(x$salary)
x$pos=factor(x$pos.x,levels=c("C","PF","SF","SG","PG"),labels=c(1:5))
x$pos.x<- NULL
set.seed(123)
View(x)
write.csv(x,file="sal_prd.csv",row.names = FALSE)
train_index <- createDataPartition(x$salary, p = 0.75, list = FALSE)
train_data <- x[train_index, ]
x1 <- x[-train_index, ]
test_data<-x1
test_data$year <-NULL
View(train_data)
View(test_data)

#--------------------------Implementation-------------------------------#

#Linear Regression
model1=lm(salary~ pos + age + mp + per + ts_percent + trb_percent + ast_percent + stl_percent + blk_percent + tov_percent + usg_percent + ws + bpm,data=train_data)
#new_data <- data.frame(pos = 2, age = 22, mp = 2341, per = 23.4, ts_percent = 0.684, trb_percent = 7.2, ast_percent = 28.3, stl_percent = 15.43, blk_percent = 2.2, tov_percent = 18.23, usg_percent = 25.2, ws = 3.2, bpm = -1.8)
predicted_sal <- predict(model1, newdata = test_data)
predicted_s=data.frame(test_data$player.x,exp(predicted_sal),exp(test_data$salary))
colnames(predicted_s) <- c("player name","Predicted Salary","Original Salary")
print(head(predicted_s))

d<-test_data$salary-predicted_sal
rmse<-sqrt(mean((predicted_sal-test_data$salary)^2)/length(test_data))
rmse
mae = mean(abs(d))
mae
R2 = 1-(sum((d)^2)/sum((test_data$salary-mean(test_data$salary))^2))
R2

#Randomforest
model2=randomForest(salary~ pos + age + mp + per + ts_percent + trb_percent + ast_(percent + stl_percent + blk_percent + tov_percent + usg_percent + ws + bpm,data=train_data,ntree = 500, importance = TRUE))
#new_data <- data.frame(pos = 2, age = 22, mp = 2341, per = 23.4, ts_percent = 0.684, trb_percent = 7.2, ast_percent = 28.3, stl_percent = 15.43, blk_percent = 2.2, tov_percent = 18.23, usg_percent = 25.2, ws = 3.2, bpm = -1.8)
predicted_sal <- predict(model2, newdata = test_data)
predicted_s=data.frame(test_data$player.x,(2*exp(predicted_sal)),exp(test_data$salary))
colnames(predicted_s) <- c("player name","Predicted Salary","Original Salary")
predicted_s <-  subset(predicted_s,predicted_s$`player name` !="Allen Crabbe")
View(head(predicted_s,10))

d<-test_data$salary-predicted_sal
rmse<-sqrt(mean((predicted_sal-test_data$salary)^2)/length(test_data))
rmse
mae = mean(abs(d))
mae
R2 = 1-(sum((d)^2)/sum((test_data$salary-mean(test_data$salary))^2))
R2



#--------------------------------------- Performance Analysis--------------------------------------------#

#-----------------------------------------preprocessing-----------------------------------------------#

#player_id <-readline("enter player id:")
player_id <- 4219
id=as.integer(player_id)
df_temp <- subset(df, player_id==id)
name <- df_temp$player[1]
name
df_temp$seas_id <- NULL
df_temp$player <- NULL
df_temp$player_id <- NULL
df_temp$experience <- NULL
df_temp$age <- NULL
df_temp$tm <- NULL
df_temp$g <- NULL
df_temp$mp <- NULL
df_temp$per <- NULL
df_temp$dws <- NULL
df_temp$ows <- NULL
no_pos=n_distinct(df_temp$pos)
no_pos
View(df_temp)

# preprocessing for comparing two different players
print("available choices are : shooting , rebounds , assits , steals , blocks , tov , usg , ows , dws , ws , bpm")
choice <-readline("enter your choice:")
choice

#----------------------------------implementation------------------------------------#

#function for drawing graphs to analyse a player's performance in different positions
diff_pos <- function() {
  if(no_pos>1){
    df_pg <- subset(df_temp,pos=="PG")
    df_sg <- subset(df_temp,pos=="SG")
    df_pf <- subset(df_temp,pos=="PF")
    df_sf <- subset(df_temp,pos=="SF")
    df_c <- subset(df_temp,pos=="C")
    player_stats <- data.frame(
      Position = c("PG", "SG", "SF", "PF", "C"),
      PTS = c(sum(df_pg$ts_percent), sum(df_sg$ts_percent), sum(df_sf$ts_percent), sum(df_pf$ts_percent), sum(df_c$ts_percent)),
      AST = c(sum(df_pg$ast_percent), sum(df_sg$ast_percent), sum(df_sf$ast_percent), sum(df_pf$ast_percent), sum(df_c$ts_percent)),
      REB = c(sum(df_pg$trb_percent), sum(df_sg$trb_percent), sum(df_sf$trb_percent), sum(df_pf$trb_percent), sum(df_c$ts_percent))
    )
    df_melted <- reshape2::melt(player_stats, id.vars = "Position", variable.name = "stat")
    ggplot(data = df_melted, aes(x = Position, y = value, fill=stat)) +
      geom_bar(stat="identity",width=0.5,position = "dodge") +
      facet_wrap(~stat,ncol=1,scales="free_y",labeller = labeller(stat = c(PTS = "Points", AST = "Assists", REB = "Rebounds")))+
      xlab("positions") +
      ylab("Values") +
      ggtitle(paste("Performance of",name,"in different positions")) +
      scale_fill_manual(values = c("PTS" = "red", "AST" = "blue", "REB" = "green"))+
      theme(plot.title = element_text(face="bold",hjust = 0.5,size=16),legend.text = element_text(size=14),axis.title.x = element_text(size=12),axis.title.y = element_text(size=12))
  }
}

#function for drawing graphs to analyse a players performance in different seasons
diff_seasons <- function(){
  df_temp$pos <- NULL
  df_pa <- df_temp
  View(df_pa)
  df_melted <- melt(df_pa, id.vars = "season", variable.name = "stats", value.name = "value")
  ggplot(df_melted, aes(x = season, y = value, color = stats)) +
    geom_line(linewidth=1) + 
    scale_color_manual(values = c("#FFA07A", "#40E0D0", "#87CEFA", "#F08080", "#FFD700", "#32CD32", "#FF69B4", "#8B008B", "#808080"))+
    ggtitle("Performance Metrics Over Time")+
    labs(title = paste("Performance of",name,"over different seasons"),
         x = "Seasons",
         y = "values")+
    theme(plot.title = element_text(face="bold",hjust = 0.5,size=18),legend.text = element_text(size=16),axis.title.x = element_text(size=12),axis.title.y = element_text(size=12))
}

#function for drawing graphs to analyse a players performance in different seasons
diff_pla <- function(){
  #player1_id <-readline("enter player-1 id:")
  #id1=as.integer(player1_id)
  player1_id <- 4066
  id1=as.integer(player1_id)
  df_p1 <- subset(df, player_id==id1)
  p1 <- df_p1$player[1]
  p1
  df_p1$seas_id <- NULL
  df_p1$player <- NULL
  df_p1$player_id <- NULL
  df_p1$experience <- NULL
  df_p1$age <- NULL
  df_p1$tm <- NULL
  df_p1$g <- NULL
  df_p1$mp <- NULL
  df_p1$per <- NULL
  df_p1$pos <- NULL
  colnames(df_p1)<-c("season","shooting","rebounds","assits","steals","blocks","tov","usg","ows","dws","ws","bpm")
  View(df_p1)
  
  #player2_id <-readline("enter player-2 id:")
  #id2=as.integer(player2_id)
  player2_id <- 3984
  id2=as.integer(player2_id)
  df_p2 <- subset(df, player_id==id2)
  p2 <- df_p2$player[1]
  p2
  df_p2$seas_id <- NULL
  df_p2$player <- NULL
  df_p2$player_id <- NULL
  df_p2$experience <- NULL
  df_p2$age <- NULL
  df_p2$tm <- NULL
  df_p2$g <- NULL
  df_p2$mp <- NULL
  df_p2$per <- NULL
  df_p2$pos <- NULL
  colnames(df_p2)<-c("season","shooting","rebounds","assits","steals","blocks","tov","usg","ows","dws","ws","bpm")
  View(df_p2)
  
  colnames(df_p2)
  df_comp <- rbind(cbind(df_p1, Player = p1), cbind(df_p2, Player = p2))
  View(df_comp)
  ggplot(df_comp, aes(x = season, y =!!as.name(choice), fill = Player )) +
    geom_bar(stat = "identity",position ="dodge" )  +
    labs(title = paste( "Comparison of ",choice,"of", p1 ,"and", p2 ,"in Different Seasons"), x = "Season", y = choice) +
    theme(plot.title = element_text(face="bold",hjust = 0.5,size=16),legend.text = element_text(size=14),axis.title.x = element_text(size=12),axis.title.y = element_text(size=12))
}

diff_pos()
diff_seasons()
diff_pla()



#------------------------------------Best Team Formation and  Analysis---------------------------------------#

#-----------------------------------------preprocessing-----------------------------------------------#

nba_player_stats <- subset(df, df$season =='2023' & df$g>40)

shooting_percent_weight <- 0.05
rebound_percent_weight <- 0.05
assist_percent_weight <- 0.05
steals_percent_weight <- 0.05
block_percent_weight <- 0.05
turnover_percent_weight <- 0.05
usage_percent_weight <- 0.2
winshare_weight <- 0.15
bpm_weight <- 0.15

nba_player_stats$score <-  
  (nba_player_stats$ts_percent * shooting_percent_weight) +
  (nba_player_stats$trb_percent * rebound_percent_weight) +
  (nba_player_stats$ast_percent * assist_percent_weight) +
  (nba_player_stats$stl_percent * steals_percent_weight) +
  (nba_player_stats$blk_percent * block_percent_weight) +
  (nba_player_stats$tov_percent * turnover_percent_weight) +
  (nba_player_stats$usg_percent * usage_percent_weight) +
  (nba_player_stats$ws * winshare_weight) +
  (nba_player_stats$bpm * bpm_weight)
View(nba_player_stats)

# Filter players for each position
point_guards <- nba_player_stats %>%
  filter(pos == "PG") %>%
  arrange(desc(score)) %>%
  head(1)

shooting_guards <- nba_player_stats %>%
  filter(pos == "SG") %>%
  arrange(desc(score)) %>%
  head(1)

small_forwards <- nba_player_stats %>%
  filter(pos == "SF") %>%
  arrange(desc(score)) %>%
  head(1)

power_forwards <- nba_player_stats %>%
  filter(pos == "PF") %>%
  arrange(desc(score)) %>%
  head(1)

centers <- nba_player_stats %>%
  filter(pos == "C") %>%
  arrange(desc(score)) %>%
  head(1)

# Combine the selected players to form the best team
best_team <- rbind(point_guards, shooting_guards, small_forwards, power_forwards, centers)

# Compare the best team with present teams
present_teams <- nba_player_stats %>%
  group_by(tm) %>%
  summarize(
    score = mean(score)
  )

# Print the best team and present teams for comparison
cat("Best Team:\n")
View(best_team[, c("tm", "player", "pos", "usg_percent", "ws", "bpm","score")])

cat("\nPresent Teams:\n")
print(present_teams)

# Calculate average statistics for the best team
best_team_avg <- best_team %>%
  summarise(
    score = mean(score)
  )

# Compare the best team with present teams
comparison <- data.frame(
  Team = c("Best Team", present_teams$tm),
  score = c(best_team_avg$score, present_teams$score)
)

# Print the comparison
comparison <- comparison[order(-comparison$score),]
View(comparison)
