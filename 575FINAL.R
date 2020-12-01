### code 575 final ###
library(ranger)
library(rpart)
library(rpart.plot)
library(e1071)
library(tidyverse)

#filter out the rows to only have those that include the bears
# import it however you do sorry that's not included here
pdata <- Play_By_Play_2019_Proposal

v2rm <- c("play_id","game_id","old_game_id","season_type","game_half","quarter_end", "drive" , "time", "desc","qb_dropback","qb_kneel","qb_spike","qb_scramble",
          "pass_length","pass_location","air_yards","yards_after_catch","run_location","run_gap","kick_distance","two_point_conv_result"
          ,"home_timeouts_remaining","away_timeouts_remaining","timeout","timeout_team","posteam_timeouts_remaining","defteam_timeouts_remaining"
          ,"posteam_score_post","defteam_score_post","score_differential_post","opp_fg_prob","opp_safety_prob","opp_td_prob","safety_prob","td_prob","extra_point_prob"
          ,"two_point_conversion_prob","first_down_penalty","third_down_converted","third_down_failed","incomplete_pass","touchback","interception","punt_inside_twenty"
          ,"punt_in_endzone","punt_out_of_bounds","punt_downed","punt_fair_catch","kickoff_inside_twenty","kickoff_in_endzone","kickoff_out_of_bounds","kickoff_downed","kickoff_fair_catch","fumble_forced"
          ,"fumble_not_forced","fumble_out_of_bounds","solo_tackle","safety","penalty","tackled_for_loss","fumble_lost","own_kickoff_recovery","own_kickoff_recovery_td"
          ,"qb_hit","sack","return_touchdown","extra_point_attempt","two_point_attempt","field_goal_attempt","kickoff_attempt","fumble","complete_pass"
          ,"assist_tackle","lateral_reception","lateral_rush","lateral_return","lateral_recovery","passer_player_id","passer_player_name","receiver_player_id","receiver_player_name"
          ,"rusher_player_id","rusher_player_name","lateral_receiver_player_id","lateral_receiver_player_name","lateral_rusher_player_id","lateral_rusher_player_name"
          ,"lateral_sack_player_id","lateral_sack_player_name","interception_player_id","interception_player_name","lateral_interception_player_id","lateral_interception_player_name"
          ,"punt_returner_player_id","punt_returner_player_name","lateral_punt_returner_player_id","lateral_punt_returner_player_name","kickoff_returner_player_name","kickoff_returner_player_id"
          ,"lateral_kickoff_returner_player_id","lateral_kickoff_returner_player_name","punter_player_id","punter_player_name","kicker_player_name","kicker_player_id"
          ,"own_kickoff_recovery_player_id","own_kickoff_recovery_player_name","blocked_player_id","blocked_player_name","tackle_for_loss_1_player_id","tackle_for_loss_1_player_name","tackle_for_loss_2_player_id"
          ,"tackle_for_loss_2_player_name","qb_hit_1_player_id","qb_hit_1_player_name","qb_hit_2_player_id","qb_hit_2_player_name","forced_fumble_player_1_team","forced_fumble_player_1_player_id"
          ,"forced_fumble_player_1_player_name","forced_fumble_player_2_team","forced_fumble_player_2_player_id","forced_fumble_player_2_player_name","solo_tackle_1_team","solo_tackle_2_team","solo_tackle_1_player_id"
          ,"solo_tackle_2_player_id","solo_tackle_1_player_name","solo_tackle_2_player_name","assist_tackle_1_player_id","assist_tackle_1_player_name","assist_tackle_1_team","assist_tackle_2_player_id","assist_tackle_2_player_name"
          ,"assist_tackle_2_team","assist_tackle_3_player_id","assist_tackle_3_player_name","assist_tackle_3_team","assist_tackle_4_player_id","assist_tackle_4_player_name"
          ,"assist_tackle_4_team","pass_defense_1_player_id","pass_defense_1_player_name","pass_defense_2_player_id","pass_defense_2_player_name","fumbled_1_team"
          ,"fumbled_1_player_id","fumbled_1_player_name","fumbled_2_player_id","fumbled_2_player_name","fumbled_2_team","fumble_recovery_1_team","fumble_recovery_1_yards","fumble_recovery_1_player_id"
          ,"fumble_recovery_1_player_name","fumble_recovery_2_team","fumble_recovery_2_yards","fumble_recovery_2_player_id","fumble_recovery_2_player_name","return_team"
          ,"return_yards","penalty_team","penalty_player_id","penalty_yards","replay_or_challenge","replay_or_challenge_result","penalty_type","defensive_two_point_attempt"
          ,"defensive_two_point_conv","defensive_extra_point_attempt","defensive_extra_point_conv","season","stadium","play_clock","play_deleted","st_play_type"
          ,"drive_quarter_start","drive_quarter_end","drive_yards_penalized","drive_start_transition","drive_end_transition","drive_game_clock_start"
          ,"drive_game_clock_end","drive_play_id_started","drive_play_id_ended","game_stadium","first_down","aborted_play","passer_id","rusher_id","receiver_id","name"
          ,"id","qb_epa","xyac_epa","xyac_mean_yardage","xyac_median_yardage","xyac_success","xyac_fd", "order_sequence", "extra_point_result","td_team","total_home_score","total_away_score","posteam_score","defteam_score"
          ,"score_differential","ep","epa","total_home_epa","total_away_epa","total_home_rush_epa","total_away_rush_epa","total_home_pass_epa"
          ,"total_away_pass_epa","air_epa","yac_epa","comp_air_epa","comp_yac_epa","total_home_comp_air_epa","total_away_comp_air_epa","total_home_comp_yac_epa"
          ,"total_away_comp_yac_epa","total_home_raw_air_epa","total_away_raw_air_epa","total_home_raw_yac_epa","total_away_raw_yac_epa","wp","def_wp"
          ,"home_wp","away_wp","wpa","home_wp_post","away_wp_post","vegas_wp","vegas_home_wp","total_home_rush_wpa","total_away_rush_wpa","total_home_pass_wpa"
          ,"total_away_pass_wpa","air_wpa","yac_wpa","comp_air_wpa","comp_yac_wpa","total_home_comp_air_wpa","total_away_comp_air_wpa","total_home_comp_yac_wpa"
          ,"total_away_comp_yac_wpa","total_home_raw_air_wpa","total_away_raw_air_wpa","total_home_raw_yac_wpa","total_away_raw_yac_wpa","first_down_rush"
          ,"first_down_pass","order_sequence","weather","nfl_api_id","end_clock_time","drive_real_start_time","drive_play_count"
          ,"drive_time_of_possession","drive_first_downs","away_score","home_score","location","result","total","spread_line","total_line"
          ,"stadium_id","success","play", "play_type_nfl", "side_of_field", "drive_start_yard_line", "drive_end_yard_line", "end_yard_line", "rush_attempt", "pass_attempt",
          "special_teams_play", "penalty_player_name","home_coach","away_coach","passer","rusher","receiver", "yrdln", "cp", "cpoe", "fg_prob", "time_of_day")

pdata2 <- Play_By_Play_2018 %>% select(-v2rm)
m <- c("fixed_drive", "fixed_drive_result", "passer_jersey_number", "passer_jersey_number", "jersey_number",
       "rusher_jersey_number", "receiver_jersey_number", "series_result")
pdata2 <- pdata2 %>% select(-m)
colnames(pdata)

############## Update columns for inside games wind/temperature (65 set to average inside stadium temperature ################
pdata$temp[pdata$temp=="NA"]<-65
pdata$wind[pdata$wind=="NA"]<-0

#pdata <- pdata %>% replace_na(list(temp=65, na.rm=TRUE))
#pdata<- pdata %>% replace_na(list(wind=0, na.rm=TRUE))


pdata2 <- pdata2 %>% replace_na(list(temp=65, na.rm=TRUE))
pdata2<- pdata2 %>% replace_na(list(wind=0, na.rm=TRUE))

pdata <- pdata %>% select(-v2rm) 

# model 1 - pass or run
#model 2 - redzone score or not


pdata <- pdata %>% filter(posteam !="NA")
pdata2 <- pdata2 %>% filter(posteam !="NA")

##### making the variables factor ########
names <- c('shotgun', "no_huddle", "posteam",'posteam_type', "defteam", "sp", "qtr",
           "down", "play_type", "goal_to_go", "field_goal_result", "punt_blocked", "fourth_down_converted", "fourth_down_failed",
           "pass", "touchdown", "pass_touchdown", "rush_touchdown", "rush",
           "punt_attempt", "special", "drive_inside20", "series_success", 
           "div_game", "roof", "surface", "drive_ended_with_score", "home_team", "away_team")

pdata[,names] <- lapply(pdata[,names], factor)
pdata2[,names] <- lapply(pdata2[,names], factor)


nums <- c("yardline_100", "quarter_seconds_remaining", "half_seconds_remaining", "game_seconds_remaining", "yards_gained",
          "temp", "wind", "series", "ydsnet")

pdata[,nums] <- lapply(pdata[,nums], as.numeric)
pdata2[,nums] <- lapply(pdata2[,nums], as.numeric)


#### convert our dates ########

library(lubridate)
pdata$game_date <- parse_date_time(pdata$game_date, "%x") #error but still works
str(pdata$game_date)
pdata2$game_date <- parse_date_time(pdata2$game_date, "mdY") #error but still works
str(pdata2$game_date)

#Start time to time format
pdata$start_time <- str_remove(pdata$start_time, "1899-12-31")
summary(pdata$start_time)

pdata$start_time <- c(as.numeric(pdata$start_time))
pdata$start_time <- format(as.POSIXct((pdata$start_time) * 86400, origin = "1970-01-01", tz = "UTC"), 
                           "%H:%M:%S")

#pdata2$start_time <- parse_date_time((pdata2$start_time), "%H:%M:%S")

str(pdata2$start_time)
str(pdata$start_time)

start_group <- ifelse(pdata$start_time == "09:29:59" | pdata$start_time == "12:29:59" | pdata$start_time == "13:00:00" | pdata$start_time == "13:05:00",  "First Slate", 
                      ifelse(pdata$start_time == "15:04:59" | pdata$start_time == "16:05:00" | pdata$start_time == "16:25:00" | pdata$start_time == "16:30:00" | pdata$start_time == "16:34:59" | pdata$start_time == "16:40:00", "Second Slate",
                             ifelse(pdata$start_time == "18:29:59" | pdata$start_time == "18:40:00" | pdata$start_time == "19:10:00" | pdata$start_time == "20:15:00" | pdata$start_time == "20:19:59" | pdata$start_time == "22:19:59", "Prime Time", "NA")))     
table(start_group)
pdata <- cbind(pdata, start_group)
pdata$start_group <- as.factor(pdata$start_group)
levels(pdata$start_group)



start_group2 <- ifelse(pdata2$start_time == "09:30:00" | pdata2$start_time == "12:30:00" | pdata2$start_time == "13:00:00" | pdata2$start_time == "13:05:00",  "First Slate", 
                      ifelse(pdata2$start_time == "15:05:00" | pdata2$start_time == "16:05:00" | pdata2$start_time == "16:25:00" | pdata2$start_time == "16:30:00" | pdata2$start_time == "16:35:00" | pdata2$start_time == "16:40:00", "Second Slate",
                             ifelse(pdata2$start_time == "18:30:00" | pdata2$start_time == "18:40:00" | pdata2$start_time == "19:10:00" | pdata2$start_time == "20:15:00" | pdata2$start_time == "20:20:00" | pdata2$start_time == "22:22:00", "Prime Time", "NA")))     

pdata2$start_time

table(start_group2)
pdata2 <- cbind(pdata2, start_group2)
pdata2$start_group2 <- as.factor(pdata2$start_group2)
levels(pdata2$start_group2)
pdata2 <- pdata2 %>% filter(start_group2 !="NA") %>% droplevels()

pdata <- pdata %>% select(-start_time)
pdata2 <- pdata2 %>% select(-start_time)
pdata2$start_group <- pdata2$start_group2
pdata2 <- pdata2 %>% select(-start_group2)

pdataCOMB <- rbind(pdata, pdata2)


# make chicago df
chidata <- pdata[grep("CHI", pdata$posteam), ]
head(chidata)

#for outcome 2--- make new df w just that in the redzone
chid <- which(chidata$drive_inside20==1)
chidataRED <- chidata[chid, ]
chidataRED$drive_ended_with_score <- na.omit(chidataRED$drive_ended_with_score)
summary(chidataRED$drive_ended_with_score)  #label imbalance

# using the combined df for chicago data
# make chicago df
chidataCOMB <- pdataCOMB[grep("CHI", pdataCOMB$posteam), ]
dim(chidataCOMB)  #2701 rows

#for outcome 2--- make new df w just that in the redzone
chid <- which(chidataCOMB$drive_inside20==1)
chidataREDCOMB <- chidataCOMB[chid, ]
chidataREDCOMB$drive_ended_with_score <- na.omit(chidataREDCOMB$drive_ended_with_score)
dim(chidataREDCOMB)  #1035



gbdata <- pdata[grep("GB", pdata$posteam), ]
head(gbdata)

dtdata <- pdata[grep("DET", pdata$posteam), ]
head(dtdata)

mndata <- pdata[grep("MIN", pdata$posteam), ]
head(mndata)

divdata <- rbind(gbdata, dtdata, mndata, chidata)
dim(divdata) 

# in game prop of pass to rush- can change to play type so it says which is which---------------
ggplot(chidata, aes(y = game_date)) +
  geom_bar(aes(fill = pass), position = position_stack(reverse = TRUE)) + coord_flip() +
  labs(title = "Proportion of rush to pass Chicago Bears 2019 Season") + theme_bw()

ggplot(chidata[!is.na(chidata$play_type),], aes( x = play_type)) + geom_bar(fill="lightblue") + 
  labs(title = "Play Types for Chicago Bears 2019") + theme_bw()


ggplot(chidata, aes(x = game_date)) + geom_bar(fill="lightblue", ) + 
  labs(title = "Chicago Bears- Plays per game") + theme_bw() #+

#All Plays for Chicago graph
all_Chi_Play <- chidata %>% filter(chidata$play_type != "NA") %>%
  droplevels()
table(all_Chi_Play$play_type)
ggplot(all_Chi_Play, aes(x = play_type), position = position_stack(reverse = TRUE)) + 
  coord_flip() + geom_bar(fill="lightblue") + 
  labs(title = "Play Types for Chicago Bears 2019") + theme_bw()

#Rush vs. Pass for Chicago table and Graph
PRchidata <- chidata %>% filter(chidata$play_type == "run" | chidata$play_type == "pass") %>%
  droplevels()
table(PRchidata$play_type)
Chi_table<- table(PRchidata$game_date, PRchidata$play_type)
kable(Chi_table, "simple", align = "c")

#Graph with count for all games
ggplot(PRchidata, aes(x = play_type), position = position_stack(reverse = TRUE))+
  coord_flip()+geom_bar(stat = "identity")+
  labs(title = "Chicago Bears Pass vs. Rush for each Game", x = "Game", y = "Count of Plays")+ 
  scale_fill_discrete(labels = c("Pass", "Run"))+ theme(axis.text.x = element_text(angle = 45))+ theme_bw()

#Table for Division Rush vs. Pass
PRdivdata <- divdata %>% filter(divdata$play_type == "run" | divdata$play_type == "pass") %>%
  droplevels()
table(PRdivdata$play_type)
Div_table<- table(PRdivdata$game_date, PRdivdata$play_type)
kable(Div_table, "simple", align = "c")

#Division Bar Graph
ggplot(PRdivdata, aes(x = game_date, y = play_type, fill = play_type), position = position_stack(reverse = TRUE))+coord_flip()+
  geom_bar(stat = "identity")+labs(title = "NFC North Pass vs. Rush for each Game", x = "Game", y = "Count of Plays")+ 
  scale_fill_discrete(labels = c("Pass", "Run"))+ theme_bw()+theme(axis.text.y = element_text(angle = 90))

##################################################################
Pchidata <- chidata[grep("pass", chidata$play_type), ]
Rchidata <- chidata[grep("run", chidata$play_type), ]
PRchidata <- rbind(Pchidata, Rchidata)
dim(PRchidata)

########################################## main models #################################
#test/train split
TRG_PCT=0.7
nr=nrow(PRchidata)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)

PRchiTrn=PRchidata[trnIndex,]   #training data with the randomly selected row-indices
PRchiTst = PRchidata[-trnIndex,]


## model 1 ##
#to include
m1subset = select(PRchiTrn, -c("home_team", "away_team", "sp", "field_goal_result", "rush", "play_type",
                              "special","drive_ended_with_score", "punt_attempt", "pass_touchdown", "rush_touchdown",
                              "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked", "shotgun", "yards_gained",
                              "start_time", "defteam", "ydsnet", "time_of_day", "series_success"))  # temp gonna be in it once we fix 


library(ranger)
library(rpart)
library(rpart.plot)

# get the sheet and do the model with that
rpModel1=rpart(pass ~ ., data=m1subset, method= "class", 
               parms = list(split = "information"), 
               control = rpart.control(minsplit = 30), na.action=na.omit)

# plotting the tree
rpModel1$variable.importance
rpart.plot::prp(rpModel1, type=2, extra=100)

# train and test accuracy
predTrn=predict(rpModel1, PRchiTrn, type='class')
table(pred = predTrn, true=PRchiTrn$pass)
mean(predTrn == PRchiTrn$pass)
table(pred = predict(rpModel1, PRchiTst, type='class'), true=PRchiTst$pass)
mean(predict(rpModel1, PRchiTst, type='class') == PRchiTst$pass)

#GLM for Model 1, rush or pass
summary(m1subset)
m1subset$time_of_day <- as.factor(m1subset$time_of_day)
mod1_play_type <- glm(pass ~ week + yardline_100 + game_date + quarter_seconds_remaining + half_seconds_remaining + game_seconds_remaining +
                      half_seconds_remaining + game_seconds_remaining + qtr + down + goal_to_go + ydstogo + no_huddle + series + time_of_day +
                        drive_inside20 +div_game +roof + surface + start_group, data = m1subset, family = "binomial")

full_model <- mod1_play_type
null_model <- glm(pass ~ 1, data = m1subset, family = "binomial")
step(null_model, scope = list(lower = null_model, upper = full_model), direction = "both")

mod1_glm <- glm(pass ~ down + ydstogo + no_huddle + half_seconds_remaining, family = "binomial", data = m1subset)

#Test Accuracy
PRchiTst <- PRchiTst %>% filter(down !="NA")
pred <- predict(mod1_glm, newdata = PRchiTst)
pred_class <- as.factor(ifelse(pred >= 0.5, "1", "0"))
library(caret)
confusionMatrix(pred_class, PRchiTst$pass)

##################### model 2 ############################ RZ score y or no
#test/train split--- 488 rows- is this enough?
TRG_PCT=0.8
nr=nrow(chidataRED)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)

redchiTrn=chidataRED[trnIndex,]   #training data with the randomly selected row-indices
redchiTst = chidataRED[-trnIndex, ]

m2subset= select(redchiTrn, -c("game_id", "home_team", "away_team", "sp", "field_goal_result", 
                                "special", "punt_attempt", "pass_touchdown", "rush_touchdown", "posteam", "defteam",
                                "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked",
                                "start_time"))  

rpModel2=rpart(drive_ended_with_score ~ ., data=m2subset, method= "class", 
               parms = list(split = "information"), 
               control = rpart.control(minsplit = 30), na.action=na.omit)

# plotting the tree
rpModel2$variable.importance
rpart.plot::prp(rpModel2, type=2, extra=100)

# train and test accuracy??
predTrn=predict(rpModel2, redchiTrn, type='class')
table(pred = predTrn, true=redchiTrn$drive_ended_with_score)
mean(predTrn == redchiTrn$drive_ended_with_score)
table(pred = predict(rpModel2, redchiTst, type='class'), true=redchiTst$drive_ended_with_score)
mean(predict(rpModel2, redchiTst, type='class') == redchiTst$drive_ended_with_score)


#GLM For Model 2
summary(m2subset)
str(m2subset$drive_ended_with_score)
mod2_play_type <- glm(drive_ended_with_score ~ week + posteam_type + yardline_100 + game_date + quarter_seconds_remaining + half_seconds_remaining +
                        game_seconds_remaining + qtr + down + goal_to_go + ydstogo + play_type + yards_gained + shotgun + no_huddle +
                        series + time_of_day +  div_game + roof + surface + pass + rush + start_group, data = m2subset, family = "binomial")

full_model2 <- mod2_play_type
null_model2 <- glm(drive_ended_with_score ~ 1, data = m2subset, family = "binomial")
summary(null_model2)
step(null_model2, scope = list(lower = null_model2, upper = full_model2), direction = "both")

#mod2_glm <- glm(pass ~ down + ydstogo + no_huddle + half_seconds_remaining, family = "binomial", data = m1subset) #Not optimal and error with step

#Test Accuracy 
pred2 <- predict(mod2_glm, newdata = redchiTst)
pred_class2 <- as.factor(ifelse(pred2 >= 0.5, "1", "0"))
library(caret)
confusionMatrix(pred_class2, redchiTst$drive_ended_with_score)



######################################### model 3 ############## punt or go for it ###############
# not a good model to predict as it is
# chi4th$posteam
# cc <- which(chidata$down == 4)
# chi4th <- chidata[cc, ]
# cc <- which(chi4th$posteam == "CHI")
# chi4th <- chidata[cc, ]
# 
# chi4th$posteam 
# # #only 130 if just the bears on offense
# # 
# # cc <- which(divdata$down == 4)
# # div4th <- divdata[cc, ]     #1033 with division
# 
# 
# TRG_PCT=0.7
# nr=nrow(chi4th)
# trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)
# 
# c4trn=chi4th[trnIndex,]   #training data with the randomly selected row-indices
# c4tst = chi4th[-trnIndex, ]
# 
# m3subset= select(c4trn, -c("game_id", "home_team", "away_team", "sp", "drive_ended_with_score", "posteam_score", "defteam_score", "play_type",
#                             "special", "pass_touchdown", "rush_touchdown", "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked", "drive_inside20", "goal_to_go",
#                             "start_time", "shotgun", "pass", "rush", "yards_gained", "field_goal_result", "down", "ydstogo"))
# 
# summary(m3subset$punt_attempt)
# rpModel3=rpart(punt_attempt ~ ., data=m3subset, method= "class", 
#                parms = list(split = "information"), 
#                control = rpart.control(minsplit = 30), na.action=na.omit)
# 
# # plotting the tree
# rpModel3$variable.importance
# rpart.plot::prp(rpModel3, type=2, extra=100)
# 
# # train and test accuracy??
# predTrn=predict(rpModel4, FGtrn, type='class')
# table(pred = predTrn, true=FGtrn$field_goal_result)
# mean(predTrn == FGtrn$field_goal_result)
# table(pred = predict(rpModel2, FGtst, type='class'), true=FGtst$field_goal_result)
# mean(predict(rpModel4, FGtst, type='class') == FGtst$field_goal_result)




########################  FG model ###########################
#### predict if a FG was made ####

#############  FG with the whole league -- otherwise it's too small 

FGdata <- pdata %>% filter(pdata$field_goal_result == "made" | pdata$field_goal_result == "missed") %>% droplevels()
#999

TRG_PCT=0.7
nr=nrow(FGdata)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)

FGtrn=FGdata[trnIndex,]   #training data with the randomly selected row-indices
FGtst = FGdata[-trnIndex, ]

m4subset= select(FGtrn, -c("game_id", "home_team", "away_team", "sp", "drive_ended_with_score", "posteam_score", "defteam_score",
                           "special", "punt_attempt", "pass_touchdown", "rush_touchdown", "posteam", "defteam", "play_type",
                           "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked", "drive_inside20", "goal_to_go",
                           "start_time", "game_date"))  


rpModel4=rpart(field_goal_result ~ ., data=m4subset, method= "class", 
               parms = list(split = "information"), 
               control = rpart.control(minsplit = 30), na.action=na.omit)


# plotting the tree
rpModel4$variable.importance
rpart.plot::prp(rpModel4, type=2, extra=100)

# train and test accuracy??
predTrn=predict(rpModel4, FGtrn, type='class')
table(pred = predTrn, true=FGtrn$field_goal_result)
mean(predTrn == FGtrn$field_goal_result)
table(pred = predict(rpModel2, FGtst, type='class'), true=FGtst$field_goal_result)
mean(predict(rpModel4, FGtst, type='class') == FGtst$field_goal_result)

#GLM model for FG 
summary(m4subset)
week + yardline_100 + game_date + quarter_seconds_remaining + half_seconds_remaining + game_seconds_remaining +
  half_seconds_remaining + game_seconds_remaining + qtr + down + goal_to_go + ydstogo + no_huddle + series + time_of_day +
  drive_inside20 +div_game +roof + surface + start_group

mod4_play_type <- glm(field_goal_result ~ week + posteam_type + yardline_100 + quarter_seconds_remaining + 
                        half_seconds_remaining + game_seconds_remaining + qtr + down + ydstogo + play_type + 
                        field_goal_result + series + time_of_day + div_game +
                        roof + surface + start_group, data = m4subset, family = "binomial")

full_model4 <- mod4_play_type
null_model4 <- glm(field_goal_result ~ 1, data = m4subset, family = "binomial")
step(null_model4, scope = list(lower = null_model4, upper = full_model4), direction = "both")

mod4_glm <- glm(field_goal_result ~ yardline_100 + week, family = "binomial", data = m4subset)

#Test Accuracy 
pred4 <- predict(mod4_glm, newdata = FGtst)
pred_class4 <- as.factor(ifelse(pred4 >= 0.5, "made", "missed"))
library(caret)
confusionMatrix(pred_class4, FGtst$field_goal_result)
##############################################################################################################################################################
#########################################################################
########################################## Baseline ####################################################
library(ranger)
# install.packages(ranger)
pdata2 <- Play_By_Play_2019_Proposal
# 
BLrm <- c("play_id","old_game_id","season_type","game_half","quarter_end", "drive" , "time", "desc","qb_dropback","qb_kneel","qb_spike","qb_scramble",
          "pass_length","pass_location","air_yards","yards_after_catch","run_location","run_gap","kick_distance","two_point_conv_result"
          ,"home_timeouts_remaining","away_timeouts_remaining","timeout","timeout_team","posteam_timeouts_remaining","defteam_timeouts_remaining"
          ,"posteam_score_post","defteam_score_post","score_differential_post","opp_fg_prob","opp_safety_prob","opp_td_prob","safety_prob","td_prob","extra_point_prob"
          ,"two_point_conversion_prob","first_down_penalty","third_down_converted","third_down_failed","incomplete_pass","touchback","interception","punt_inside_twenty"
          ,"punt_in_endzone","punt_out_of_bounds","punt_downed","punt_fair_catch","kickoff_inside_twenty","kickoff_in_endzone","kickoff_out_of_bounds","kickoff_downed","kickoff_fair_catch","fumble_forced"
          ,"fumble_not_forced","fumble_out_of_bounds","solo_tackle","safety","penalty","tackled_for_loss","fumble_lost","own_kickoff_recovery","own_kickoff_recovery_td"
          ,"qb_hit","sack","return_touchdown","extra_point_attempt","two_point_attempt","field_goal_attempt","kickoff_attempt","fumble","complete_pass"
          ,"assist_tackle","lateral_reception","lateral_rush","lateral_return","lateral_recovery","passer_player_id","passer_player_name","receiver_player_id","receiver_player_name"
          ,"rusher_player_id","rusher_player_name","lateral_receiver_player_id","lateral_receiver_player_name","lateral_rusher_player_id","lateral_rusher_player_name"
          ,"lateral_sack_player_id","lateral_sack_player_name","interception_player_id","interception_player_name","lateral_interception_player_id","lateral_interception_player_name"
          ,"punt_returner_player_id","punt_returner_player_name","lateral_punt_returner_player_id","lateral_punt_returner_player_name","kickoff_returner_player_name","kickoff_returner_player_id"
          ,"lateral_kickoff_returner_player_id","lateral_kickoff_returner_player_name","punter_player_id","punter_player_name","kicker_player_name","kicker_player_id"
          ,"own_kickoff_recovery_player_id","own_kickoff_recovery_player_name","blocked_player_id","blocked_player_name","tackle_for_loss_1_player_id","tackle_for_loss_1_player_name","tackle_for_loss_2_player_id"
          ,"tackle_for_loss_2_player_name","qb_hit_1_player_id","qb_hit_1_player_name","qb_hit_2_player_id","qb_hit_2_player_name","forced_fumble_player_1_team","forced_fumble_player_1_player_id"
          ,"forced_fumble_player_1_player_name","forced_fumble_player_2_team","forced_fumble_player_2_player_id","forced_fumble_player_2_player_name","solo_tackle_1_team","solo_tackle_2_team","solo_tackle_1_player_id"
          ,"solo_tackle_2_player_id","solo_tackle_1_player_name","solo_tackle_2_player_name","assist_tackle_1_player_id","assist_tackle_1_player_name","assist_tackle_1_team","assist_tackle_2_player_id","assist_tackle_2_player_name"
          ,"assist_tackle_2_team","assist_tackle_3_player_id","assist_tackle_3_player_name","assist_tackle_3_team","assist_tackle_4_player_id","assist_tackle_4_player_name"
          ,"assist_tackle_4_team","pass_defense_1_player_id","pass_defense_1_player_name","pass_defense_2_player_id","pass_defense_2_player_name","fumbled_1_team"
          ,"fumbled_1_player_id","fumbled_1_player_name","fumbled_2_player_id","fumbled_2_player_name","fumbled_2_team","fumble_recovery_1_team","fumble_recovery_1_yards","fumble_recovery_1_player_id"
          ,"fumble_recovery_1_player_name","fumble_recovery_2_team","fumble_recovery_2_yards","fumble_recovery_2_player_id","fumble_recovery_2_player_name","return_team"
          ,"return_yards","penalty_team","penalty_player_id","penalty_yards","replay_or_challenge","replay_or_challenge_result","penalty_type","defensive_two_point_attempt"
          ,"defensive_two_point_conv","defensive_extra_point_attempt","defensive_extra_point_conv","season","stadium","play_clock","play_deleted","st_play_type"
          ,"drive_quarter_start","drive_quarter_end","drive_yards_penalized","drive_start_transition","drive_end_transition","drive_game_clock_start"
          ,"drive_game_clock_end","drive_play_id_started","drive_play_id_ended","game_stadium","first_down","aborted_play","passer_id","rusher_id","receiver_id","name"
          ,"id","qb_epa","xyac_epa","xyac_mean_yardage","xyac_median_yardage","xyac_success","xyac_fd", "order_sequence","nfl_api_id","end_clock_time","drive_real_start_time","location","result",
          "play_type_nfl", "side_of_field", "drive_start_yard_line", "drive_end_yard_line", "end_yard_line","home_coach", "away_coach","yrdln", "ydsnet")
pdata <- pdata2
pdata <- pdata %>% select(-BLrm)
str(chidata)
pdata= pdata %>% mutate_if(is.character, as.factor)
#########################################################################
########################################## Baseline ####################################################
library(ranger)
# install.packages(ranger)
# 
# #Rush vs. Pass for Chicago table and Graph
PRchidataBL <- chidata %>% filter(chidata$play_type == "run" | chidata$play_type == "pass") %>%
  droplevels()
table(PRchidata$play_type)
Chi_table<- table(PRchidata$game_date, PRchidata$play_type)
table(Chi_table, "simple", align = "c")

#test/train split
TRG_PCT=0.7
nr=nrow(PRchidata)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)

PRchiTrnBL = PRchidataBL[trnIndex,]   #training data with the randomly selected row-indices
PRchiTstBL = PRchidataBL[-trnIndex,]

summary(PRchiTrnBL)
## model 1 ##
#to include  #88
BLsubset = select(PRchiTrnBL, c("game_id", "home_team", "away_team", "sp", "field_goal_result", "play_type",
                                "special","drive_ended_with_score", "punt_attempt", "pass_touchdown", "rush_touchdown",
                                "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked", "shotgun", "yards_gained",
                                "temp", "start_time","ep","epa","total_home_epa","total_away_epa","total_home_rush_epa","total_away_rush_epa","total_home_pass_epa"
                                ,"total_away_pass_epa","air_epa","yac_epa","comp_air_epa","comp_yac_epa","total_home_comp_air_epa","total_away_comp_air_epa","total_home_comp_yac_epa"
                                ,"total_away_comp_yac_epa","total_home_raw_air_epa","total_away_raw_air_epa","total_home_raw_yac_epa","total_away_raw_yac_epa","wp","def_wp"
                                ,"home_wp","away_wp","wpa","home_wp_post","away_wp_post","vegas_wp","vegas_home_wp","total_home_rush_wpa","total_away_rush_wpa","total_home_pass_wpa"
                                ,"total_away_pass_wpa","air_wpa","yac_wpa","comp_air_wpa","comp_yac_wpa","total_home_comp_air_wpa","total_away_comp_air_wpa","total_home_comp_yac_wpa"
                                ,"total_away_comp_yac_wpa","total_home_raw_air_wpa","total_away_raw_air_wpa","total_home_raw_yac_wpa","total_away_raw_yac_wpa","first_down_rush"
                                ,"first_down_pass","weather","drive_play_count" 
                                ,"drive_time_of_possession","drive_first_downs","away_score","home_score","total","spread_line","total_line"
                                ,"stadium_id","success","play",
                                "special_teams_play", "penalty_player_name", "passer", "rusher", "receiver", "series_success", "cp", "cpoe", 
                                "no_score_prob", "fg_prob"))


rpModelBL=rpart(play_type ~ ., data=BLsubset, method= "class", 
                parms = list(split = "information"), 
                control = rpart.control(minsplit = 30), na.action=na.omit)

# plotting the tree
rpModelBL$variable.importance
rpart.plot::prp(rpModelBL, type=2, extra=100)

# train and test accuracy 
predTrn=predict(rpModelBL, PRchiTrnBL, type='class')
table(pred = predTrn, true=PRchiTrnBL$play_type)
mean(predTrn == PRchiTrnBL$play_type)
table(pred = predict(rpModelBL, PRchiTstBL, type='class'), true=PRchiTstBL$play_type)
mean(predict(rpModelBL, PRchiTstBL, type='class') == PRchiTstBL$play_type)


######################   for field goal  #############

FGdata <- pdata %>% filter(pdata$field_goal_result == "made" | pdata$field_goal_result == "missed") %>% droplevels()
#999

TRG_PCT=0.7
nr=nrow(FGdata)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)

FGtrn=FGdata[trnIndex,]   #training data with the randomly selected row-indices
FGtst = FGdata[-trnIndex, ]


BLsubset = select(FGtrn, c("game_id", "home_team", "away_team", "field_goal_result", "play_type", "field_goal_result",
                                "special", "punt_attempt", "pass_touchdown", "rush_touchdown",
                                "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked", "shotgun", "yards_gained",
                                "temp", "start_time","ep","epa","total_home_epa","total_away_epa","total_home_rush_epa","total_away_rush_epa","total_home_pass_epa"
                                ,"total_away_pass_epa","air_epa","yac_epa","comp_air_epa","comp_yac_epa","total_home_comp_air_epa","total_away_comp_air_epa","total_home_comp_yac_epa"
                                ,"total_away_comp_yac_epa","total_home_raw_air_epa","total_away_raw_air_epa","total_home_raw_yac_epa","total_away_raw_yac_epa","wp","def_wp"
                                ,"home_wp","away_wp","wpa","home_wp_post","away_wp_post","vegas_wp","vegas_home_wp","total_home_rush_wpa","total_away_rush_wpa","total_home_pass_wpa"
                                ,"total_away_pass_wpa","air_wpa","yac_wpa","comp_air_wpa","comp_yac_wpa","total_home_comp_air_wpa","total_away_comp_air_wpa","total_home_comp_yac_wpa"
                                ,"total_away_comp_yac_wpa","total_home_raw_air_wpa","total_away_raw_air_wpa","total_home_raw_yac_wpa","total_away_raw_yac_wpa","first_down_rush"
                                ,"first_down_pass","weather","drive_play_count" 
                                ,"drive_time_of_possession","drive_first_downs","away_score","home_score","total","spread_line","total_line"
                                ,"stadium_id","success","play",
                                "special_teams_play", "penalty_player_name", "passer", "rusher", "receiver", "series_success", "cp", "cpoe", 
                                "no_score_prob", "fg_prob"))


rpModelBL=rpart(field_goal_result ~ ., data=BLsubset, method= "class", 
                parms = list(split = "information"), 
                control = rpart.control(minsplit = 30), na.action=na.omit)

# plotting the tree
rpModelBL$variable.importance
rpart.plot::prp(rpModelBL, type=2, extra=100)


predTrn=predict(rpModelBL, FGtrn, type='class')
table(pred = predTrn, true=FGtrn$field_goal_result)
mean(predTrn == FGtrn$field_goal_result)
table(pred = predict(rpModelBL, FGtst, type='class'), true=FGtst$field_goal_result)
mean(predict(rpModelBL, FGtst, type='class') == FGtst$field_goal_result)


#############   chi red zone baseline ####################


TRG_PCT=0.8
nr=nrow(chidataRED)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)

redchiTrn=chidataRED[trnIndex,]   #training data with the randomly selected row-indices
redchiTst = chidataRED[-trnIndex, ]

BLsubset = select(redchiTrn, c("game_id", "home_team", "away_team", "field_goal_result", "play_type", "field_goal_result",
                           "special", "punt_attempt", "pass_touchdown", "rush_touchdown", "drive_ended_with_score",
                           "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked", "shotgun", "yards_gained",
                           "temp", "start_time","ep","epa","total_home_epa","total_away_epa","total_home_rush_epa","total_away_rush_epa","total_home_pass_epa"
                           ,"total_away_pass_epa","air_epa","yac_epa","comp_air_epa","comp_yac_epa","total_home_comp_air_epa","total_away_comp_air_epa","total_home_comp_yac_epa"
                           ,"total_away_comp_yac_epa","total_home_raw_air_epa","total_away_raw_air_epa","total_home_raw_yac_epa","total_away_raw_yac_epa","wp","def_wp"
                           ,"home_wp","away_wp","wpa","home_wp_post","away_wp_post","vegas_wp","vegas_home_wp","total_home_rush_wpa","total_away_rush_wpa","total_home_pass_wpa"
                           ,"total_away_pass_wpa","air_wpa","yac_wpa","comp_air_wpa","comp_yac_wpa","total_home_comp_air_wpa","total_away_comp_air_wpa","total_home_comp_yac_wpa"
                           ,"total_away_comp_yac_wpa","total_home_raw_air_wpa","total_away_raw_air_wpa","total_home_raw_yac_wpa","total_away_raw_yac_wpa","first_down_rush"
                           ,"first_down_pass","weather","drive_play_count" 
                           ,"drive_time_of_possession","drive_first_downs","away_score","home_score","total","spread_line","total_line"
                           ,"stadium_id","success","play",
                           "special_teams_play", "penalty_player_name", "passer", "rusher", "receiver", "series_success", "cp", "cpoe", 
                           "no_score_prob", "fg_prob"))

rpModel2=rpart(drive_ended_with_score ~ ., data=BLsubset, method= "class", 
               parms = list(split = "information"), 
               control = rpart.control(minsplit = 30), na.action=na.omit)

# plotting the tree
rpModel2$variable.importance
rpart.plot::prp(rpModel2, type=2, extra=100)

# train and test accuracy??
predTrn=predict(rpModel2, redchiTrn, type='class')
table(pred = predTrn, true=redchiTrn$drive_ended_with_score)
mean(predTrn == redchiTrn$drive_ended_with_score)
table(pred = predict(rpModel2, redchiTst, type='class'), true=redchiTst$drive_ended_with_score)
mean(predict(rpModel2, redchiTst, type='class') == redchiTst$drive_ended_with_score)




##### address the label imbalance and do models
#now for the drive end w score
library(ROSE)
chidataredBOTH <- ovun.sample(drive_ended_with_score ~ ., data = chidataRED, method = "both")$data
table(chidataredBOTH$drive_ended_with_score)

# pass data balance
PRchidataBOTH <- ovun.sample(pass ~ ., data = PRchidata, method = "both")$data
table(PRchidataBOTH$pass)


#then for field goals
FGdataBOTH <- ovun.sample(field_goal_result ~ ., data = FGdata, method = "both")$data
table(FGdataBOTH$field_goal_result)



