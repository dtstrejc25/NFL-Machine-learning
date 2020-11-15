### code 575 final ###

#filter out the rows to only have those that include the bears
# import it however you do sorry that's not included here
pdata <- Play_By_Play_2019_Proposal

v2rm <- c("play_id","old_game_id","season_type","game_half","quarter_end", "drive" , "time", "desc","qb_dropback","qb_kneel","qb_spike","qb_scramble",
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
          ,"score_differential","weather","nfl_api_id","end_clock_time","drive_real_start_time","drive_time_of_possession","drive_first_downs","location","result"
          ,"stadium_id","success","play", "play_type_nfl", "side_of_field", "drive_start_yard_line", "drive_end_yard_line", "end_yard_line", "rush_attempt", "pass_attempt",
          ,"special_teams_play", "penalty_player_name", "home_coach", "away_coach", "passer", "rusher", "receiver", "series_success", "yrdln", "ydsnet"))
          
############## Adding back for baseline model all NA's to zero ###################
pdata <- pdata %>% replace_na(list((ep = 0), (epa = 0), (total_home_epa = 0), (total_away_epa = 0), (total_home_rush_epa = 0),
      (total_away_rush_epa = 0), (total_home_pass_epa = 0), (total_away_pass_epa = 0), (air_epa = 0), (yac_epa = 0), (comp_air_epa = 0),
      (comp_yac_epa = 0), (total_home_comp_air_epa = 0), (total_away_comp_air_epa = 0), (total_home_comp_yac_epa = 0), (total_away_comp_yac_epa = 0),
      (total_home_raw_air_epa = 0), (total_away_raw_air_epa = 0), (total_home_raw_yac_epa = 0), (total_away_raw_yac_epa = 0), (wp = 0), (def_wp = 0),
      (home_wp = 0), (away_wp = 0), (wpa = 0), (home_wp_post = 0), (away_wp_post = 0), (vegas_wp = 0), (vegas_home_wp = 0), (total_home_rush_wpa = 0)(total_away_rush_wpa = 0)(total_home_pass_wpa = 0),
      (total_away_pass_wpa = 0), (air_wpa = 0), (yac_wpa = 0), (comp_air_wpa = 0), (comp_yac_wpa = 0), (total_home_comp_air_wpa = 0), (total_away_comp_air_wpa = 0),
      (total_home_comp_yac_wpa = 0), (total_away_comp_yac_wpa = 0), (total_home_raw_air_wpa = 0), (total_away_raw_air_wpa = 0), (total_home_raw_yac_wpa = 0),
      (total_away_raw_yac_wpa = 0), (first_down_rush = 0), (first_down_pass = 0), (no_score_prob = 0), (fg_prob = 0), (drive_play_count = 0), (away_score = 0),
      (home_score = 0), (total_line = 0), (spread_line = 0), (total = 0)(cp = 0), (cpoe = 0)))

############## Update columns for inside games wind/temperature (65 set to average inside stadium temperature ################
pdata <- pdata %>% replace_na(list(temp=65, na.rm=TRUE))
pdata<- pdata %>% replace_na(list(wind=0, na.rm=TRUE))

pdata <- pdata %>% select(-v2rm)


# model 1 - pass or run
#model 2 - redzone score or not
# model 3 - punt on down 4 or go for it

pdata <- pdata %>% filter(posteam !="NA")

##### making the variables factor ########
names <- c('shotgun', "no_huddle", "posteam",'posteam_type', "defteam", "sp", "qtr",
           "down", "play_type", "goal_to_go", "field_goal_result", "punt_blocked", "fourth_down_converted", "fourth_down_failed",
            "pass", "touchdown", "pass_touchdown", "rush_touchdown", "rush",
           "punt_attempt", "special", "drive_inside20", 
            "div_game", "roof", "surface", "drive_ended_with_score", "home_team", "away_team")

pdata[,names] <- lapply(pdata[,names], factor)

nums <- c("yardline_100", "quarter_seconds_remaining", "half_seconds_remaining", "game_seconds_remaining", "yards_gained",
          "temp", "wind", "series")

pdata[,nums] <- lapply(pdata[,nums], as.numeric)


#### convert our dates ########

library(lubridate)
pdata$game_date <- parse_date_time(pdata$game_date, "%x") #error but still works
str(pdata$game_date)

#Start time to time format
pdata$start_time <- str_remove(pdata$start_time, "1899-12-31")
summary(pdata$start_time)

pdata$start_time <- c(as.numeric(pdata$start_time))
pdata$start_time <- format(as.POSIXct((pdata$start_time) * 86400, origin = "1970-01-01", tz = "UTC"), "%H:%M:%S")
table(pdata$start_time)

start_group <- ifelse(pdata$start_time == "09:29:59" | pdata$start_time == "12:29:59" | pdata$start_time == "13:00:00" | pdata$start_time == "13:05:00",  "First Slate", 
               ifelse(pdata$start_time == "15:04:59" | pdata$start_time == "16:05:00" | pdata$start_time == "16:25:00" | pdata$start_time == "16:30:00" | pdata$start_time == "16:34:59" | pdata$start_time == "16:40:00", "Second Slate",
               ifelse(pdata$start_time == "18:29:59" | pdata$start_time == "18:40:00" | pdata$start_time == "19:10:00" | pdata$start_time == "20:15:00" | pdata$start_time == "20:19:59" | pdata$start_time == "22:19:59", "Prime Time", "NA")))     

table(start_group)
pdata <- cbind(pdata, start_group)
pdata$start_group <- as.factor(pdata$start_group)
levels(pdata$start_group)

# make chicago df
chidata <- pdata[grep("CHI", pdata$posteam), ]
head(chidata)

#for outcome 2--- make new df w just that in the redzone
chid <- which(chidata$drive_inside20==1)
chidataRED <- chidata[chid, ]
chidataRED$drive_ended_with_score <- na.omit(chidataRED$drive_ended_with_score)
summary(chidataRED$drive_ended_with_score)  #label imbalance

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



#test/train split
TRG_PCT=0.7
nr=nrow(PRchidata)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)

PRchiTrn=PRchidata[trnIndex,]   #training data with the randomly selected row-indices
PRchiTst = PRchidata[-trnIndex,]


## model 1 ##
#to include
m1subset= select(PRchiTrn, -c("game_id", "home_team", "away_team", "sp", "field_goal_result", "rush", "play_type",
                               "special","drive_ended_with_score", "punt_attempt", "pass_touchdown", "rush_touchdown",
                               "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked", "shotgun", "yards_gained",
                               "start_time", "defteam")) 



library(ranger)
# get the sheet and do the model with that
rpModel1=rpart(pass ~ ., data=m1subset, method= "class", 
               parms = list(split = "information"), 
               control = rpart.control(minsplit = 30), na.action=na.omit)

# plotting the tree
rpModel1$variable.importance
rpart.plot::prp(rpModel1, type=2, extra=100)

# train and test accuracy??
predTrn=predict(rpModel1, PRchiTrn, type='class')
table(pred = predTrn, true=PRchiTrn$pass)
mean(predTrn == PRchiTrn$pass)
table(pred = predict(rpModel1, PRchiTst, type='class'), true=PRchiTst$pass)
mean(predict(rpModel1, PRchiTst, type='class') == PRchiTst$pass)




###################### model 2 ############################ RZ score y or no
#test/train split--- 1052 rows- is this enough?
TRG_PCT=0.7
nr=nrow(chidataRED)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)

redchiTrn=chidataRED[trnIndex,]   #training data with the randomly selected row-indices
redchiTst = chidataRED[-trnIndex, ]

m2subset= select(redchiTrn, -c("game_id", "home_team", "away_team", "sp", "field_goal_result", 
                                "special", "punt_attempt", "pass_touchdown", "rush_touchdown", "posteam", "defteam",
                                "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked",
                                "temp", "start_time"))  # last 2 are gonna be in it once we fix them

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


######################################### model 3 ############## punt or go for it ###############
# dataset with down = 4
chi4th$posteam
cc <- which(chidata$down == 4)
chi4th <- chidata[cc, ]
cc <- which(chi4th$posteam == "CHI")
chi4th <- chidata[cc, ]

chi4th$posteam 
# #only 130 if just the bears on offense
# 
# cc <- which(divdata$down == 4)
# div4th <- divdata[cc, ]     #1033 with division


TRG_PCT=0.8
nr=nrow(chi4th)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)

c4trn=chi4th[trnIndex,]   #training data with the randomly selected row-indices
c4tst = chi4th[-trnIndex, ]

m3subset= select(c4trn, -c("game_id", "home_team", "away_team", "sp", "drive_ended_with_score", "posteam_score", "defteam_score", "play_type",
                            "special", "pass_touchdown", "rush_touchdown", "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked", "drive_inside20", "goal_to_go",
                            "temp", "start_time", "shotgun", "pass", "rush", "yards_gained", "field_goal_result", "down", "ydstogo"))

summary(m3subset$punt_attempt)
rpModel3=rpart(punt_attempt ~ ., data=m3subset, method= "class", 
               parms = list(split = "information"), 
               control = rpart.control(minsplit = 30), na.action=na.omit)

# plotting the tree
rpModel3$variable.importance
rpart.plot::prp(rpModel3, type=2, extra=100)

# train and test accuracy??
predTrn=predict(rpModel4, FGtrn, type='class')
table(pred = predTrn, true=FGtrn$field_goal_result)
mean(predTrn == FGtrn$field_goal_result)
table(pred = predict(rpModel2, FGtst, type='class'), true=FGtst$field_goal_result)
mean(predict(rpModel4, FGtst, type='class') == FGtst$field_goal_result)




########################  FG model ###########################
#### predict if a FG was made ####

FGdata <- FGdata %>% filter(FGdata$field_goal_result == "made" | FGdata$field_goal_result == "missed") %>% droplevels()


TRG_PCT=0.7
nr=nrow(FGdata)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)

FGtrn=FGdata[trnIndex,]   #training data with the randomly selected row-indices
FGtst = FGdata[-trnIndex, ]

m4subset= select(FGtrn, -c("game_id", "home_team", "away_team", "sp", "drive_ended_with_score", "posteam_score", "defteam_score",
                            "special", "punt_attempt", "pass_touchdown", "rush_touchdown", "posteam", "defteam",
                            "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked", "drive_inside20", "goal_to_go",
                            "temp", "start_time"))  # last 2 are gonna be in it once we fix them


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
