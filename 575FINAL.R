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
          ,"score_differential","ep","epa","total_home_epa","total_away_epa","total_home_rush_epa","total_away_rush_epa","total_home_pass_epa"
          ,"total_away_pass_epa","air_epa","yac_epa","comp_air_epa","comp_yac_epa","total_home_comp_air_epa","total_away_comp_air_epa","total_home_comp_yac_epa"
          ,"total_away_comp_yac_epa","total_home_raw_air_epa","total_away_raw_air_epa","total_home_raw_yac_epa","total_away_raw_yac_epa","wp","def_wp"
          ,"home_wp","away_wp","wpa","home_wp_post","away_wp_post","vegas_wp","vegas_home_wp","total_home_rush_wpa","total_away_rush_wpa","total_home_pass_wpa"
          ,"total_away_pass_wpa","air_wpa","yac_wpa","comp_air_wpa","comp_yac_wpa","total_home_comp_air_wpa","total_away_comp_air_wpa","total_home_comp_yac_wpa"
          ,"total_away_comp_yac_wpa","total_home_raw_air_wpa","total_away_raw_air_wpa","total_home_raw_yac_wpa","total_away_raw_yac_wpa","first_down_rush"
          ,"first_down_pass","order_sequence","weather","nfl_api_id","end_clock_time","drive_real_start_time","drive_play_count"
          ,"drive_time_of_possession","drive_first_downs","away_score","home_score","location","result","total","spread_line","total_line"
          ,"stadium_id","success","play", "play_type_nfl", "side_of_field", "drive_start_yard_line", "drive_end_yard_line", "end_yard_line", "rush_attempt", "pass_attempt",
          "special_teams_play", "penalty_player_name", "home_coach", "away_coach", "passer", "rusher", "receiver", "series_success", "cp", "cpoe", "yrdln", "ydsnet",
          "no_score_prob", "fg_prob")



pdata <- pdata %>% select(-v2rm)


# model 1 - pass or run
#model 2 - redzone score or not
# model 3 - punt on down 4 or go for it


pdata<- pdata %>% replace_na(list(wind=0, na.rm=TRUE))
# what to do for temperature??


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

#can't get this one to work
pdata$start_time <- str_remove(pdata$start_time, "1899-12-31")
summary(pdata$start_time)
pdata$start_time<-parse_date(pdata$start_time,  format = "%H:%M:%S")


# make chicago df
chidata <- pdata[grep("CHI", pdata$game_id), ]
head(chidata)

#for outcome 2--- make new df w just that in the redzone
chid <- which(chidata$drive_inside20==1)
chidataRED <- chidata[chid, ]
chidataRED$drive_ended_with_score <- na.omit(chidataRED$drive_ended_with_score)
summary(chidataRED$drive_ended_with_score)




#include the 3 teams also in the the conference-MIN,DET,GB

gbdata <- pdata[grep("GB", pdata$game_id), ]
head(gbdata)

dtdata <- pdata[grep("DET", pdata$game_id), ]
head(dtdata)

mndata <- pdata[grep("MIN", pdata$game_id), ]
head(mndata)

divdata <- rbind(gbdata, dtdata, mndata, chidata)
dim(divdata)   #11116 x 42

#so our dataframes are pdata, chidata, divdata

# make pass vs rush split so only include rows where these two happened
##########  outcome 1:: pass vs run ###################

Pchidata <- chidata[grep("pass", chidata$play_type), ]
Rchidata <- chidata[grep("run", chidata$play_type), ]
PRchidata <- rbind(Pchidata, Rchidata)
dim(PRchidata)  #2020 x 42



ggplot(chidata[!is.na(chidata$play_type),], aes( x = play_type)) + geom_bar(fill="lightblue") + 
  labs(title = "Play Types for Chicago Bears 2019") + theme_bw()


ggplot(chidata, aes(x = game_date)) + geom_bar(fill="lightblue", ) + 
  labs(title = "Chicago Bears- Plays per game") + theme_bw() #+


#this is what will rotate your text~ 
# + theme(axis.text.x = element_text(angle = 45))
# 
# + theme_bw()   # add this too if you're doing ggplot!!!!! presentation quality haha

chidata %>% 
  filter(down == 4 & punt_attemmpt == 0) %>% select(ydstogo) %>% head()




# now we can analyze pass vs rush


PRchidata$pass <- ifelse(PRchidata$pass==1,1,0)
PRchidata$pass <- as.factor(PRchidata$pass)



#test/train split
TRG_PCT=0.7
nr=nrow(PRchidata)
trnIndex = sample(1:nr, size = round(TRG_PCT*nr), replace=FALSE)

PRchiTrn=PRchidata[trnIndex,]   #training data with the randomly selected row-indices
PRchiTst = PRchidata[-trnIndex,]


## model 1 ##
#to include
m1subset= select(PRchidata, -c("game_id", "home_team", "away_team", "sp", "field_goal_result", "rush", "pass",
                             "special","drive_ended_with_score", "punt_attempt", "pass_touchdown", "rush_touchdown",
                             "touchdown", "fourth_down_failed", "fourth_down_converted", "punt_blocked",
                             "temp", "time_of_day"))  # last 2 are gonna be in it once we fix them



library(ranger)
# get the sheet and do the model with that
rpModel1=rpart(play_type ~ ., data=m1subset, method= "class", 
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

# JUST DO AUC FOR THIS PRELIMINARY ASSIGNMENT- LATER WE'LL DO ROC AND OTHER EVAL METRICS

# library(pROC)
# PRchiTst$play_type <- na.omit(PRchiTst$play_type)
# PRchiTrn_p<- predict(rpModel1, PRchiTrn)
# PRchiTst_p<- predict(rpModel1, PRchiTst)
# 
# rocTrn <- roc(PRchiTrn$play_type, PRchiTrn_p[,2], levels=c(0, 1))
# rocTst <- roc(PRchiTst$play_type, PRchiTst_p[,2], levels=c(0, 1))
# 
# plot.roc(rocTrn, col='blue', legacy.axes = TRUE)
# plot.roc(rocTst, col='red', add=TRUE)
# legend("bottomright", legend=c("Training", "Test"),
#        col=c("blue", "red"), lwd=2, cex=0.8, bty='n')
# 
# 
# plot.roc(rpModel1, )

#if you know another way of evaluating test/train accuracy please lmk i'm not sure if that above
#code is actually saying anything 


