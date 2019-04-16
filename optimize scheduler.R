# sports-team-scheduler version 0.2 #

library('tidyverse')
getwd()
setwd("C:/R_Script_Data/Optimize Scheduler")


# all_matches_import <- read_csv("tbl_All_Possible_Matchups.csv") # replaced data import with hard coded lists and crossing function

## Generate list of all possible team matchups ##
teamList <- c('Team 01','Team 02','Team 03','Team 04','Team 05','Team 06','Team 07','Team 08','Team 09','Team 10','Team 11','Team 12','Team 13','Team 14','Team 15')
teamList_t <- tibble(Teams = teamList)
team_matchups_all <- crossing(teamList_t, teamList_t)
names(team_matchups_all) <- c("HomeTeam","AwayTeam")
team_matchups_all <- filter(team_matchups_all, HomeTeam != AwayTeam)
team_matchups_all <-rowid_to_column(team_matchups_all, var="matchID") #add matchID column as index for matching later

## Five week schedule, games starting from 6p to 10p, with 3 ball fields ##

# GameWeeks_t <- tibble(GameWeekNum = c("Week 01", "Week 02","Week 03","Week 04", "Week 05","Week 06")) # 6 week game option
GameWeeks_t <- tibble(GameWeekNum = c("Week 01", "Week 02","Week 03","Week 04", "Week 05"))
GameHours_t <- tibble(HourOfGame = c(6,7,8,9,10)) # other game times could be added here 
GameBallFields_t <- tibble(BallFieldNum = c("F1","F2","F3")) # generic IDs for ball fields

game_slots_all <- crossing(GameWeeks_t,GameHours_t,GameBallFields_t)
# game_slots_all <- filter(game_slots_all,HourOfGame != 10) # remove all of the 10pm games
# game_slots_all <- bind_rows(game_slots_all,crossing(GameWeeks_t[1:3,],GameHours_t[5,],GameBallFields_t[1,])) # adds the 3 pre-agreed 10pm games

game_slots_all <- rowid_to_column(game_slots_all, var = "GameSlotID") #add GameSlotID column as index for matching later



matches_selected <- sample_n(team_matchups_all,0) #Creates tibble with no rows

away_teams_t <- (team_matchups_all %>% group_by(AwayTeam) %>% summarise(awayCount=n()))[,1] #creates away teams list as tibble
away_games_counter <- add_column(away_teams_t, awayCount = 0) #create away games count tibble & initialize game count


## Begin loop section
# iNum <- 1 # this line is for use in testing

for (iNum in seq_along(teamList)) #teamList has been 15
  {

  print(iNum)
  cur_team_Home <- filter(team_matchups_all, HomeTeam == teamList[iNum])
  # print(cur_team_Home)
  away_games_shortjoin <- filter(away_games_counter, awayCount < 5)
  # print(away_games_shortjoin)
  cur_team_Home_short <- cur_team_Home %>% inner_join(away_games_shortjoin)
  # print(cur_team_Home_short)

  
  if (iNum == 1) matches_selected <- sample_n(cur_team_Home,5) else {
    if (length(pull(cur_team_Home_short[,'AwayTeam']))<5) { 
      matches_selected <- bind_rows(matches_selected, cur_team_Home_short)} else 
    matches_selected <- bind_rows(matches_selected, sample_n(cur_team_Home_short,5))}
  # print(matches_selected)
  
  away_games_counter <- (matches_selected %>% group_by(AwayTeam) %>% summarise(awayCount=n()))
  away_games_counter <- right_join(away_games_counter, away_teams_t)
  away_games_counter <- replace_na(away_games_counter, list(awayCount=0))
  # print(away_games_counter)
  
  

} # end of for loop

## one final pass for last case

print(iNum)
cur_team_Home <- filter(team_matchups_all, HomeTeam == teamList[iNum])
# print(cur_team_Home)
away_games_shortjoin <- filter(away_games_counter, awayCount < 5)
# print(away_games_shortjoin)
cur_team_Home_short <- cur_team_Home %>% inner_join(away_games_shortjoin)
# print(cur_team_Home_short)


if (iNum == 1) matches_selected <- sample_n(cur_team_Home,5) else {
  if (length(pull(cur_team_Home_short[,'AwayTeam']))<5) { 
    matches_selected <- bind_rows(matches_selected, cur_team_Home_short)} else 
      matches_selected <- bind_rows(matches_selected, sample_n(cur_team_Home_short,5))}
# print(matches_selected)

away_games_counter <- (matches_selected %>% group_by(AwayTeam) %>% summarise(awayCount=n()))
away_games_counter <- right_join(away_games_counter, away_teams_t)
away_games_counter <- replace_na(away_games_counter, list(awayCount=0))
# print(away_games_counter)
# print((matches_selected %>% group_by(HomeTeam) %>% summarise(HomeCount=n()))) # Counts how many home games for each team
# matches_selected <- inner_join(matches_selected[,1:4], away_games_counter, by = c("AwayTeam")) #updates away game counts

matches_selected <- rowid_to_column(matches_selected, var = "SelectedMatchID") #add GameSlotID column as index for matching later

## End match selection section


######### WORKING DRAFT  SECTION ##############

########## now to put the selected matches into different slots

game_slots_filled <- game_slots_all %>%
  add_column(SelectedMatchID = NA) # add the SelectedMatchID column (foreign key for team_matchups_all)

matches_selected %>% 
  select(SelectedMatchID) %>%
  sample_n(74) # this randomly samples the matches selected. I think I could use it to randomize the matchups into their gameslots.
  
sample_n(team_matchups_all,75)


############### Extras

# How to find all unique combinations for a data set ###### not working yet!
# teams_t2 <- teams_t[,1]
# dimnames(crossing(teams_t, nesting(pull(teams_t[,1])))) # <- c('HomeTeam', 'AwayTeam'))
# expand(teams_t, teams_t2,nesting(pull(teams_t[,1]), pull(teams_t2[,1])))

# How to Count rows as a single value 
# summarise(filter(all_matches_import, AwayTeam == 'Team 14')[,3], n=n())[[1,1]]
# pull(summarise(filter(all_matches_import, AwayTeam == 'Team 14')[,3], n=n()))

##### How to clear variables in memory
# rm(list=c("data_1", "data_2", "data_3")) # where "data_1" is one of the objects to be removed
# gc() #This is the garbage cleaning 


# rename(team_matchups,replace = c("Teams" = "HomeTeam",Teams1 = "AwayTeam"))
# str(team_matchups)

# This is how to prompt the user and store their input in a variable. #
# user <- readline("Give the username : ")
# passw <- readline("Give the password : ")



