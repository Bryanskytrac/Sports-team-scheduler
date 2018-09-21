# sports-team-scheduler version 0.1 #

library('tidyverse')
getwd()
setwd("C:/R_Script_Data/Optimize Scheduler")

all_matches_import <- read_csv("tbl_All_Possible_Matchups.csv")
# all_game_slots_import <- read_csv("tbl_All_Games.csv") #Not used currently
all_game_slots_import2 <- read.csv("Q_All_Standard_Time_Games_v2mod.csv") # Five week schedule, games starting from 6p to 10p
# summary(all_matches_import)
summary(all_game_slots_import2)

matches_selected <- sample_n(all_matches_import,0) #Creates empty tibble?
#teamNum <- c('01','02','03','04','05','06','07','08','09','10','11','12','13','14','15') #not needed now with full team names

teamList <- c('Team 01','Team 02','Team 03','Team 04','Team 05','Team 06','Team 07','Team 08','Team 09','Team 10','Team 11','Team 12','Team 13','Team 14','Team 15')
teams_t <- (all_matches_import %>% group_by(AwayTeam) %>% summarise(awayCount=n()))[,1]

# away_games_counter <- tibble(teams_t, awayCount = 0) #this is to initialize the tibble
away_games_counter <- add_column(teams_t, awayCount = 0) #this is to initialize the tibble

## Begin loop section
# iNum <- 4 # this line is for use in testing

for (iNum in 1:15) {

  print(iNum)
  # cur_team_Home <- filter(all_matches_import, HomeTeam == paste0('Team ',teamNum[iNum]))
  cur_team_Home <- filter(all_matches_import, HomeTeam == teamList[iNum])
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
  away_games_counter <- right_join(away_games_counter, teams_t)
  away_games_counter <- replace_na(away_games_counter, list(awayCount=0))
  # print(away_games_counter)
  
  

} # end of for loop

## one final pass for last case

print(iNum)
# cur_team_Home <- filter(all_matches_import, HomeTeam == paste0('Team ',teamNum[iNum]))
cur_team_Home <- filter(all_matches_import, HomeTeam == teamList[iNum])
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
away_games_counter <- right_join(away_games_counter, teams_t)
away_games_counter <- replace_na(away_games_counter, list(awayCount=0))
# print(away_games_counter)
# print((matches_selected %>% group_by(HomeTeam) %>% summarise(HomeCount=n()))) # Counts how many home games for each team
# matches_selected <- inner_join(matches_selected[,1:4], away_games_counter, by = c("AwayTeam")) #updates away game counts



## End loop section



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
