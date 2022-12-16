require(tidyverse)
library(httr)
library(jsonlite)
library(readr)
require(DBI)
require(dbplyr)
require(RSQLite)
require(stringr)




leagueList = list("SHL", "SMJHL")
league = "SHL"

#####################
# S53=2019
#####################

year = readRDS("year")
season <- readRDS("season")
startLink <- paste0("https://simulationhockey.com/games/shl/S",season,"/csv/")
{
team_data <- read_delim("team_data.csv",delim=";")#read_delim(url(paste0(startLink,"team_data.csv")), delim=";")
player_goalie_career_stats_rs<- read_delim("player_goalie_career_stats_rs.csv",delim=";") #read_delim(url(paste0(startLink,"player_goalie_career_stats_rs.csv")), delim=";")
player_goalie_career_stats_po<- read_delim("player_goalie_career_stats_po.csv",delim=";")#read_delim(url(paste0(startLink,"player_goalie_career_stats_po.csv")), delim=";")
player_skater_career_stats_rs <-read_delim("player_skater_career_stats_rs.csv",delim=";") #read_delim(url(paste0(startLink,"player_skater_career_stats_rs.csv")), delim=";")
player_skater_career_stats_po <- read_delim("player_skater_career_stats_po.csv",delim=";")#read_delim(url(paste0(startLink,"player_skater_career_stats_po.csv")), delim=";")
player_master <-read_delim("player_master.csv",delim=";") #read_delim(url(paste0(startLink,"player_master.csv")), delim=";")
player_ratings <- read_delim("player_ratings.csv",delim=";")#read_delim(url(paste0(startLink,"player_ratings.csv")), delim=";")




}
##############################################################
#Cleaning up all the data
##############################################################
{

player_ratings$pos <- ""
for(b in 1:length(player_ratings$G)) {
  if (player_ratings$LD[b] >=15) {
    player_ratings$pos[b] <- "D"
  } else if (player_ratings$C[b] >= 15){
    player_ratings$pos[b] <- "F"
  }
}

position <- player_ratings %>%
  select(PlayerId, pos)


player_master <- player_master %>%
  filter(TeamId >= 0) %>%
  select(Name = `Last Name`, FHMIDS = PlayerId)

player_goalie_career_stats_rs <- player_goalie_career_stats_rs %>%
  filter(Year == year)
player_goalie_career_stats_po <- player_goalie_career_stats_po %>%
  filter(Year == year)
player_skater_career_stats_rs <- player_skater_career_stats_rs %>%
  filter(Year == year)
player_skater_career_stats_po <- player_skater_career_stats_po %>%
  filter(Year == year)

player_skater_career_stats_po$isPlayoffs <- 1
player_skater_career_stats_rs$isPlayoffs <- 0
player_skater_career_stats_po$PPP <- player_skater_career_stats_po$`PP G` + player_skater_career_stats_po$`PP A`
player_skater_career_stats_rs$PPP <- player_skater_career_stats_rs$`PP G` + player_skater_career_stats_rs$`PP A`
player_skater_career_stats_po$PKP <- player_skater_career_stats_po$`SH G` + player_skater_career_stats_po$`SH A`
player_skater_career_stats_rs$PKP <- player_skater_career_stats_rs$`SH G` + player_skater_career_stats_rs$`SH A`
player_skater_career_stats_po$Points <- player_skater_career_stats_po$G + player_skater_career_stats_po$A
player_skater_career_stats_rs$Points <- player_skater_career_stats_rs$G + player_skater_career_stats_rs$A
player_skater_career_stats_po$Season <- season
player_skater_career_stats_rs$Season <- season
player_goalie_career_stats_po$Season <- season
player_goalie_career_stats_rs$Season <- season
player_goalie_career_stats_po$isPlayoffs <- 1
player_goalie_career_stats_rs$isPlayoffs <- 0
player_skater_career_stats_po$FightsLost <- player_skater_career_stats_po$Fights - player_skater_career_stats_po$`Fights Won`
player_skater_career_stats_rs$FightsLost <- player_skater_career_stats_rs$Fights - player_skater_career_stats_rs$`Fights Won`

if(league == "SHL"){
  player_skater_career_stats_po$LeagueId <- 1
  player_skater_career_stats_rs$LeagueId <- 1
  player_goalie_career_stats_po$LeagueId <- 1
  player_goalie_career_stats_rs$LeagueId <- 1


} else if(league == "SMJHL") {
  player_skater_career_stats_po$LeagueId <- 2
  player_skater_career_stats_rs$LeagueId <- 2
  player_goalie_career_stats_po$LeagueId <- 2
  player_goalie_career_stats_rs$LeagueId <- 2
}

player_skater_career_stats_po <- player_skater_career_stats_po %>%
  select( -c(Year, `League Id`, GR))
player_skater_career_stats_rs <- player_skater_career_stats_rs %>%
  select( -c(Year, `League Id`, GR))
player_goalie_career_stats_po <- player_goalie_career_stats_po %>%
  select( -c(Year, `League Id`, GR))
player_goalie_career_stats_rs <- player_goalie_career_stats_rs %>%
  select( -c(Year, `League Id`, GR))



}

playerSkaters <- rbind(player_skater_career_stats_po,player_skater_career_stats_rs)
playerGoalies <- rbind(player_goalie_career_stats_rs,player_goalie_career_stats_po)
playerSkaters <- merge(playerSkaters, position)

playerSkaters <- playerSkaters %>%
  select(FranchiseId = `Team Id`, FHMID = PlayerId, Pos = pos, LeagueId, Season, isPlayoffs, GamesPlayed = GP,
         Goals = G, Assists = A, Points, PlusMinus = `+/-`, PenaltyMinutes = PIM, Hits =  HIT, Shots = SOG, ShotsBlocked = SB,
         MinutesPlayed = TOI, PPGoals = `PP G`, PPAssists = `PP A`, PPPoints = PPP, PPMinutes = PPTOI,
         PKGoals = `SH G`, PKAssists =  `SH A`, PKPoints = PKP, PKMinutes = SHTOI,
         GameWinningGoals =  GWG, FaceoffsTotal = FO, FaceoffWins = `FO W`, FightsWon = `Fights Won`, FightsLost, GvA, TkA)

playerGoalies <- playerGoalies %>%
  select(FranchiseID = `Team Id`, FHMID = PlayerId, LeagueId, Season, isPlayoffs, GamesPlayed = GP, Wins = W, Losses = L,
         OverTimeLosses = `T/OL`, Minutes = Min, Shutouts = SO, GoalsAgainst = GA, ShotsAgainst = SA, EmptyGoalAgainst = ENG)



con <- dbConnect(SQLite(), "database/SHLHistory.db")
dbConnect(SQLite(), "database/SHLHistory")

#AllPlayers <- dbGetQuery(con, "SELECT * FROM playerMaster")

#NewPlayers <- subset(player_master, !(player_master$FHMIDS %in% AllPlayers$FHMIDS))

#dbWriteTable(con, "playerMaster", NewPlayers, overwrite =F, append = T, row.names=FALSE)
dbWriteTable(con, "shlSkaters", playerSkaters, overwrite = F, append = T, row.names=FALSE)
dbWriteTable(con, "shlGoalies", playerGoalies, overwrite = F, append = T, row.names=FALSE)


season <- season +1
year <- year +1


saveRDS(season, "season")
saveRDS(year, "year")


#playerRegular <- dbGetQuery(con, "SELECT FHMID, sum(GamesPlayed) AS GamesPlayed, sum(Goals) AS Goals, sum(Assists) AS Assists, sum(Points) AS Points, sum(PlusMinus) AS PlusMinus, sum(PenaltyMinutes) AS PenaltyMinutes, sum(Hits) AS Hits, sum(Shots) AS Shots, sum(ShotsBlocked) AS ShotsBlocked, sum(MinutesPlayed) AS MinutesPlayed, sum(PPGoals) AS PPGoals, sum(PPAssists) AS PPAssists, sum(PPPoints) AS PPPoints, sum(PPMinutes) AS PPMinutes, sum(PKGoals) AS PKGoals, sum(PKAssists) AS PKAssists, sum(PKPoints) AS PKPoints, sum(PKMinutes) AS PKMinutes, sum(GameWinningGoals) AS GameWinningGoals, sum(FaceoffsTotal) AS FaceoffsTotal, sum(FightsWon) AS FightsWon, sum(FightsLost) AS FightsLost,  sum(FaceoffWins) AS FaceoffWins FROM shlSkaters WHERE isPlayoffs = 1 GROUP BY FHMID")

#write.csv(playerRegular, "Leaders.csv")
