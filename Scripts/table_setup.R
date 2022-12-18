require(tidyverse)
library(httr)
library(jsonlite)
library(readr)
require(DBI)
require(dbplyr)
require(RSQLite)
require(stringr)

con <- dbConnect(SQLite(), "database/SHLHistory.db")
dbConnect(SQLite(), "database/SHLHistory")

shl_rs_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, Pos, teamName, Season, GamesPlayed, Goals, Assists, Points, PlusMinus, PenaltyMinutes, Hits, Shots, ShotsBlocked, MinutesPlayed, PPGoals, PPAssists, PPPoints, PPMinutes, PKGoals, PKAssists, PKPoints, PKMinutes, GameWinningGoals, FaceoffsTotal, FaceoffWins, FightsWon, FightsLost, GvA, TkA
    FROM
    (SELECT *
    FROM shlSkaters
    INNER JOIN (SELECT playerMaster.Name as playerName, FHMIDS FROM playerMaster)
    ON shlSkaters.FHMID = FHMIDS
    INNER JOIN (SELECT teams.Name as teamName, fhmFranchiseId FROM teams)
    ON shlSkaters.FranchiseId = fhmFranchiseId
    WHERE isPlayoffs = 0)
    "
  )

shl_ps_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, Pos, teamName, Season, GamesPlayed, Goals, Assists, Points, PlusMinus, PenaltyMinutes, Hits, Shots, ShotsBlocked, MinutesPlayed, PPGoals, PPAssists, PPPoints, PPMinutes, PKGoals, PKAssists, PKPoints, PKMinutes, GameWinningGoals, FaceoffsTotal, FaceoffWins, FightsWon, FightsLost, GvA, TkA
    FROM
    (SELECT *
    FROM shlSkaters
    INNER JOIN (SELECT playerMaster.Name as playerName, FHMIDS FROM playerMaster)
    ON shlSkaters.FHMID = FHMIDS
    INNER JOIN (SELECT teams.Name as teamName, fhmFranchiseId FROM teams)
    ON shlSkaters.FranchiseId = fhmFranchiseId
    WHERE isPlayoffs = 1)
    "
  )

shl_franchise_rs_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, teamName, GamesPlayed, Goals, Assists, Points, PlusMinus, PenaltyMinutes, Hits, Shots, ShotsBlocked, MinutesPlayed, PPGoals, PPAssists, PPPoints, PPMinutes, PKGoals, PKAssists, PKPoints, PKMinutes, GameWinningGoals, FaceoffsTotal, FaceoffWins, FightsWon, FightsLost FROM
    (SELECT * FROM shlFranchiseCareerRegularSeason
    INNER JOIN (SELECT playerMaster.Name as playerName, FHMIDS FROM playerMaster)
    ON shlFranchiseCareerRegularSeason.FHMID = FHMIDS
    INNER JOIN (SELECT teams.Name as teamName, fhmFranchiseId FROM teams)
    ON shlFranchiseCareerRegularSeason.FranchiseId = fhmFranchiseId)
    "
  )

shl_franchise_ps_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, teamName, GamesPlayed, Goals, Assists, Points, PlusMinus, PenaltyMinutes, Hits, Shots, ShotsBlocked, MinutesPlayed, PPGoals, PPAssists, PPPoints, PPMinutes, PKGoals, PKAssists, PKPoints, PKMinutes, GameWinningGoals, FaceoffsTotal, FaceoffWins, FightsWon, FightsLost FROM
    (SELECT * FROM shlFranchiseCareerPlayoffs
    INNER JOIN (SELECT playerMaster.Name as playerName, FHMIDS FROM playerMaster)
    ON shlFranchiseCareerPlayoffs.FHMID = FHMIDS
    INNER JOIN (SELECT teams.Name as teamName, fhmFranchiseId FROM teams)
    ON shlFranchiseCareerPlayoffs.FranchiseId = fhmFranchiseId)
    "
  )

shl_career_rs_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, GamesPlayed, Goals, Assists, Points, PlusMinus, PenaltyMinutes, Hits, Shots, ShotsBlocked, MinutesPlayed, PPGoals, PPAssists, PPPoints, PPMinutes, PKGoals, PKAssists, PKPoints, PKMinutes, GameWinningGoals, FaceoffsTotal, FaceoffWins, FightsWon, FightsLost FROM
    (SELECT * FROM shlCareerRegularSeason
    INNER JOIN (SELECT playerMaster.Name as playerName, FHMIDS FROM playerMaster)
    ON shlCareerRegularSeason.FHMID = FHMIDS)
    "
  )
shl_career_ps_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, GamesPlayed, Goals, Assists, Points, PlusMinus, PenaltyMinutes, Hits, Shots, ShotsBlocked, MinutesPlayed, PPGoals, PPAssists, PPPoints, PPMinutes, PKGoals, PKAssists, PKPoints, PKMinutes, GameWinningGoals, FaceoffsTotal, FaceoffWins, FightsWon, FightsLost FROM
    (SELECT * FROM shlCareerPlayoffs
    INNER JOIN (SELECT playerMaster.Name as playerName, FHMIDS FROM playerMaster)
    ON shlCareerPlayoffs.FHMID = FHMIDS)
    "
  )

return_table <- function(type, season) {
  stats_table <- switch(
    type,
    "Career" = ifelse(season == "Playoffs", return(shl_career_ps_stats), return(shl_career_rs_stats)),
    "Franchise" = ifelse(season == "Playoffs", return(shl_franchise_ps_stats), return(shl_franchise_rs_stats)),
    "Season" = ifelse(season == "Playoffs", return(shl_ps_stats), return(shl_rs_stats)),
  )
  return(stats_table)
}
