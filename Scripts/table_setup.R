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

return_era_seasons <- function(era) {
  print(era)
  seasons <- switch(
    era,
    "Dead Puck Era (S1-S5)" = c(1, 5),
    "Experimental Era (S6-S8)" = c(6, 8),
    "Inflation Era (S9-S11)" = c(9, 11),
    "52-Game Era (S21-S24)" = c(21, 24),
    "Realism Era (S12-S20)" = c(12, 20),
    "Realism Era (S25-S52)" = c(25, 52),
    "STHS Era (S1-S52)" = c(1, 52),
    "FHM6 Era" = c(53, 65),
    "66-Game FHM6 Era" = c(57, 65),
    "50-Game FHM6 Era" = c(53, 56),
    "FHM8 Records" = c(66, max(shl_rs_stats$Season))
  )
  return(seasons)
}

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
    INNER JOIN (SELECT teams.Name as teamName, fhmFranchiseId FROM teams WHERE League = 'SHL')
    ON shlSkaters.FranchiseId = fhmFranchiseId
    WHERE isPlayoffs = 0 AND LeagueID = 1)
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
    INNER JOIN (SELECT teams.Name as teamName, fhmFranchiseId FROM teams WHERE League = 'SHL')
    ON shlSkaters.FranchiseId = fhmFranchiseId
    WHERE isPlayoffs = 1 AND LeagueID = 1)
    "
  )

shl_franchise_rs_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, teamName, GamesPlayed, Goals, Assists, Points, PlusMinus, PenaltyMinutes, Hits, Shots, ShotsBlocked, MinutesPlayed, PPGoals, PPAssists, PPPoints, PPMinutes, PKGoals, PKAssists, PKPoints, PKMinutes, GameWinningGoals, FaceoffsTotal, FaceoffWins, FightsWon, FightsLost FROM
    ((SELECT * FROM shlFranchiseCareer WHERE isPlayoffs = 0)
    INNER JOIN (SELECT playerMaster.Name as playerName, FHMIDS FROM playerMaster)
    ON FHMID = FHMIDS
    INNER JOIN (SELECT teams.Name as teamName, fhmFranchiseId FROM teams WHERE League = 'SHL')
    ON FranchiseId = fhmFranchiseId)
    "
  )

shl_franchise_ps_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, teamName, GamesPlayed, Goals, Assists, Points, PlusMinus, PenaltyMinutes, Hits, Shots, ShotsBlocked, MinutesPlayed, PPGoals, PPAssists, PPPoints, PPMinutes, PKGoals, PKAssists, PKPoints, PKMinutes, GameWinningGoals, FaceoffsTotal, FaceoffWins, FightsWon, FightsLost FROM
    ((SELECT * FROM shlFranchiseCareer WHERE isPlayoffs = 1)
    INNER JOIN (SELECT playerMaster.Name as playerName, FHMIDS FROM playerMaster)
    ON FHMID = FHMIDS
    INNER JOIN (SELECT teams.Name as teamName, fhmFranchiseId FROM teams WHERE League = 'SHL')
    ON FranchiseId = fhmFranchiseId)
    "
  )

shl_career_rs_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, GamesPlayed, Goals, Assists, Points, PlusMinus, PenaltyMinutes, Hits, Shots, ShotsBlocked, MinutesPlayed, PPGoals, PPAssists, PPPoints, PPMinutes, PKGoals, PKAssists, PKPoints, PKMinutes, GameWinningGoals, FaceoffsTotal, FaceoffWins, FightsWon, FightsLost FROM
    ((SELECT * FROM shlCareers WHERE isPlayoffs = 0)
    INNER JOIN (SELECT playerMaster.Name as playerName, FHMIDS FROM playerMaster)
    ON FHMID = FHMIDS)
    "
  )
shl_career_ps_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, GamesPlayed, Goals, Assists, Points, PlusMinus, PenaltyMinutes, Hits, Shots, ShotsBlocked, MinutesPlayed, PPGoals, PPAssists, PPPoints, PPMinutes, PKGoals, PKAssists, PKPoints, PKMinutes, GameWinningGoals, FaceoffsTotal, FaceoffWins, FightsWon, FightsLost FROM
    ((SELECT * FROM shlCareers WHERE isPlayoffs = 1)
    INNER JOIN (SELECT playerMaster.Name as playerName, FHMIDS FROM playerMaster)
    ON FHMID = FHMIDS)
    "
  )
achievements_table <-
  dbGetQuery(
    con,
    "
    SELECT Name, Season, Achievement, Type FROM
    (SELECT * FROM playerMaster
    INNER JOIN achievement ON playerMaster.FHMIDS = achievement.FHMID)
    "
  )

hof_eligibility <-
  dbGetQuery(
    con,
    "
    SELECT Name, lastSeason FROM
    (SELECT * FROM playerMaster
    INNER JOIN
    (SELECT FHMID, max(Season) AS lastSeason FROM shlSkaters group by FHMID)
    ON playerMaster.FHMIDS = FHMID)
    "
  )

shl_goalie_rs_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, teamName, Season, GamesPlayed, Wins, Losses, OvertimeLosses, Minutes, Shutouts, GoalsAgainst, ShotsAgainst, EmptyGoalAgainst FROM
    (SELECT * FROM shlGoalies
    INNER JOIN (SELECT Name as playerName, FHMIDS FROM playerMaster)
    ON FHMID = FHMIDS
    INNER JOIN (SELECT Name as teamName, fhmFranchiseId  FROM teams WHERE League = 'SHL')
    ON FranchiseID = fhmFranchiseId
    WHERE isPlayoffs = 0 AND LeagueId = 1)
    "
  )

shl_goalie_ps_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, teamName, Season, GamesPlayed, Wins, Losses, OvertimeLosses, Minutes, Shutouts, GoalsAgainst, ShotsAgainst, EmptyGoalAgainst FROM
    (SELECT * FROM shlGoalies
    INNER JOIN (SELECT Name as playerName, FHMIDS FROM playerMaster)
    ON FHMID = FHMIDS
    INNER JOIN (SELECT Name as teamName, fhmFranchiseId  FROM teams WHERE League = 'SHL')
    ON FranchiseID = fhmFranchiseId
    WHERE isPlayoffs = 1 AND LeagueId = 1)
    "
  )

shl_goalie_franchise_rs_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, teamName, GamesPlayed, Wins, Losses, OvertimeLosses, Minutes, Shutouts, GoalsAgainst, ShotsAgainst, EmptyGoalAgainst FROM
    (SELECT * FROM shlGoaliesFranchiseCareer
    INNER JOIN (SELECT Name as playerName, FHMIDS FROM playerMaster)
    ON FHMID = FHMIDS
    INNER JOIN (SELECT Name as teamName, fhmFranchiseId  FROM teams WHERE League = 'SHL')
    ON FranchiseID = fhmFranchiseId
    WHERE isPlayoffs = 0)
    "
  )

shl_goalie_franchise_ps_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, teamName, GamesPlayed, Wins, Losses, OvertimeLosses, Minutes, Shutouts, GoalsAgainst, ShotsAgainst, EmptyGoalAgainst FROM
    (SELECT * FROM shlGoaliesFranchiseCareer
    INNER JOIN (SELECT Name as playerName, FHMIDS FROM playerMaster)
    ON FHMID = FHMIDS
    INNER JOIN (SELECT Name as teamName, fhmFranchiseId  FROM teams WHERE League = 'SHL')
    ON FranchiseID = fhmFranchiseId
    WHERE isPlayoffs = 1)
    "
  )

shl_goalie_career_rs_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, GamesPlayed, Wins, Losses, OvertimeLosses, Minutes, Shutouts, GoalsAgainst, ShotsAgainst, EmptyGoalAgainst FROM
    (SELECT * FROM shlGoaliesCareer
    INNER JOIN (SELECT Name as playerName, FHMIDS FROM playerMaster)
    ON FHMID = FHMIDS
    WHERE isPlayoffs = 0)
    "
  )

shl_goalie_career_ps_stats <-
  dbGetQuery(
    con,
    "
    SELECT playerName, GamesPlayed, Wins, Losses, OvertimeLosses, Minutes, Shutouts, GoalsAgainst, ShotsAgainst, EmptyGoalAgainst FROM
    (SELECT * FROM shlGoaliesCareer
    INNER JOIN (SELECT Name as playerName, FHMIDS FROM playerMaster)
    ON FHMID = FHMIDS
    WHERE isPlayoffs = 1)
    "
  )

return_skaters <- function(type, season) {
  stats_table <- switch(
    type,
    "Career" = ifelse(season == "Playoffs", return(shl_career_ps_stats), return(shl_career_rs_stats)),
    "Franchise" = ifelse(season == "Playoffs", return(shl_franchise_ps_stats), return(shl_franchise_rs_stats)),
    "Season" = ifelse(season == "Playoffs", return(shl_ps_stats), return(shl_rs_stats)),
  )
  print(stats_table)
  return(stats_table)
}

return_goalies <- function(type, season) {
  stats_table <- switch(
    type,
    "Career" = ifelse(season == "Playoffs", return(shl_goalie_career_ps_stats), return(shl_goalie_career_rs_stats)),
    "Franchise" = ifelse(season == "Playoffs", return(shl_goalie_franchise_ps_stats), return(shl_goalie_franchise_rs_stats)),
    "Season" = ifelse(season == "Playoffs", return(shl_goalie_ps_stats), return(shl_goalie_rs_stats)),
  )
  return(stats_table)
}
