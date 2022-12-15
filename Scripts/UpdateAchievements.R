require(DBI)
require(dbplyr)
require(RSQLite)
require(stringr)

con <- dbConnect(SQLite(), "database/SHLHistory.db")
dbConnect(SQLite(), "database/SHLHistory")

achieve <- read.csv("achievement.csv")

dbWriteTable(con, "achievement", achieve, overwrite =F, append = T, row.names=FALSE)
