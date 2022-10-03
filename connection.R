# ---- connection.R
library(RPostgres)
library(DBI)

# DB Connection
host_db <- ""
def_db <- ""
usr <- ""
pwd <- ""
db_opts <- ""

con <- dbConnect(
  RPostgres::Postgres(), 
  dbname = def_db,
  host = host_db,
  port = 26257,
  user = usr, 
  password = pwd, 
  options = db_opts
)
