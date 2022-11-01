# ---- connection.R
library(RPostgres)
library(DBI)

# Get creds
source(".secrets/creds.R")

# DB Connection
con <- dbConnect(
  RPostgres::Postgres(), 
  dbname = def_db,
  host = host_db,
  port = 26257,
  user = usr, 
  password = pwd, 
  options = db_opts
)
