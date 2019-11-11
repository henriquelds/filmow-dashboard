library(RPostgreSQL)
library(DT)
library(plotly)
library(rjson)
library(pool)

pool <- dbPool(
  drv = dbDriver("PostgreSQL", max.con = 100),
  dbname = "filmow",
  host = "localhost",
  user = "filmow_dashboard",
  password = "filmow",
  idleTimeout = 3600000
)

data_maxmin <- c(dbGetQuery(pool,"SELECT MAX(year), MIN(year) from movies;"))
directors <- c(dbGetQuery(pool, "SELECT director as name, COUNT(*) as cont FROM public.movie_to_directors GROUP BY director ORDER BY cont DESC LIMIT 50;"))