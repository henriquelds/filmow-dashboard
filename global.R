library(RPostgreSQL)
library(DT)
library(plotly)
library(rjson)
library(pool)

Merge.factors <- function(x, p) { 
  #Combines factor levels in x that are less than a specified proportion, p.
  t <- table(x)
  y <- subset(t, prop.table(t) < p)
  tit <- paste("Outros (",length(y),")",sep = "")
  z <- subset(t, prop.table(t) >= p)
  other <- rep(tit, sum(y))
  new.table <- c(z, table(other))
  new.x <- as.factor(rep(names(new.table), new.table))
  return(new.x)
}


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