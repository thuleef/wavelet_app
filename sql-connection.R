library(RMySQL)
con <-
  dbConnect(
    MySQL(),
    user = 'thepufferfish',
    password = 'usingthis0nce',
    host = 'caswp-db.cyxyjqnr1bi5.us-west-1.rds.amazonaws.com',
    port = 3306,
    dbname = 'RTDF'
  )