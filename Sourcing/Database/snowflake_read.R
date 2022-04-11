# R Package for read access to Snowflake Datalake
library(tidyverse)
library(DBI)
library(keyring)

# Connecting to Snowflake Database
con <- DBI::dbConnect(odbc::odbc(),
                      driver = "SnowflakeDSIIDriver",
                      server = "elkay.east-us-2.azure.snowflakecomputing.com",
                      database = "DATALAKE",
                      Warehouse = "DATASCIENCE_WH",
                      UID    = "DATASCIENCE_READONLY", #rstudioapi::askForPassword("Datalake_readonly"),
                      PWD    = key_get("Snowflake","DATASCIENCE_READONLY", "Elkay"))#rstudioapi::askForPassword("Database password"))
#trusted_connection = "yes",
#host = "localhost",
#port = 1433)
#DBI::dbSendQuery(con, "USE WAREHOUSE DATALAKE_WH;")

print("Successfully compiled. Connections: con ...")
print("Functions: use DBI::dbGetQuery")
print("Remember to close connection")

# CLOSE CONNECTION
#dbDisconnect(con)
