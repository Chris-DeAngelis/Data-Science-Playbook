# R Package for read/write access to Snowflake Datalake
library(DBI) # Working with databases
library(dbplyr) # Working with databases
library(keyring) # Storing Credentials

######################## Read/Write ########################
write_connection <- DBI::dbConnect(odbc::odbc(),
                                   driver = "SnowflakeDSIIDriver",
                                   server = "elkay.east-us-2.azure.snowflakecomputing.com",
                                   database = "DATALAKE",
                                   Warehouse = "DATASCIENCE_WH",
                                   UID    = "DATALAKE_READ_WRITE_CREATE", #rstudioapi::askForPassword("Datalake_readonly"),
                                   PWD    = key_get("Snowflake_Write","DATALAKE_READ_WRITE_CREATE", "Elkay"))#rstudioapi::askForPassword("Database password"))

write_table <- function(table="MARKETING_SUMMARY",data=combined.allmetrics,overwrite=FALSE,append=FALSE){
  table_id <- Id(database = "DATALAKE", schema = "SERVING", table = table)
  dbWriteTable(write_connection, table_id, data, overwrite = overwrite, append = append, row.names = FALSE)
}

print("Successfully compiled. Connections: write_connection ...")
print("Functions: write_table ...")
print("Remember to close connection")

# CLOSE CONNECTION
#dbDisconnect(write_connection)