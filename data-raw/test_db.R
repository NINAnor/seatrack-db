## code to prepare `test_db` dataset goes here

# Make sure inst/extdata exists and create it if not
if (!dir.exists("inst/extdata")) {
  dir.create("inst/extdata")
}

con <- DBI::dbConnect(RSQLite::SQLite(), dbname = "inst/extdata/seatrack_test.db")

# Remove all existing tables
tables <- DBI::dbListTables(con)
for (table in tables) {
  DBI::dbRemoveTable(the$con, table)
}

# Load the SQL file and execute its contents


DBI::dbDisconnect(con)
