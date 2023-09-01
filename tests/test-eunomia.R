library(testthat)
library(Eunomia)
connectionDetails <- Eunomia::getEunomiaConnectionDetails()
Eunomia::createCohorts(connectionDetails)
endDateFixSql <-"update main.cohort set cohort_end_date = dateadd(d,1, cohort_start_date) where cohort_end_date is null"
conn <- DatabaseConnector::connect(connectionDetails = connectionDetails)
DatabaseConnector::renderTranslateExecuteSql(connection = conn, sql = endDateFixSql)
DatabaseConnector::dbDisconnect(conn)


workFolder <- tempfile("work")
dir.create(workFolder)
resultsfolder <- tempfile("results")
dir.create(resultsfolder)
jobContext <- readRDS("tests/testJobContext.rds")
jobContext$moduleExecutionSettings$workSubFolder <- workFolder
jobContext$moduleExecutionSettings$resultsSubFolder  <- resultsfolder
jobContext$moduleExecutionSettings$connectionDetails <- connectionDetails
jobContext$moduleExecutionSettings$resultsConnectionDetails <- connectionDetails
jobContext$moduleExecutionSettings$resultsDatabaseSchema <- jobContext$moduleExecutionSettings$workDatabaseSchema

test_that("Test createDataModelSchema", {
  source("Main.R")
  createDataModelSchema(jobContext)
  
  # Verify that the table(s) are created
  connection <- DatabaseConnector::connect(
    connectionDetails = jobContext$moduleExecutionSettings$resultsConnectionDetails
  )
  on.exit(DatabaseConnector::disconnect(connection))
  tableList <- DatabaseConnector::getTableNames(
    connection = connection
  )
  resultsTablesCreated <- tableList[grep(getModuleInfo()$TablePrefix, tableList)]
  expect_true(length(resultsTablesCreated) > 0)
})

test_that("Run module", {
  source("Main.R")
  execute(jobContext)
  resultsFiles <- list.files(resultsfolder)
  expect_true("ci_incidence_summary.csv" %in% resultsFiles)
})

unlink(workFolder)
unlink(resultsfolder)
unlink(connectionDetails$server())
