# Create a job context for testing purposes

library(Strategus)
library(dplyr)
library(readr)
source("SettingsFunctions.R")

# Create CohortIncidenceModule settings ---------------------------------------

#TODO: read JSON from file and load an IR design.
designJSON <- readr::read_file("tests/testDesign.json")
irDesign <- CohortIncidence::IncidenceDesign$new(designJSON)$toList()

cohortIncidenceModuleSpecifications <- createCohortIncidenceModuleSpecifications(irDesign = irDesign)

# Module Settings Spec ----------------------------
analysisSpecifications <- createEmptyAnalysisSpecificiations() %>%
  addModuleSpecifications(cohortIncidenceModuleSpecifications)

executionSettings <- Strategus::createCdmExecutionSettings(connectionDetailsReference = "dummy",
                                                           workDatabaseSchema = "main",
                                                           cdmDatabaseSchema = "main",
                                                           cohortTableNames = CohortGenerator::getCohortTableNames(cohortTable = "cohort"),
                                                           workFolder = "dummy",
                                                           resultsFolder = "dummy",
                                                           minCellCount = 5)

# Job Context ----------------------------
module <- "CohortIncidenceModule"
moduleIndex <- 1
moduleExecutionSettings <- executionSettings
moduleExecutionSettings$workSubFolder <- "dummy"
moduleExecutionSettings$resultsSubFolder <- "dummy"
moduleExecutionSettings$databaseId <- 123
jobContext <- list(sharedResources = list(),
                   settings = analysisSpecifications$moduleSpecifications[[moduleIndex]]$settings,
                   moduleExecutionSettings = moduleExecutionSettings)
saveRDS(jobContext, "tests/testJobContext.rds")

