# Copyright 2022 Observational Health Data Sciences and Informatics
#
# This file is part of CohortIncidenceModule
# 
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
# 
#     http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

# Module methods -------------------------
validate <- function(jobContext) {
  # Verify the job context details - this feels like a task to centralize for
  # all modules
  checkmate::assert_list(x = jobContext)
  if (is.null(jobContext$settings)) {
    stop("Analysis settings not found in job context")
  }
  if (is.null(jobContext$sharedResources)) {
    stop("Shared resources not found in job context")
  }
  if (is.null(jobContext$moduleExecutionSettings)) {
    stop("Execution settings not found in job context")
  }
  
  # Validate that the analysis specification will work when we 
  # enter the execute statement. This is done by deserializing the design.
  irDesign <- CohortIncidence::IncidenceDesign$new(jobContext$settings$irDesign);
  designJson <- rJava::J("org.ohdsi.analysis.cohortincidence.design.CohortIncidence")$fromJson(as.character(irDesign$asJSON()))
  
  invisible(designJson)
}

execute <- function(jobContext) {
  rlang::inform("Validating inputs")
  validate(jobContext)
  
  # Establish the connection and ensure the cleanup is performed
  connection <- DatabaseConnector::connect(jobContext$moduleExecutionSettings$connectionDetails)
  on.exit(DatabaseConnector::disconnect(connection))
  
  # extract CohortIncidence design from jobContext
  irDesign <- as.character(CohortIncidence::IncidenceDesign$new(jobContext$settings$irDesign)$asJSON());
  
  # construct buildOptions from executionSettings
  # Questions:
  # Will there be a subgroup cohort table?
  # Are we pulling the source name from the right place?
  
  buildOptions <- CohortIncidence::buildOptions(cohortTable = paste0(jobContext$moduleExecutionSettings$workDatabaseSchema, '.', jobContext$moduleExecutionSettings$cohortTableNames$cohortTable),
                                                cdmDatabaseSchema = jobContext$moduleExecutionSettings$cdmDatabaseSchema,
                                                sourceName = as.character(jobContext$moduleExecutionSettings$databaseId),
                                                refId = 1)
  
  executeResults <- CohortIncidence::executeAnalysis(connection = connection,
                                                     incidenceDesign = irDesign,
                                                     buildOptions = buildOptions)
  #Export the results
  exportFolder <- jobContext$moduleExecutionSettings$resultsSubFolder
  if (!dir.exists(exportFolder)) {
    dir.create(exportFolder, recursive = TRUE)
  }
  
  rlang::inform("Export data")
  
  
  if (nrow(executeResults) > 0) {
    executeResults$database_id <- jobContext$moduleExecutionSettings$databaseId
  } else {
    executeResults$database_id <- character(0)
  }

  #TODO: restrict executeREsults to minCellCount
  
  readr::write_csv(executeResults, file.path(exportFolder,"incidence_summary.csv")) # this will be renamed later

  moduleInfo <- ParallelLogger::loadSettingsFromJson("MetaData.json")
  resultsDataModel <- readr::read_csv(file="resultsDataModelSpecification.csv", show_col_types = FALSE)  
  newTableNames <- paste0(moduleInfo$TablePrefix, resultsDataModel$"table_name")
  # Rename export files based on table prefix
  file.rename(file.path(exportFolder, paste0(unique(resultsDataModel$"table_name"), ".csv")),
              file.path(exportFolder, paste0(unique(newTableNames), ".csv")))
  resultsDataModel$table_name <- newTableNames
  readr::write_csv(resultsDataModel, file.path(exportFolder, "resultsDataModelSpecification.csv"))
  
}

