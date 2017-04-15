#' Interprets the rows in a specified data.frame as documents and upserts those documents.
#'
#' @param accountUrl The URI of the DocumentDB account.
#' @param primaryOrSecondaryKey The master key to authenticate.
#' @param databaseId The ID of the database to modify.
#' @param collectionId The ID of the collection to modify.
#' @param documentsDataFrame A data.frame containing the documents. Ensure that the data.frame has an id column.
#' @param partitionKeyColumn Optional. Name of the column which contains the partition key. The partition key points to the partition where the documents are (to be) stored.
#' @param consistencyLevel Optional. The consistency level override. The valid values are: Strong, Bounded, Session, or Eventual (in order of strongest to weakest). The override must be the same or weaker than the account's configured consistency level.
#' @param sessionToken Optional. A string token used with session level consistency. For more information, see \href{https://azure.microsoft.com/en-us/documentation/articles/documentdb-consistency-levels}{Using consistency levels in DocumentDB}.
#' @param userAgent Optional. A string that specifies the client user agent performing the request. The recommended format is {user agent name}/{version}. For example, the official DocumentDB .NET SDK sets the User-Agent string to Microsoft.Document.Client/1.0.0.0. A custom user-agent could be something like ContosoMarketingApp/1.0.0.
#'
#' @return Some information extracted from the REST API response such as request charge and session token.
#' @export
#'
#' @examples
#' # create a data.frame with dummy data
#' someDataFrame <- data.frame(
#'   id = 1:10,
#'   machineId = paste("Machine", 1:10),
#'   value1 = rnorm(10, 0, 1),
#'   value2 = rnorm(10, 0, 1),
#'   value3 = rnorm(10, 0, 1)
#' )
#' 
#' # upsert the data.frame
#' library(documentdbr)
#' upsertResult <-
#'    upsertDocuments(
#'      accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'      primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'      databaseId = "MyDatabaseId",
#'      collectionId = "MyCollectionId",
#'      documentsDataFrame = someDataFrame
#'    )
upsertDocuments <-
  function(accountUrl,
           primaryOrSecondaryKey,
           databaseId,
           collectionId,
           documentsDataFrame,
           partitionKeyColumn = "",
           consistencyLevel = "",
           sessionToken = "",
           userAgent = "") {

      # ensure that id column is of type character (otherwise DocumentDB doesn't like it)
      documentsDataFrame[, "id"] <- sapply(documentsDataFrame[, "id"], format, scientific = FALSE)

      # convert all factor columns to character (otherwise, the below generated JSONs will only contain index numbers)
      isFactorColumn <- sapply(documentsDataFrame, is.factor)
      documentsDataFrame[, isFactorColumn] <- sapply(documentsDataFrame[, isFactorColumn], format, scientific = FALSE)

      # assemble JSONs for each row and do the upserts
      columnNames <- names(documentsDataFrame)
      requestCharge <- 0
      for (currentRowIndex in 1:nrow(documentsDataFrame)) {
          json <- as.character(jsonlite::toJSON(documentsDataFrame[currentRowIndex,]))
          json <- gsub("(^\\[)|(\\]$)", "", json)
          singleUpsertResult <- upsertDocument(
              accountUrl = accountUrl,
              primaryOrSecondaryKey = primaryOrSecondaryKey,
              databaseId = databaseId,
              collectionId = collectionId,
              document = json,
              partitionKey = documentsDataFrame[currentRowIndex, partitionKeyColumn],
              consistencyLevel = consistencyLevel,
              sessionToken = sessionToken,
              userAgent = userAgent
          )
          sessionToken <- singleUpsertResult$sessionToken
          requestCharge <- requestCharge + singleUpsertResult$requestCharge
      }

      # return result
      list(requestCharge = requestCharge, sessionToken = sessionToken)
  }