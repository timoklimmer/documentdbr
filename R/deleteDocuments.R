#' Deletes documents in a collection, either all or only those which satisfy a specified predicate.
#'
#' Note: As this function deletes each document one by one, it can take very long to do the deletions. In some cases, it is a better approach to delete and recreate the collection.
#'
#' @param connectionInfo A DocumentDB connection info object generated with getDocumentDBConnectionInfo().
#' @param predicate Optional. The predicate which has to be satisfied. Example: "c.machineId IN ('Machine 1', 'Machine 2')" deletes all documents of machine 1 and 2. Default is "", ie. all documents in the collection are deleted.
#' @param partitionKey Optional. Can be used to limit the operation to a certain partition.
#' @param consistencyLevel Optional. The consistency level override. The valid values are: Strong, Bounded, Session, or Eventual (in order of strongest to weakest). The override must be the same or weaker than the account's configured consistency level.
#' @param sessionToken Optional. A string token used with session level consistency. For more information, see \href{https://azure.microsoft.com/en-us/documentation/articles/documentdb-consistency-levels}{Using consistency levels in DocumentDB}.
#' @param userAgent Optional. A string that specifies the client user agent performing the request. The recommended format is {user agent name}/{version}. For example, the official DocumentDB .NET SDK sets the User-Agent string to Microsoft.Document.Client/1.0.0.0. A custom user-agent could be something like ContosoMarketingApp/1.0.0.
#'
#' @return Some information extracted from the REST API responses such as (aggregated) request charges and latest session token.
#' @export
#'
#' @examples
#' # load the documentdbr package
#' library(documentdbr)
#' 
#' # get a DocumentDBConnectionInfo object
#' myCollection <- getDocumentDBConnectionInfo(
#'   accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'   primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'   databaseId = "MyDatabaseId",
#'   collectionId = "MyCollectionId"
#' )
#' 
#' # delete all documents from the collection and print the request charge
#' deleteResult <- deleteDocuments(myCollection)
#' print(deleteResult$requestCharge)
#' 
#' # delete all documents where value1 is < 0.5 and print the request charge
#' deleteResult <- deleteDocuments(myCollection, predicate = "c.value1 < 0.5")
#' print(deleteResult$requestCharge)
deleteDocuments <- function(
  connectionInfo,
  predicate = "",
  partitionKey = "",
  consistencyLevel = "",
  sessionToken = "",
  userAgent = "") {

    # initialization
    requestCharge <- 0

    # do the delete in multiple batches (to ensure we are not running out of memory)
    needsAnotherLoop <- TRUE
    while (needsAnotherLoop) {
        # get the ids of the next 1000 documents to delete
        idQueryText <- "SELECT TOP 1000 c.id FROM c"
        if (length(predicate) != 0 && predicate != "") {
            idQueryText <- paste(idQueryText, "WHERE", predicate)
        }
        if (length(partitionKey) != 0 && partitionKey != "") {
            enableCrossPartitionQuery <- FALSE;
        } else {
            enableCrossPartitionQuery <- TRUE;
        }
        idQueryResult <- selectDocuments(
            connectionInfo = connectionInfo,
            queryText = idQueryText,
            enableCrossPartitionQuery = enableCrossPartitionQuery,
            partitionKey = partitionKey,
            maxItemsPerChunk = 1000,
            consistencyLevel = consistencyLevel,
            sessionToken = sessionToken,
            userAgent = userAgent
        )
        idsToDelete <- idQueryResult$documents$id
        requestCharge <- requestCharge + idQueryResult$requestCharge
        sessionToken <- idQueryResult$sessionToken
        if (!length(idsToDelete) > 0) {
            needsAnotherLoop <- FALSE
        }

        # delete the documents (one after another)
        # note: this may performance-wise be improved in future once options are available
        for (id in sapply(idQueryResult$documents[, "id"], format, scientific = FALSE)) {
            # do the delete
            deletionResult <- deleteDocument(
                connectionInfo = connectionInfo,
                documentId = id,
                partitionKey = partitionKey,
                consistencyLevel = consistencyLevel,
                sessionToken = sessionToken,
                userAgent = userAgent
              )
            # update requestCharge and sessionToken
            requestCharge <- requestCharge + deletionResult$requestCharge
            sessionToken <- deletionResult$sessionToken
        }
    }

    # return result
    list(
        requestCharge = requestCharge,
        sessionToken = sessionToken
    )
}