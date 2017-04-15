#' Counts documents in a collection, either all or only those which satisfy a specified predicate.
#'
#' @param accountUrl The URI of the DocumentDB account.
#' @param primaryOrSecondaryKey The master key to authenticate.
#' @param databaseId The ID of the database to query.
#' @param collectionId The ID of the collection to query.
#' @param predicate Optional. The predicate which has to be satisfied. Example: "c.machineId IN ('Machine 1', 'Machine 2')" counts all documents of machine 1 and 2. Default is "", ie. all documents in the collection are counted.
#' @param partitionKey Optional. Can be used to limit the operation to a certain partition.
#' @param consistencyLevel Optional. The consistency level override. The valid values are: Strong, Bounded, Session, or Eventual (in order of strongest to weakest). The override must be the same or weaker than the account's configured consistency level.
#' @param sessionToken Optional. A string token used with session level consistency. For more information, see \href{https://azure.microsoft.com/en-us/documentation/articles/documentdb-consistency-levels}{Using consistency levels in DocumentDB}.
#' @param userAgent Optional. A string that specifies the client user agent performing the request. The recommended format is {user agent name}/{version}. For example, the official DocumentDB .NET SDK sets the User-Agent string to Microsoft.Document.Client/1.0.0.0. A custom user-agent could be something like ContosoMarketingApp/1.0.0.
#'
#' @return The number of documents counted and some information extracted from the REST API response such as request charge and session token.
#' @export
#'
#' @examples
#' # load the documentdbr package
#' library(documentdbr)
#' 
#' # count all documents in the collection
#' queryResult <- countDocuments(
#'      accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'      primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'      databaseId = "MyDatabaseId",
#'      collectionId = "MyCollectionId"
#' )
#' cat(paste("The collection has", as.numeric(queryResult$count), "documents.\n"))
#'
#' 
#' # count all documents where value1 < 0.5
#' queryResult <- countDocuments(
#'      accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'      primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'      databaseId = "MyDatabaseId",
#'      collectionId = "MyCollectionId",
#'      predicate = "c.value1 < 0.5",
#' )
#' cat(paste("In", as.numeric(queryResult$count), "documents value1 is < 0.5."))
countDocuments <-
  function(accountUrl,
           primaryOrSecondaryKey,
           databaseId,
           collectionId,
           predicate = "",
           partitionKey = "",
           consistencyLevel = "",
           sessionToken = "",
           userAgent = "") {

      # prepare query
      # queryText
      queryText <- "SELECT count(c.id) FROM c"
      if (predicate != "") {
          queryText <- paste(queryText, "WHERE", predicate)
      }
      # enableCrossPartitionQuery
      if (partitionKey != "") {
          enableCrossPartitionQuery <- FALSE;
      } else {
          enableCrossPartitionQuery <- TRUE;
      }

      # run query
      queryResult <- selectDocuments(
              accountUrl = accountUrl,
              primaryOrSecondaryKey = primaryOrSecondaryKey,
              databaseId = databaseId,
              collectionId = collectionId,
              queryText = queryText,
              enableCrossPartitionQuery = enableCrossPartitionQuery,
              partitionKey = partitionKey,
              consistencyLevel = consistencyLevel,
              sessionToken = sessionToken,
              userAgent = userAgent
          )

      # return result
      list(
        count = as.numeric(queryResult$documents),
        requestCharge = queryResult$requestCharge,
        sessionToken = queryResult$sessionToken
    )
  }