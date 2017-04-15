#' Deletes a single document from a collection.
#'
#' @param accountUrl The URI of the DocumentDB account.
#' @param primaryOrSecondaryKey The master key to authenticate.
#' @param databaseId The ID of the database to query.
#' @param collectionId The ID of the collection to query.
#' @param documentId The ID of the document to delete.
#' @param partitionKey Optional. The partition key value for the document to be deleted. Must be included if and only if the collection is created with a partitionKey definition.
#' @param consistencyLevel Optional. The consistency level override. The valid values are: Strong, Bounded, Session, or Eventual (in order of strongest to weakest). The override must be the same or weaker than the account's configured consistency level.
#' @param sessionToken Optional. A string token used with session level consistency. For more information, see \href{https://azure.microsoft.com/en-us/documentation/articles/documentdb-consistency-levels}{Using consistency levels in DocumentDB}.
#' @param userAgent Optional. A string that specifies the client user agent performing the request. The recommended format is {user agent name}/{version}. For example, the official DocumentDB .NET SDK sets the User-Agent string to Microsoft.Document.Client/1.0.0.0. A custom user-agent could be something like ContosoMarketingApp/1.0.0.
#'
#' @return Some information extracted from the REST API response such as request charge and session token.
#' @export
#'
#' @examples
#' # deletes the document with id fe7718ad-0000-4f42-cf5a-e2d79d2156df
#' library(documentdbr)
#' deleteResult <-
#'   deleteDocument(
#'     accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'     primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'     databaseId = "MyDatabaseId",
#'     collectionId = "MyCollectionId",
#'     documentId = "fe7718ad-0000-4f42-cf5a-e2d79d2156df"
#'   )
#' print(deleteResult$requestCharge)
deleteDocument <-
  function(accountUrl,
           primaryOrSecondaryKey,
           databaseId,
           collectionId,
           documentId,
           partitionKey = "",
           consistencyLevel = "",
           sessionToken = "",
           userAgent = "") {

      # initialization
      collectionResourceLink <- paste0("dbs/", databaseId, "/colls/", collectionId, "/docs", "/", documentId)
      deleteUrl <- paste0(accountUrl, "/", collectionResourceLink)
      requestCharge <- 0

      # prepare DELETE call
      currentHttpDate <- httr::http_date(Sys.time())
      authorizationToken <- generateAuthToken(
          verb = "DELETE",
          resourceType = "docs",
          resourceLink = collectionResourceLink,
          date = currentHttpDate,
          key = primaryOrSecondaryKey,
          keyType = "master",
          tokenVersion = "1.0"
      )
      if (partitionKey != "") {
          partitionKey = paste0("[\"", partitionKey, "\"]")
      }

      # do REST call
      response <- httr::DELETE(
          deleteUrl,
          httr::add_headers(
            "Accept" = "application/json",
            "authorization" = authorizationToken,
            "x-ms-date" = currentHttpDate,
            "x-ms-version" = "2016-07-11",
            "x-ms-documentdb-partitionkey" = partitionKey,
            "x-ms-consistency-level" = consistencyLevel,
            "x-ms-session-token" = sessionToken,
            "User-Agent" = userAgent
          )
          #, verbose()
      )

      # process response
      if (floor(response$status_code / 100) != 2) {
          # error
          # parse result
          # note: in case the delete was successful, content is empty - thus we need to do the extraction here
          completeResultFromJson <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
          # stop with an error message
          errorMessage <- paste0(
                "A ", completeResultFromJson$code, " error occured during DocumentDB querying."
                , " Error Message: ", completeResultFromJson$message
          )
          stop(errorMessage)
      } else {
          # no error
          # get request charge and session token
          requestCharge <- as.numeric(response$headers$`x-ms-request-charge`)
          sessionToken <- response$headers$`x-ms-session-token`
      }

      # return result
      list(requestCharge = requestCharge, sessionToken = sessionToken)
  }