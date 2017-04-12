#' Inserts or updates the specified JSON document into a collection.
#'
#' @param accountUrl The URI of the DocumentDB account.
#' @param primaryOrSecondaryKey The master key to authenticate.
#' @param databaseId The ID of the database to query.
#' @param collectionId The ID of the collection to query.
#' @param document Either a JSON document (jsonlite) or JSON string denoting the document to add or update.
#' @param partitionKey Optional. The partition key value pointing to the partition where the document is (to be) stored.
#'
#' @return Some information extracted from the REST API response such as request charge and session token.
#' @export
#'
#' @examples
#' # inserts a document if not existing yet, otherwise updates it
#' library(documentdbr)
#' upsertResult <-
#'   upsertDocument(
#'     accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'     primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'     databaseId = "ToDoList",
#'     collectionId = "Items",
#'     document = toJSON("{\"id\": \"fe7718ad-0000-4f42-cf5a-e2d79d2156df\", \"Value\": \"126\"}")
#'   )
#' print(upsertResult$requestCharge)
upsertDocument <-
  function(accountUrl,
           primaryOrSecondaryKey,
           databaseId,
           collectionId,
           document,
           partitionKey = "") {

      # initialization
      collectionResourceLink <- paste0("dbs/", databaseId, "/colls/", collectionId)
      postUrl <- paste0(accountUrl, "/", collectionResourceLink, "/docs")
      requestCharge <- 0

      # prepare POST call
      currentHttpDate <- httr::http_date(Sys.time())
      authorizationToken <- generateAuthToken(
          verb = "POST",
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
      if (is(document, "json")) {
          document <- jsonlite::fromJSON(document)
      }
      response <- httr::POST(
          postUrl,
          httr::add_headers(
                "Accept" = "application/json",
                "authorization" = authorizationToken,
                "x-ms-date" = currentHttpDate,
                "x-ms-version" = "2016-07-11",
                "x-ms-documentdb-is-upsert" = "True",
                "x-ms-documentdb-partitionkey" = partitionKey
          ),
          body = document
          #, verbose()
      )

      # process response
      completeResultFromJson <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      if (floor(response$status_code / 100) != 2) {
          # error
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