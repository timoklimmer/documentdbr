#' Queries a single document from a collection.
#'
#' @param accountUrl The URI of the DocumentDB account.
#' @param primaryOrSecondaryKey The master key to authenticate.
#' @param databaseId The ID of the database to query.
#' @param collectionId The ID of the collection to query.
#' @param documentId The ID of the document to be queried.
#' @param partitionKey Optional. The partition key value for the document to be read. Must be included if and only if the collection is created with a partitionKey definition.
#'
#' @return The result of the query as data.frame object and some information extracted from the REST API response such as request charge and session token.
#' @export
#'
#' @examples
#' # queries a single document
#' queryResult <-
#'   querySingleDocument(
#'     accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'     primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'     databaseId = "ToDoList",
#'     collectionId = "Items",
#'     documentId = "3c7718ab-858b-4f42-bf5a-e2d79d2156de"
#'   )
#' str(queryResult$document)
#' print(queryResult$document)
#' print(queryResult$requestCharge)
querySingleDocument <-
  function(accountUrl,
           primaryOrSecondaryKey,
           databaseId,
           collectionId,
           documentId,
           partitionKey = "") {

    # initialization
    collectionResourceLink <- paste0("dbs/", databaseId, "/colls/", collectionId, "/docs/", documentId)
    getUrl <- paste0(accountUrl, "/", collectionResourceLink)
    requestCharge <- 0

    # query result
    # prepare POST call
    currentHttpDate <- httr::http_date(Sys.time())
    authorizationToken <- generateAuthToken(
        verb = "GET",
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
    response <- httr::GET(
            getUrl,
            httr::add_headers(
              "Content-Type" = "application/query+json",
              "Accept" = "application/json",
              "authorization" = authorizationToken,
              "x-ms-date" = currentHttpDate,
              "x-ms-version" = "2016-07-11",
              "x-ms-documentdb-partitionkey" = partitionKey
            ),
            body = content
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

        # get document, request charge and session token
        document <- data.frame(completeResultFromJson)
        requestCharge <- as.numeric(response$headers$`x-ms-request-charge`)
        sessionToken <- response$headers$`x-ms-session-token`
    }

    # return result
    list(
        document = document,
        requestCharge = requestCharge,
        sessionToken = sessionToken
    )
  }