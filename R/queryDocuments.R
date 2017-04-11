#' Queries a collection for documents.
#'
#' @param accountUrl The URI of the DocumentDB account.
#' @param primaryOrSecondaryKey The master key to authenticate.
#' @param databaseId The ID of the database to query.
#' @param collectionId The ID of the collection to query.
#' @param queryText The SQL query to execute.
#' @param maxItemsPerChunk Optional. Use it for performance and cost tuning.
#'
#' @return The result of the query as data.frame object.
#' @export
#'
#' @examples
#' # returns the result of a SQL query as data.frame
#' library(documentdbr)
#' queryResult <-
#'   queryDocuments(
#'     accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'     primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'     databaseId = "ToDoList",
#'     collectionId = "Items",
#'     queryText = "SELECT Items.name, Items.description, Items.isComplete FROM Items WHERE Items.isComplete = true",
#'     maxItemsPerChunk = 100
#'   )
#' str(queryResult$documents)
#' print(queryResult$documents)
#' print(queryResult$requestCharge)
queryDocuments <-
  function(accountUrl,
           primaryOrSecondaryKey,
           databaseId,
           collectionId,
           queryText,
           maxItemsPerChunk = 100) {

    # initialization
    collectionResourceLink <- paste0("dbs/", databaseId, "/colls/", collectionId)
    postUrl <- paste0(accountUrl, "/", collectionResourceLink, "/docs")
    content <- paste0("{\"query\":\"", queryText, "\"}")
    requestCharge <- 0

    # query all pages and merge individual results
    isFirstLoop <- TRUE
    msContinuation <- ""
    totalDocuments <- data.frame()
    while (isFirstLoop || !is.null(msContinuation)) {
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

        # do REST call
        response <- httr::POST(
            postUrl,
            httr::add_headers(
              "Content-Type" = "application/query+json",
              "Accept" = "application/json",
              "authorization" = authorizationToken,
              "x-ms-date" = currentHttpDate,
              "x-ms-version" = "2016-07-11",
              "x-ms-documentdb-isquery" = "True",
              "x-ms-query-enable-crosspartition" = "true",
              "x-ms-max-item-count" = maxItemsPerChunk,
              "x-ms-continuation" = msContinuation
            ),
            body = content
            #, verbose()
          )

        # process response
        completeResultFromJson <- jsonlite::fromJSON(content(response, "text", encoding = "UTF-8"))
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

            # get intermediate result, request charge and session token and append it to total result
            intermediateResult <- completeResultFromJson$Documents
            intermediateResourceUsage <-
              if (is.null(totalDocuments)) {
                totalDocuments <- intermediateResult
              } else {
                totalDocuments <- rbind(totalDocuments, intermediateResult)
            }
            requestCharge <- requestCharge + as.numeric(response$headers$`x-ms-request-charge`)
            sessionToken <- response$headers$`x-ms-session-token`

            # get continuation token for the while loop to check if there is other page(s) we have to process
            msContinuation <- response$headers$`x-ms-continuation`
        }

        # unflag isFirstLoop
        isFirstLoop <- FALSE
    }

    # return result
    list(
        documents = totalDocuments,
        requestCharge = requestCharge,
        sessionToken = sessionToken
    )
  }