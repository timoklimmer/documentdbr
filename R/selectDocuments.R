#' Runs a SELECT query on a collection and returns its result as data.frame.
#'
#' @param connectionInfo A DocumentDB connection info object generated with getDocumentDBConnectionInfo().
#' @param queryText The SQL query to execute.
#' @param enableCrossPartitionQuery Optional. If the collection is partitioned, this must be set to TRUE to allow execution across multiple partitions. Queries that filter against a single partition key, or against single-partitioned collections do not need to set this parameter. Default value is FALSE.
#' @param partitionKey Optional. Needs to be set if the collection is partitioned and the cross partition query option is disabled.
#' @param maxItemsPerChunk Optional. Use it for performance and cost tuning.
#' @param consistencyLevel Optional. The consistency level override. The valid values are: Strong, Bounded, Session, or Eventual (in order of strongest to weakest). The override must be the same or weaker than the account's configured consistency level.
#' @param sessionToken Optional. A string token used with session level consistency. For more information, see \href{https://azure.microsoft.com/en-us/documentation/articles/documentdb-consistency-levels}{Using consistency levels in DocumentDB}.
#' @param userAgent Optional. A string that specifies the client user agent performing the request. The recommended format is {user agent name}/{version}. For example, the official DocumentDB .NET SDK sets the User-Agent string to Microsoft.Document.Client/1.0.0.0. A custom user-agent could be something like ContosoMarketingApp/1.0.0.
#'
#' @return The result of the query as data.frame object and some information extracted from the REST API response such as request charge and session token.
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
#' # run a SQL query, get its result as as data.frame and print some infos
#' queryResult <- selectDocuments(myCollection, "SELECT Items.name, Items.description, Items.isComplete FROM Items WHERE Items.isComplete = true")
#' str(queryResult$documents)
#' print(queryResult$documents)
#' print(queryResult$requestCharge)
#' 
#' # run another SQL query, this time getting the sum of all value1 fields
#' anotherQueryResult <- selectDocuments(myCollection, "SELECT sum(c.value1) AS TotalSumValue1 FROM c")
#' print(paste("The total sum over all value1 fields is:", as.numeric(anotherQueryResult$documents)))
selectDocuments <-
  function(connectionInfo,
           queryText,
           enableCrossPartitionQuery = FALSE,
           partitionKey = "",
           maxItemsPerChunk = 100,
           consistencyLevel = "",
           sessionToken = "",
           userAgent = "") {

    # initialization
    resourceLink <- paste0("dbs/", connectionInfo$databaseId, "/colls/", connectionInfo$collectionId)
    postUrl <- paste0(connectionInfo$accountUrl, "/", resourceLink, "/docs")
    content <- paste0("{\"query\":\"", escapeTextForJson(queryText), "\"}")
    requestCharge <- 0
    enableCrossPartitionQuery <- tolower(as.character(enableCrossPartitionQuery))
    if (length(partitionKey) != 0 && partitionKey != "") {
        partitionKey = paste0("[\"", partitionKey, "\"]")
    }

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
            resourceLink = resourceLink,
            date = currentHttpDate,
            key = connectionInfo$primaryOrSecondaryKey,
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
              "x-ms-documentdb-query-enablecrosspartition" = enableCrossPartitionQuery,
              "x-ms-max-item-count" = maxItemsPerChunk,
              "x-ms-continuation" = msContinuation,
              "x-ms-documentdb-partitionkey" = partitionKey,
              "x-ms-consistency-level" = consistencyLevel,
              "x-ms-session-token" = sessionToken,
              "User-Agent" = userAgent
            ),
            body = content
            #, verbose()
          )
        # log status code
        print(paste0("Request status code: ", response$status_code))
        # process response
        completeResultFromJson <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
        # handle 429 responses by retrieving wait time from response header and pausing
        if (response$status_code == 429) {
          # get retry wait time and pause
          waitTime <- response$headers$`x-ms-retry-after-ms`
          print(paste0("Waiting for ", waitTime, "ms to retry request"))
          if (!is.null(waitTime)) {
            Sys.sleep(as.numeric(waitTime) / 1000)
          }
        } else if (floor(response$status_code / 100) != 2 && response$status_code != 429) {
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
          if (is.data.frame(intermediateResult)) { # check that the chunk contains data
            if (is.null(totalDocuments)) {
              totalDocuments <- intermediateResult
            } else {
              totalDocuments <- plyr::rbind.fill(totalDocuments, intermediateResult)
            }
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