#' Creates a collection.
#'
#' @param connectionInfo A DocumentDB connection info object generated with getDocumentDBConnectionInfo().
#' @param collectionId The ID of the collection to create.
#' @param throughput Optional. The user specified throughput for the collection expressed in units of 100 request units per second. This can be between 400 and 250,000 (or higher by requesting a limit increase). If the value is over 10,000, then the collection must include a partitionKey definition. If the value is equal to or under 10,000, then the collection must not include a partitionKey definition.
#' @param partitionKey Optional. This value is used to configure the partition key to be used for partitioning data into multiple partitions. If the throughput value is over 10,000, then the collection must include a partitionKey definition. If the throughput value is equal to or under 10,000, then the collection must not include a partitionKey definition.
#' @param indexingPolicy Optional. This value is used to configure indexing policy. By default, the indexing is automatic for all document paths within the collection. To specify a custom indexing policy pass a string with a JSON element, eg. "indexingPolicy": {...}. See \url{https://docs.microsoft.com/en-us/rest/api/documentdb/create-a-collection} for more details.
#' @param databaseId Optional. The ID of the database in which to create the collection. If no database ID is specified, the ID will be taken from the connection info.
#' @param consistencyLevel Optional. The consistency level override. The valid values are: Strong, Bounded, Session, or Eventual (in order of strongest to weakest). The override must be the same or weaker than the account's configured consistency level.
#' @param sessionToken Optional. A string token used with session level consistency. For more information, see \href{https://azure.microsoft.com/en-us/documentation/articles/documentdb-consistency-levels}{Using consistency levels in DocumentDB}.
#' @param userAgent Optional. A string that specifies the client user agent performing the request. The recommended format is {user agent name}/{version}. For example, the official DocumentDB .NET SDK sets the User-Agent string to Microsoft.Document.Client/1.0.0.0. A custom user-agent could be something like ContosoMarketingApp/1.0.0.
#'
#' @return Some information extracted from the REST API response such as request charge and session token.
#' @export
#'
#' @examples
#' # load the documentdbr package
#' library(documentdbr)
#' 
#' # setup connection infos
#' myDocDB <- getDocumentDBConnectionInfo(
#'   accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'   primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'   databaseId = "MyDatabase"
#' )
#'
#' # create the MyCollection collection with default throughput (within MyDatabase as specified in the connection info)
#' # note: alternatively, the database id can be specified as parameter
#' createCollection(myDocDB, "MyNewDatabase")
createCollection <-
  function(connectionInfo,
           collectionId,
           throughput = "",
           partitionKey = "",
           indexingPolicy = "",
           databaseId = "",
           consistencyLevel = "",
           sessionToken = "",
           userAgent = "") {

      # initialization
      if (length(databaseId) == 0 || databaseId == "") {
          databaseId <- connectionInfo$databaseId
      }

      resourceLink <- paste0("dbs/", databaseId)
      postUrl <- paste0(connectionInfo$accountUrl, "/", resourceLink, "/colls")
      requestCharge <- 0
      if ((length(indexingPolicy) != 0) && (indexingPolicy != "")) {
          indexingPolicyElement <- paste0(", ", indexingPolicy)
      } else {
          indexingPolicyElement <- ""
      }
      if (length(partitionKey) != 0 && partitionKey != "") {
          partitionKeyElement <- paste0(", \"partitionKey\": { \"paths\": [\"", partitionKey, "\"], \"kind\": \"Hash\" }")
      } else {
          partitionKeyElement <- ""
      }
      body <- paste0(
      "{\"id\": \"", collectionId, "\"",
      indexingPolicyElement,
      partitionKeyElement,
      "}")

      # prepare POST call
      currentHttpDate <- httr::http_date(Sys.time())
      authorizationToken <- generateAuthToken(
      verb = "POST",
      resourceType = "colls",
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
        "Accept" = "application/json",
        "authorization" = authorizationToken,
        "x-ms-date" = currentHttpDate,
        "x-ms-version" = "2016-07-11",
        "x-ms-offer-throughput" = format(throughput, scientific = FALSE),
        "x-ms-consistency-level" = consistencyLevel,
        "x-ms-session-token" = sessionToken,
        "User-Agent" = userAgent
      ),
      body = body
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