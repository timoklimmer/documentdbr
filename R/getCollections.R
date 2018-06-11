#' Gets a list of collections in a Cosmos DB database
#'
#' @param connectionInfo A DocumentDB connection info object generated with getDocumentDBConnectionInfo().
#' @param userAgent Optional. A string that specifies the client user agent performing the request. The recommended format is {user agent name}/{version}. For example, the official DocumentDB .NET SDK sets the User-Agent string to Microsoft.Document.Client/1.0.0.0. A custom user-agent could be something like ContosoMarketingApp/1.0.0.
#'
#' @return The information extracted from the REST API response about the collections and some additional details such as request charge and session token.
#' @export
#'
#' @examples
#' # load the documentdbr package
#' library(documentdbr)
#'
#' # get a DocumentDBConnectionInfo object
#' myConnection <- getDocumentDBConnectionInfo(
#'   accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'   primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'   databaseId = "MyDatabaseId",
#'   collectionId = ""  # collectionId is ignored for this operation
#' )
#'
#' # get a list of collections and print some info about it
#' getResult <- getCollections(myConnection)
#' str(getResult$collections)
#' print(getResult$collections)
#'
#' # print request charge
#' print(getResult$requestCharge)
getCollections <-
  function(connectionInfo,
           userAgent = "") {

    # initialization
    resourceLink <- paste0("dbs/", connectionInfo$databaseId)
    getUrl <- paste0(connectionInfo$accountUrl, "/", resourceLink, "/colls")
    requestCharge <- 0

    # query result
    # prepare GET call
    currentHttpDate <- httr::http_date(Sys.time())
    authorizationToken <- generateAuthToken(
      verb = "GET",
      resourceType = "colls",
      resourceLink = resourceLink,
      date = currentHttpDate,
      key = connectionInfo$primaryOrSecondaryKey,
      keyType = "master",
      tokenVersion = "1.0"
    )
    # do REST call
    response <- httr::GET(
      getUrl,
      httr::add_headers(
        "Content-Type" = "application/query+json",
        "Accept" = "application/json",
        "authorization" = authorizationToken,
        "x-ms-date" = currentHttpDate,
        "x-ms-version" = "2016-07-11",
        "User-Agent" = userAgent
      )
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

      # get collections, request charge and session token
      collections <- data.frame(completeResultFromJson)
      requestCharge <- as.numeric(response$headers$`x-ms-request-charge`)
      sessionToken <- response$headers$`x-ms-session-token`
    }

    # return result
    list(
      collections = collections,
      requestCharge = requestCharge,
      sessionToken = sessionToken
    )
  }
