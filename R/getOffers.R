#' Gets a list of available "offers", which are the performance levels you can
#' provision for a Cosmos DB collection.
#'
#' @param connectionInfo A DocumentDB connection info object generated with getDocumentDBConnectionInfo().
#' @param userAgent Optional. A string that specifies the client user agent performing the request. The recommended format is {user agent name}/{version}. For example, the official DocumentDB .NET SDK sets the User-Agent string to Microsoft.Document.Client/1.0.0.0. A custom user-agent could be something like ContosoMarketingApp/1.0.0.
#'
#' @return The available offers as a data.frame object and some information extracted from the REST API response such as request charge and session token.
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
#' # get a list of offers and print some info about it
#' getResult <- getOffers(myCollection)
#' str(getResult$offers)
#' print(getResult$offers)
#'
#' # print request charge
#' print(getResult$requestCharge)
getOffers <-
  function(connectionInfo,
           userAgent = "") {

    # initialization
    resourceLink <- paste0("/offers")
    getUrl <- paste0(connectionInfo$accountUrl, "/", resourceLink)
    requestCharge <- 0

    # query result
    # prepare GET call
    currentHttpDate <- httr::http_date(Sys.time())
    authorizationToken <- generateAuthToken(
      verb = "GET",
      resourceType = "offers",
      resourceLink = "",
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

      # get offers, request charge and session token
      offers <- data.frame(completeResultFromJson)
      requestCharge <- as.numeric(response$headers$`x-ms-request-charge`)
      sessionToken <- response$headers$`x-ms-session-token`
    }

    # return result
    list(
      offers = offers,
      requestCharge = requestCharge,
      sessionToken = sessionToken
    )
  }
