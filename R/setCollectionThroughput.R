#' Change the throughput for a Cosmos DB collection
#'
#' @description
#' This function allows you to change the throughput (RU's) for a Cosmos DB collection from within your R code.
#'
#' Note that the change will take effect immediately, but it the Azure portal may take a couple of minutes to show
#' the new scale setting.
#'
#' @param connectionInfo A DocumentDB connection info object generated with getDocumentDBConnectionInfo().
#' @param throughput The user specified throughput for the collection expressed in units of 100 request units per second. This can be between 400 and 250,000 (or higher by requesting a limit increase). If the value is over 10,000, then the collection must include a partitionKey definition. If the value is equal to or under 10,000, then the collection must not include a partitionKey definition.
#' @param userAgent Optional. A string that specifies the client user agent performing the request. The recommended format is {user agent name}/{version}. For example, the official DocumentDB .NET SDK sets the User-Agent string to Microsoft.Document.Client/1.0.0.0. A custom user-agent could be something like ContosoMarketingApp/1.0.0.
#'
#' @return Some information extracted from the REST API response such as request charge and session token.
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
#' # scale up Cosmos DB performance before running a big query
#' scaleResult <- setCollectionThroughput(myCollection, 9500)
#'
#' # run a big query
#' queryResult <- selectDocuments(myCollection, "SELECT Items.name, Items.description, Items.isComplete FROM Items")
#'
#' # scale Cosmos DB performance back down to save money
#' scaleResult <- setCollectionThroughput(myCollection, 500)
setCollectionThroughput <-
  function(connectionInfo,
           throughput,
           userAgent = "") {

    # Get information about the collection and its current offer
    collectionsResult <- documentdbr::getCollections(connectionInfo, userAgent)
    offersResult <- documentdbr::getOffers(connectionInfo, userAgent)

    collection = collectionsResult$collections[collectionsResult$collections$DocumentCollections.id == connectionInfo$collectionId, ]
    if (nrow(collection) != 1) {
      stop(paste0("API did not return a collection with ID \"", connectionInfo$collectionId, "\""))
    }

    offer = offersResult$offers[offersResult$offers$Offers.offerResourceId == collection$DocumentCollections._rid, ]
    if (nrow(offer) != 1) {
      stop(paste0("API did not return an offer for collection with RID \"", collection$DocumentCollections._rid, "\""))
    }

    currentVersion = offer$Offers.offerVersion

    # validate the input provided by the caller
    throughput = suppressWarnings(as.integer(throughput))
    if (is.na(throughput) == TRUE) {
      stop("\"throughput\" must be an integer value.")
    }
    if (throughput %% 100 != 0) {
      stop("\"throughput\" must be a multiple of 100.")
    }
    if (throughput < 400) {
      stop("The minimum throughput supported by DocumentDB is 400 RU's")
    }

    if (currentVersion != "V2") {
      if (throughput > 10000) {
        stop(paste0("Collection \"", collection$DocumentCollections.id, "\" current has an offer version of ", currentVersion, ".  When switching to a user-defined throughput, the maximum throughput is 10,000."))
      }
    } else {
      currentThroughput = offer$Offers.content$offerThroughput
      if (currentThroughput <= 10000) {
        if (throughput > 10000) {
          stop(paste0("The maximum throughput for collection \"", collection$DocumentCollections.id, "\" is 10,000 because it has only one partition."))
        }
      } else {
        if (throughput <= 10000) {
          stop(paste0("The minimum throughput for collection \"", collection$DocumentCollections.id, "\" is 10,100 because it has multiple partitions."))
        }
      }
    }


    # initialization
    resourceId <- offer$Offers._rid
    resourceLink <- paste0("offers/", resourceId)
    putUrl <- paste0(connectionInfo$accountUrl, "/", resourceLink)
    requestCharge <- 0

    body = list(
      offerVersion = "V2",
      offerType = "Invalid",
      content = list(
        offerThroughput = throughput,
        userSpecifiedThroughput = throughput
      ),
      resource = collection$DocumentCollections._self,
      offerResourceId = collection$DocumentCollections._rid,
      id = resourceId,
      `_rid` = resourceId
    )
    body <- jsonlite::toJSON(body, auto_unbox = TRUE)

    # prepare PUT call
    currentHttpDate <- httr::http_date(Sys.time())
    authorizationToken <- generateAuthToken(
      verb = "PUT",
      resourceType = "offers",
      resourceLink = tolower(resourceId),
      date = currentHttpDate,
      key = connectionInfo$primaryOrSecondaryKey,
      keyType = "master",
      tokenVersion = "1.0"
    )

    # do REST call
    response <- httr::PUT(
      putUrl,
      httr::add_headers(
        "Accept" = "application/json",
        "authorization" = authorizationToken,
        "x-ms-date" = currentHttpDate,
        "x-ms-version" = "2016-07-11",
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
