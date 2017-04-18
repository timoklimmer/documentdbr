#' Creates a database with the specified database ID.
#'
#' @param connectionInfo A DocumentDB connection info object generated with getDocumentDBConnectionInfo().
#' @param databaseId The ID of the database to create.
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
#'   primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg=="
#' )
#'
#' # create the MyNewDatabase database
#' createDatabase(myDocDB, "MyNewDatabase")
createDatabase <-
  function(connectionInfo,
           newDatabaseId,
           consistencyLevel = "",
           sessionToken = "",
           userAgent = "") {

      # initialization
      resourceLink <- ""
      postUrl <- paste0(connectionInfo$accountUrl, "/dbs", resourceLink)
      requestCharge <- 0

      # prepare POST call
      currentHttpDate <- httr::http_date(Sys.time())
      authorizationToken <- generateAuthToken(
      verb = "POST",
      resourceType = "dbs",
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
        "x-ms-consistency-level" = consistencyLevel,
        "x-ms-session-token" = sessionToken,
        "User-Agent" = userAgent
      ),
      body = paste0("{\"id\": \"", newDatabaseId, "\" }")
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