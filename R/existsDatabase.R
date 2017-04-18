#' Checks whether a database exists.
#'
#' @param connectionInfo A DocumentDB connection info object generated with getDocumentDBConnectionInfo().
#' @param databaseId The ID of the database to check.
#' @param consistencyLevel Optional. The consistency level override. The valid values are: Strong, Bounded, Session, or Eventual (in order of strongest to weakest). The override must be the same or weaker than the account's configured consistency level.
#' @param sessionToken Optional. A string token used with session level consistency. For more information, see \href{https://azure.microsoft.com/en-us/documentation/articles/documentdb-consistency-levels}{Using consistency levels in DocumentDB}.
#' @param userAgent Optional. A string that specifies the client user agent performing the request. The recommended format is {user agent name}/{version}. For example, the official DocumentDB .NET SDK sets the User-Agent string to Microsoft.Document.Client/1.0.0.0. A custom user-agent could be something like ContosoMarketingApp/1.0.0.
#'
#' @return Check result and some information extracted from the REST API response such as request charge and session token.
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
#' )
#'
#' # check if the MyDatabase database exists
#' if (existsDatabase(myDocDB, "MyDatabase")$result) {
#'   print("Yes, the database exists.")
#' } else {
#'   print("No, the database does not exist.")
#' }
existsDatabase <-
  function(connectionInfo,
           databaseId,
           consistencyLevel = "",
           sessionToken = "",
           userAgent = "") {

      # initialization
      resourceLink <- paste0("dbs/", databaseId)
      getUrl <- paste0(connectionInfo$accountUrl, "/", resourceLink)
      requestCharge <- 0

      # query result
      # prepare GET call
      currentHttpDate <- httr::http_date(Sys.time())
      authorizationToken <- generateAuthToken(
      verb = "GET",
      resourceType = "dbs",
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
        "x-ms-consistency-level" = consistencyLevel,
        "x-ms-session-token" = sessionToken,
        "User-Agent" = userAgent
      )
    #, verbose()
    )

      # process response
      completeResultFromJson <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))
      if (floor(response$status_code / 100) != 2) {
          # error
          # return FALSE if error is 404
          if (response$status_code == 404) {
              result <- FALSE
          } else {
              # stop with an error message
              errorMessage <- paste0(
          "A ", completeResultFromJson$code, " error occured during DocumentDB querying."
          , " Error Message: ", completeResultFromJson$message
        )
              stop(errorMessage)
          }
      } else {
          # no error
          # return TRUE
          result <- TRUE
      }
      requestCharge <- as.numeric(response$headers$`x-ms-request-charge`)
      sessionToken <- response$headers$`x-ms-session-token`

      # return result
      list(
      result = result,
      requestCharge = requestCharge,
      sessionToken = sessionToken
    )
  }