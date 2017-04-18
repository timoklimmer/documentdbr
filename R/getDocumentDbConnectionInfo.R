#' Returns an object which contains connection information to a DocumentDB collection.
#'
#' @param accountUrl The URI of the DocumentDB account.
#' @param primaryOrSecondaryKey The master key to authenticate.
#' @param databaseId Optional. The ID of the database to work with. Not needed if operations are on account level.
#' @param collectionId Optional. The ID of the collection to work with. Not needed if operations are on database level.
#'
#' @return An object that can be passed to other functions of this package.
#' @export
#'
#' @examples
#' # get a DocumentDBConnectionInfo object
#' myDocDB <- getDocumentDBConnectionInfo(
#'   accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'   primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'   databaseId = "MyDatabaseId",
#'   collectionId = "MyCollectionId"
#' )
getDocumentDBConnectionInfo <- function(
  accountUrl,
  primaryOrSecondaryKey,
  databaseId = "",
  collectionId = ""
) {
    list(
        accountUrl = accountUrl,
        primaryOrSecondaryKey = primaryOrSecondaryKey,
        databaseId = databaseId,
        collectionId = collectionId
    )
}