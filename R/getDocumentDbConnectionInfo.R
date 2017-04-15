#' Returns an object which contains connection information to a DocumentDB collection.
#'
#' @param accountUrl The URI of the DocumentDB account.
#' @param primaryOrSecondaryKey The master key to authenticate.
#' @param databaseId The ID of the database to query/modify.
#' @param collectionId The ID of the collection to query/modify.
#'
#' @return An object that can be passed to other functions of this package.
#' @export
#'
#' @examples
#' # get a DocumentDbConnectionInfo object
#' myDocDB <- getDocumentDbConnectionInfo(
#'   accountUrl = "https://somedocumentdbaccount.documents.azure.com",
#'   primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
#'   databaseId = "MyDatabaseId",
#'   collectionId = "MyCollectionId"
#' )
getDocumentDbConnectionInfo <- function(
  accountUrl,
  primaryOrSecondaryKey,
  databaseId,
  collectionId
) {
    list(
        accountUrl = accountUrl,
        primaryOrSecondaryKey = primaryOrSecondaryKey,
        databaseId = databaseId,
        collectionId = collectionId
    )
}