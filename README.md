# documentdbr

An R package to use DocumentDB from R.

# Installation instructions

If you have not yet done before, you may have to install RTools and load the devtools package first.

Then install the development version of the package directly from GitHub with:

```r
# Install devtools
if(!require("devtools")) install.packages("devtools")
devtools::install_github("timoklimmer/documentdbr")
```

The package has dependencies on the following R packages:

- httr (>= 1.2.1),
- jsonlite (>= 1.1),
- openssl (>= 0.9.5),
- utils

# Usage

Check out the documentation by running `help(package = documentdbr)`.

This example shows how to run and show the results of a SELECT statement.

```r
# load the documentdbr package
library(documentdbr)

# setup connection infos
myCollection <- getDocumentDBConnectionInfo(
  accountUrl = "https://somedocumentdbaccount.documents.azure.com",
  primaryOrSecondaryKey = "t0C36UstTJ4c6vdkFyImkaoB6L1yeQidadg6wasSwmaK2s8JxFbEXQ0e3AW9KE1xQqmOn0WtOi3lxloStmSeeg==",
  databaseId = "MyDatabaseId",
  collectionId = "MyCollectionId"
)

# run a SQL query and print its results
queryResult <- selectDocuments(myCollection, "SELECT Items.name, Items.description, Items.isComplete FROM Items WHERE Items.isComplete = true")
queryResult$documents
```

# What's New?

See the NEWS file for infos on the latest changes. The NEWS file is also available through the package documentation.