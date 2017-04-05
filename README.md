# documentdbr

An R package to query, insert and delete documents from DocumentDB.

# Installation instructions

Install the development version of the package directly from GitHub with:

```r
# Install devtools
if(!require("devtools")) install.packages("devtools")
devtools::install_github("timoklimmer/documentdbr")
```

The package has dependencies on the following R packages:

- `httr`
- `jsonlite`
- `openssl`
- `utils`

In addition, you need a zip utility installed and your path must include the location of this zip utility.  On Linux machines this is usually included by default.  However, on Windows, you may have to install this yourself, e.g. by installing RTools and editing your path to include the RTools location.
