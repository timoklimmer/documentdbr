# generates an auth token for the later POST
# note: this is a port of the C# version provided at https://docs.microsoft.com/en-us/rest/api/documentdb/access-control-on-documentdb-resources
generateAuthToken <- function(
    verb,
    resourceType,
    resourceLink,
    date,
    key,
    keyType,
    tokenVersion) {

        # compute and return result
        verb <- tolower(verb)
        resourceType <- tolower(resourceType)
        date <- tolower(date)
        payLoad <- paste(paste(verb, resourceType, resourceLink, date, "", sep = "\n"), "\n", sep = "")
        hashPayLoad <- openssl::sha256(payLoad, jsonlite::base64_dec(key))
        signature <- jsonlite::base64_enc(hex2raw(hashPayLoad))
        result <- paste(tolower(utils::URLencode(paste("type=", keyType, "&ver=", tokenVersion, "&sig=", sep = ""), reserved = TRUE)), utils::URLencode(signature, reserved = TRUE), sep = "")
        result
}