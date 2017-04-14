# helper function to escape text for JSON
escapeTextForJson <- function(text) {
    result <- text
    result <- gsub("\\", "\\\\", result, fixed = TRUE)
    result <- gsub("\"", "\\\"", result, fixed = TRUE)
    result <- gsub("\\b", "\\b", result, fixed = TRUE)
    result <- gsub("\\f", "\\f", result, fixed = TRUE)
    result <- gsub("\n", "\\n", result, fixed = TRUE)
    result <- gsub("\r", "\\r", result, fixed = TRUE)
    result <- gsub("\t", "\\t", result, fixed = TRUE)
    result
}