library(jsonlite)
library(digest)

generateSessionId <- function() {
  # Generate a random session ID using timestamp and random number
  paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "-", digest(runif(1), algo = "md5"))
}

# Get cookie expiration date (1 day from now)
getCookieExpiration <- function() {
  format(Sys.time() + 24*60*60, "%a, %d %b %Y %H:%M:%S GMT", tz = "GMT")
}

# Save session data
saveSessionData <- function(session_id, tasks, base_dir = "sessions") {
  dir.create(base_dir, showWarnings = FALSE, recursive = TRUE)
  file_path <- file.path(base_dir, paste0(session_id, ".json"))
  write_json(tasks, file_path, pretty = TRUE)
}

# Load session data
loadSessionData <- function(session_id, base_dir = "sessions") {
  file_path <- file.path(base_dir, paste0(session_id, ".json"))
  if (file.exists(file_path)) {
    return(read_json(file_path, simplifyDataFrame = TRUE))
  }
  return(NULL)
}