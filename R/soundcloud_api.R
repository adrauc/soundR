#' GET from the Soundcloud API
#'
#' @description Returns a Soundcloud object
#'
#' @param path Character, has to be a valid string for HTTP method GET.
#'
#' @examples
#' \dontrun{
#' # send a GET command as HTML to the Soundcloud API
#' path <- "http://api.soundcloud.com/users/3207?client_id=YOUR_CLIENT_ID"
#'
#' json_response <- soundcloud_api(path)
#' json_response
#' }
#'
#' @return a Soundcloud API object
#' @imports jsonlite
#' @imports plyr
#' @imports httr
#' @export

soundcloud_api <- function(path) {
  if (!is.character(path)) {
    stop("provided link is no string", call. = FALSE)
  }
  url <- path

  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop(
      sprintf(
      "API did not return json: %s",
      http_status(resp)$message
    ),
    call. = FALSE
    )
  }
  # resp
  parsed <- fromJSON(content(resp, as="text", encoding = "UTF-8"),
                               simplifyVector = FALSE)

  if (http_error(resp)) {
    stop(
      sprintf(
        "Soundcloud API request failed %s",
        parsed[[1]][[1]]
      ),
      call. = FALSE
    )
  }

  structure(
    list(
      content = parsed,
      path = path,
      response = resp
    ),
    class = "soundcloud_api"
  )
}

print.soundcloud_api <- function(x, ...) {
  cat("<SoundCloud ", x$path, ">\n", sep = "")
  utils::str(x$content)
  invisible(x)
}
