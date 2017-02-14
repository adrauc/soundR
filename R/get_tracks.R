#' get the tracks of a user
#'
#' @description Get all tracks of a user
#'
#' @param user_id Numeric, has to be a valid user id of Soundcloud user
#' @param client_id Has to be a valid client ID
#'
#' @seealso \url{https://developers.soundcloud.com/docs/api/reference#users}
#' @examples
#' \dontrun{
#' # get all tracks of a user
#' user_id <- 20753162
#' client_id <- "512fef5d8f512ff221f512ff"
#' tracks_df <- get_tracks(user_id, client_id)
#' tracks_df
#' }
#'
#' @return a data frame with all tracks of a user
#' @export

get_tracks <- function(user_id, client_id=client_id) {
  if (!is.numeric(user_id)) {
    stop("Please provide user id as numeric")
  }
  if(!length(user_id)==1) {
    stop("Please only provide one user ID")
  }
  user_list <- list()

  path <- paste("http://api.soundcloud.com/users/", user_id,"/tracks","?client_id=",client_id,
                "&page_size=200","&linked_partitioning=1", sep="")
  temp_user <- soundcloud_api(path)$content
  # give error back, that user has no tracks
  if(length(temp_user[[1]]) == 0) {
    stop("No tracks could be found for this user")
  }
  if(is.null(temp_user$next_href)) {
    temp_user <- temp_user$collection
    return(rbind.fill(lapply(temp_user, function(f) {
      as.data.frame(Filter(Negate(is.null), f), stringsAsFactors=F)
    })))
  } else {
    i <- 1
    user_list <- list()
    temp_df <- temp_user$collection
    user_list[[i]] <- rbind.fill(lapply(temp_df, function(f) {
      as.data.frame(Filter(Negate(is.null), f), stringsAsFactors=F)
    }))
    href_check <- is.null(temp_user$next_href)
    while(!href_check) {
      i <- i + 1
      temp_user <- soundcloud_api(temp_user$next_href)$content
      temp_df <- temp_user$collection
      user_list[[i]] <- rbind.fill(lapply(temp_df, function(f) {
        as.data.frame(Filter(Negate(is.null), f), stringsAsFactors=F)
      }))
      href_check <- is.null(temp_user$next_href)
    }
    return(rbind.fill(user_list))
  }
}
