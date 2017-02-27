#' get the followings of a user
#'
#' @description Returns a data frame with all followings (followed users)
#'
#' @param user_id Numeric, has to be a valid user id of Soundcloud user
#' @param client_id Has to be a valid client ID
#'
#' @seealso \url{https://developers.soundcloud.com/docs/api/reference#users}
#' @examples
#' \dontrun{
#' # get all followings of a user
#' user_id <- 20753162
#' client_id <- "512fef5d8f512ff221f512ff"
#' followings_df <- get_followings(user_id, client_id)
#' followings_df
#' }
#'
#' @return a data frame with all followings of a user
#' @export

get_followings <- function(user_id, client_id=client_id) {
  if (!is.numeric(user_id)) {
    stop("Please provide user id as numeric")
  }
  if(!length(user_id)==1) {
    stop("Please only provide one user ID")
  }
  user_list <- list()

  path <- paste("http://api.soundcloud.com/users/", user_id,"/followings","?client_id=",client_id,
                "&page_size=200","&linked_partitioning=1", sep="")
  temp_user <- soundcloud_api(path)$content
  if(length(temp_user[[1]]) == 0) {
    stop("No followings could be found for this user")
  }
  if(is.null(temp_user$next_href)) {
    temp_user <- temp_user$collection
    return(rbind.fill(lapply(temp_user, function(f) {
      as.data.frame(Filter(Negate(is.null), f), stringsAsFactors=F)
    })))
  } else {
    i <- 1
    total <- ceiling(get_user(as.character(user_id), client_id)$followings_count/200)
    pb <- txtProgressBar(min = 0, max = total, style = 3)
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
      setTxtProgressBar(pb, i)
      href_check <- is.null(temp_user$next_href)
    }
    return(rbind.fill(user_list))
  }
}
