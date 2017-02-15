#' get the favoriters for a track
#'
#' @description Get all favoriters (users) for a track
#'
#' @param track_id Numeric, has to be a valid track id
#' @param client_id Has to be a valid client ID
#'
#' @seealso \url{https://developers.soundcloud.com/docs/api/reference#users}
#' @examples
#' \dontrun{
#' # get all comments for a track
#' track_id <- 152955202
#' client_id <- "512fef5d8f512ff221f512ff"
#' favoriters_df <- get_tfavoriters(track_id, client_id)
#' favoriters_df
#' }
#'
#' @return a data frame with all comments of a user
#' @export


get_tfavoriters <- function(track_id, client_id) {
  if (!is.numeric(track_id)) {
    stop("Please provide user id as numeric")
  }
  if(!length(track_id)==1) {
    stop("Please only provide one user ID")
  }
  user_list <- list()

  path <- paste("http://api.soundcloud.com/tracks/", track_id,"/favoriters","?client_id=",client_id,
                "&page_size=200","&linked_partitioning=1", sep="")
  temp_user <- soundcloud_api(path)$content
  if(is.null(temp_user$next_href)){
    temp_df <- temp_user$collection
    # replace empty lists in the list with new data structure
    for (j in 1:length(temp_df)) {
      if (length(temp_df[[j]]$subscriptions)==0) {
        temp_df[[j]]$subscriptions <- list(product=list(list(id = NA, name=NA)))
      } else {
        next
      }
    }
    return(rbind.fill(lapply(temp_df, function(f) {
      as.data.frame(Filter(Negate(is.null), f), stringsAsFactors=F)
    })))
  } else {
    i <- 1
    temp_df <- temp_user$collection
    # replace empty lists in the list with new data structure
    for (j in 1:length(temp_df)) {
      if (length(temp_df[[j]]$subscriptions)==0) {
        temp_df[[j]]$subscriptions <- list(product=list(list(id = NA, name=NA)))
      } else {
        next
      }
    }
    user_list[[i]] <- rbind.fill(lapply(temp_df, function(f) {
      as.data.frame(Filter(Negate(is.null), f), stringsAsFactors=F)
    }))
    href_check <- is.null(temp_user$next_href)
    # get further pages with data
    while(!href_check) {
      i <- i + 1
      temp_user <- soundcloud_api(temp_user$next_href)$content
      temp_df <- temp_user$collection
      # replace empty lists in the list with new data structure
      for (j in 1:length(temp_df)) {
        if (length(temp_df[[j]]$subscriptions)==0) {
          temp_df[[j]]$subscriptions <- list(product=list(list(id = NA, name=NA)))
        } else {
          next
        }
      }
      user_list[[i]] <- rbind.fill(lapply(temp_df, function(f) {
        as.data.frame(Filter(Negate(is.null), f), stringsAsFactors=F)
      }))
      href_check <- is.null(temp_user$next_href)
    }
    return(rbind.fill(user_list))
  }
}
