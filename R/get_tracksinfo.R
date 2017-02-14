#' get information about tracks
#'
#' @description Get all tracks of a user
#'
#' @param track_id Numeric, has to be a valid track id
#' @param client_id Has to be a valid client ID
#'
#' @seealso \url{https://developers.soundcloud.com/docs/api/reference#users}
#' @examples
#' \dontrun{
#' # get all comments of a user
#' track_id <- 285874395
#' client_id <- "512fef5d8f512ff221f512ff"
#' tracks_df <- get_tracksinfo(track_id, client_id)
#' tracks_df
#' }
#'
#' @return a data frame with all tracks of a user
#' @export

get_tracksinfo <- function(track_id, client_id) {
if (!is.numeric(track_id)) {
stop("Please provide track id as numeric")
}
  if(length(track_id)==1) {
    path <- paste("http://api.soundcloud.com/tracks/", track_id,"?client_id=",client_id, sep="")
    track_list <- soundcloud_api(path)$content
    track_list[sapply(track_list, is.null)] <- NA
    #do.call("cbind", lapply(track_list, as.data.frame))
    #rbind.fill(lapply(track_list, function(f) {
    # as.data.frame(Filter(Negate(is.null), f))
    #}))
    return(as.data.frame(track_list, stringsAsFactors=F))


  } else {
    track_list <- list()
    for (i in 1:length(track_id)) {
      path <- paste("http://api.soundcloud.com/tracks/", track_id[i],"?client_id=",client_id, sep="")
      temp_track <- soundcloud_api(path)$content
      # null have to be NA, otherwhise columns are dropped
      temp_track[sapply(temp_track, is.null)] <- NA
      track_list[[i]] <- as.data.frame(temp_track, stringsAsFactors=F)
    }
    return(rbind.fill(track_list))
  }
}
