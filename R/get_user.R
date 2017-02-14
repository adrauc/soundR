#' get users
#'
#' @description Returns a data frame with user or users
#'
#' @param username A character vector of usernames
#' @param client_id Has to be a valid client ID
#'
#' @seealso \url{https://developers.soundcloud.com/docs/api/reference#users}
#' @examples
#' \dontrun{
#' # lookup user
#' username <- "joshuakatharsis"
#' client_id <- "512fef5d8f512ff221f512ff"
#' user_df <- get_user(username, client_id)
#' user_df
#' }
#'
#' @return a data frame with all requested users
#' @export

get_user <- function(username, client_id) {
  if (!is.character(username)) {
    stop("Please provide usernames as string")
  }
  if(length(username)==1) {
    path <- paste("http://api.soundcloud.com/users/", username,"?client_id=",client_id, sep="")
    user_list <- soundcloud_api(path)$content
    user_list[sapply(user_list, is.null)] <- NA
    #do.call("cbind", lapply(user_list, as.data.frame))
    #rbind.fill(lapply(user_list, function(f) {
    # as.data.frame(Filter(Negate(is.null), f))
    #}))
    # empty subscriptions should be included
    if (length(user_list$subscriptions)==0) {
      user_list$subscriptions <- list(product=list(list(id = NA, name=NA)))
      return(as.data.frame(user_list, stringsAsFactors=F))
    } else {
    return(as.data.frame(user_list, stringsAsFactors=F))
    }

  } else {
    user_list <- list()
    for (i in 1:length(username)) {
      path <- paste("http://api.soundcloud.com/users/", username[i],"?client_id=",client_id, sep="")
      temp_user <- soundcloud_api(path)$content
      # null have to be NA, otherwhise columns are dropped
      temp_user[sapply(temp_user, is.null)] <- NA
      if (length(temp_user$subscriptions)==0) {
        temp_user$subscriptions <- list(product=list(list(id = NA, name=NA)))
        user_list[[i]] <- as.data.frame(temp_user, stringsAsFactors=F)
      } else {
        user_list[[i]] <- as.data.frame(temp_user, stringsAsFactors=F)
      }
    }
    return(rbind.fill(user_list))
  }
}
