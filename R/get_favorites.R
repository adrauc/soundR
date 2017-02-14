#' get tracks favorited by the user
#'
#' @description Returns a data frame with tracks favorited by the user
#'
#' @param user_id Numeric, has to be a valid user id of Soundcloud user
#' @param client_id Has to be a valid client ID
#'
#' @seealso \url{https://developers.soundcloud.com/docs/api/reference#users}
#' @examples
#' \dontrun{
#' # get all tracks favorited by a user
#' user_id <- 20753162
#' client_id <- "512fef5d8f512ff221f512ff"
#' favorites_df <- get_favorites(user_id, client_id)
#' favorites_df
#' }
#'
#' @return a data frame with all tracks favorited by the user
#' @export

get_favorites <- function(user_id, client_id) {

   # Helper function: delete country codes

  delete_country <- function(result_list) {
    n_results <- grepl("available_country", result_list)
    if (sum(n_results) == 0) {
      return(result_list)
    }
    if (sum(n_results) == 1) {
        result_list[[which(grepl("available_country", result_list))]]$available_country_codes <- NULL
        return(result_list)
      } else {
        pos_results <- which(n_results)
        for (i in pos_results) {
          result_list[[i]]$available_country_codes <- NULL
        }
        return(result_list)
      }
      }

  if (!is.numeric(user_id)) {
    stop("Please provide user id as numeric")
  }
  if(!length(user_id)==1) {
    stop("Please only provide one user ID")
  }
  user_list <- list()

  path <- paste("http://api.soundcloud.com/users/", user_id,"/favorites","?client_id=",client_id,
                "&page_size=200","&linked_partitioning=1", sep="")
  temp_user <- soundcloud_api(path)$content
  # check, if further pages are available
  if(is.null(temp_user$next_href)){
    temp_user <- temp_user$collection
    # delete country codes, returns messy data
    temp_user <- delete_country(temp_user)
    return(rbind.fill(lapply(temp_user, function(f) {
      as.data.frame(Filter(Negate(is.null), f), stringsAsFactors=F)
    })))
  } else {
    i <- 1
    user_list <- list()
    temp_df <- temp_user$collection
    # delete country codes, returns messy data
    temp_df <- delete_country(temp_df)
    user_list[[i]] <- rbind.fill(lapply(temp_df, function(f) {
      as.data.frame(Filter(Negate(is.null), f), stringsAsFactors=F)
    }))
    href_check <- is.null(temp_user$next_href)
    while(!href_check) {
      i <- i + 1
      temp_user <- soundcloud_api(temp_user$next_href)$content
      temp_df <- temp_user$collection
      temp_df <- delete_country(temp_df)
      user_list[[i]] <- rbind.fill(lapply(temp_df, function(f) {
        as.data.frame(Filter(Negate(is.null), f), stringsAsFactors=F)
      }))
      href_check <- is.null(temp_user$next_href)
    }
    return(rbind.fill(user_list))
  }
}
