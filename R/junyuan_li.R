#' Combine groups by column
#' @description Combine groups by column
#' @param object a data frame with two or more columns
#' @param id the column for group
#' @param col the coloumn for combinning
#' @param label the new column name
#' @param sep the separator between combined group
#' @param keep keep other columns
#' @param unique unique while combinning
#' @import dplyr
#' @export
table_bind_group <- function(object,id,col,label="groups",sep=";",keep=FALSE,unique=TRUE){
  if(unique){
    object <- object %>%
      ungroup() %>%
      distinct(.data[[id]],.data[[col]],.keep_all = TRUE)
  }
  if(keep){
    object <- object %>%
      group_by(.data[[id]]) %>%
      mutate(!!label := paste0(.data[[col]], collapse = sep)) %>%
      ungroup()
  }else{
    object <- object %>%
      group_by(.data[[id]]) %>%
      mutate(!!label := paste0(.data[[col]], collapse = sep)) %>%
      select(.data[[label]]) %>%
      distinct(.data[[id]],.data[[label]])
  }
  return(object)
}
