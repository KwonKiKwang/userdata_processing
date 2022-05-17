# common Fx
check_response <- function(response)
{
  if(httr::http_error(response)){
    message <- paste0(http_condition(response, "message"), content(response, as="text"))
    stop(message)
  }
  response
}

extract_content <- function(response)
{
  jsonlite::fromJSON(content(response, as = "text"))
}

tidy_output <- function(content, simplify)
{
  if(!simplify){return(content)}
  if(is.data.frame(content)){return(content)}
  if(length(content) == 0){return(content)}
  
  #Stop redundant warnings
  old <- options(warn = -1)
  result <- Reduce(cbind, lapply(content, as.data.frame))
  options(old)
  names(result) <- str_replace_all(names(result), "\\.", "_")
  result
}
