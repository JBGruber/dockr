## remove images
docker_rmi <- function(image, force = FALSE, noprune = FALSE) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("images", image) |>
    httr2::req_method("delete") |>
    httr2::req_url_query(force = tolower(force),
                         noprune = tolower(noprune)) |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req) |>
    httr2::resp_body_json()

  if (length(res$Warnings) > 0) warning(res$Warnings)
  invisible(res$Id)

}

## remove containers
docker_rmc <- function(container, force = FALSE, link = FALSE) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("containers/", container) |>
    httr2::req_method("delete") |>
    httr2::req_url_query(force = tolower(force),
                         link = tolower(link)) |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  if (length(res$body) > 0) res <- httr2::resp_body_json(res)
  if (length(res$Warnings) > 0) warning(res$Warnings)
  if (length(res$message) > 0) message(res$message)
  invisible(res$Id)
}


## remove networks?

