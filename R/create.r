# create functions
docker_create_image <- function(image, verbose = FALSE) {

  if (is.null(image)) stop("name can not be NULL")

  # create image
  req <- docker_base_req() |>
    httr2::req_url_path_append("images/create") |>
    httr2::req_url_query(fromImage = image) |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  if (!is.null(res$body)) res <- httr2::resp_body_raw(res)
  if (verbose) return_status(3L, res)
}

docker_create_container <- function(name, body) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("containers/create") |>
    httr2::req_url_query(name = name) |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_body_json(body)

  res <- httr2::req_perform(req) |>
    httr2::resp_body_json()

  if (length(res$Warnings) > 0) warning(res$Warnings)
  invisible(res$Id)
}


docker_create_network <- function(name) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("networks/create") |>
    httr2::req_url_query(name = name) |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_body_json(list(Name = name))

  res <- httr2::req_perform(req) |>
    httr2::resp_body_json()

  if (nchar(res$Warning) > 0) warning(res$Warnings)
  if (length(res$Id) > 0) message("Network ", name, " created. ID: ", res$Id)
  invisible(res$Id)

}

