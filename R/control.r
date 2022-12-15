# control functions
docker_start <- function(id) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("containers", id, "start") |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  message(
    switch(as.character(httr2::resp_status(res)),
           "204" = paste(id, "started"),
           "304" = "container already started",
           "404" = "no such container",
           "500" = "server error")
  )

}


#' Stop container
#'
#' @param id ID or name of the container
#' @param kill_after Number of seconds to wait before killing the container
#'
#' @export
docker_stop <- function(id, kill_after = 60L) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("containers", id, "stop") |>
    httr2::req_method("post") |>
    httr2::req_url_query(t = kill_after) |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  message(
    switch(as.character(httr2::resp_status(res)),
           "204" = paste(id, "stopped"),
           "304" = "container already stopped",
           "404" = "no such container",
           "500" = "server error")
  )
}


#' Execute commands inside a running container
#'
#' @param id ID or name of container.
#' @param cmd command to be run.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' docker_exec(id = "amcat4",
#'             cmd = "amcat4 create-admin --username admin --password supergeheim")
#'
#' docker_exec(id = "amcat4",
#'             cmd = "amcat4 create-test-index")
#' }
docker_exec <- function(id, cmd) {

  # make exec instance
  req <- docker_base_req() |>
    httr2::req_url_path_append("containers", id, "exec") |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_body_json(list(
        "AttachStdin" =  TRUE,
        "AttachStdout" =  TRUE,
        "AttachStderr" =  TRUE,
        "Cmd" = as.list(unlist(strsplit(cmd, split = " ", fixed = TRUE)))
    ))

  res <- httr2::req_perform(req) |>
    httr2::resp_body_json()

  # run instance
  req2 <- docker_base_req() |>
    httr2::req_url_path_append("exec", res$Id, "start") |>
    httr2::req_method("post") |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_body_json(list(
      "Detach" =  FALSE,
      "Tty" = FALSE
    ))

  res2 <- httr2::req_perform(req2)
  if (length(res2$body) > 0) {
    message(httr2::resp_body_string(res2))
  }

}

