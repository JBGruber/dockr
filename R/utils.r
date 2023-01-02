#' Parse Docker Compose Yaml file
#'
#' @param x A yaml/yml Docker Compose file
#'
#' @return A data.frame with image/container configurations
#' @export
#'
#' @examples
#' parse_yml(system.file("extdata/docker-compose.yml", package = "dockr"))
parse_yml <- function(x) {

  compose <- yaml::read_yaml(x)

  compose$services <- lapply(compose$services, function(s) {

    # rename
    pattern <- c("image", "networks", "environment", "command", "ports", "volumes", "restart")
    replacement <- c("Image", "NetworkMode", "Env", "Cmd", "PortBindings", "Binds", "RestartPolicy")
    for(i in seq_along(pattern)) names(s) <- gsub(pattern[i], replacement[i], names(s), fixed = TRUE)

    # reformat some special arguments
    if ("PortBindings" %in% names(s)) {
      local <- gsub(":.+$", "", s$PortBindings)
      container <- gsub("^.+?:", "", s$PortBindings)
      s$PortBindings <- list(data.frame(HostPort = local))
      names(s$PortBindings) <- paste0(container, "/tcp")
    }
    if ("Env" %in% names(s)) {
      if (!is.null(names(s$Env))) {
        env <- paste0(names(s$Env), "=", s$Env)
        s$Env <- as.list(env)
      }
    }
    if ("Binds" %in% names(s)) {
      if (grepl("~", s$Binds, fixed = TRUE)) {
        local <- normalizePath(gsub(":.+$", "", s$Binds))
        s$Binds <- gsub("^.+?(?=:)", local, s$Binds, perl = TRUE)
      }
      s$Binds <- I(s$Binds)
    }
    if ("RestartPolicy" %in% names(s)) {
      s$RestartPolicy <- list(Name = s$RestartPolicy)
    }

    return(s)
  })

  compose$version <- NULL
  class(compose) <- c("docker_config", class(compose))
  return(compose)
}


docker_base_req <- function(verbose = FALSE) {
  if (R.Version()$os == "mingw32") {
    req <- httr2::request("http://localhost:2375")
  } else {
    req <- httr2::request("http://localhost:80") |>
      httr2::req_options(UNIX_SOCKET_PATH = "/var/run/docker.sock")
  }
  if(verbose) req <- httr2::req_options(req, debugfunction = return_status, verbose = TRUE)
  return(req)
}


return_status <- function(type, sts) {
  if (type == 3) {
    sts <- readBin(sts, character())
    lines <- unlist(strsplit(sts, "\r?\n", useBytes = TRUE))
    msgs <- jsonlite::stream_in(textConnection(grep("^\\{", lines, value = TRUE)), verbose = FALSE)
    try({
      msgs <- unlist(msgs[!is.na(msgs)], recursive = TRUE)
      for (msg in msgs) {
        message(msg)
      }
    })
  }
}

resp_body_message <- function(x) {
  dat <- httr2::resp_body_raw(x)
  lines <- readBin(dat, "character", n = length(dat) / 4)
  lines <- gsub("[^[:alnum:][:blank:]?&/\\-]", "", lines[lines != ""], perl = TRUE)
  return(lines[lines != ""])
}



#' Ping Docker daemon
#'
#' Fails if Docker daemon is not reachable. On Unix systems, you want to either
#' run Docker in rootless mode (
#' \href{https://docs.docker.com/engine/install/linux-postinstall/}{see the
#' Docker documentation}) or start R with sudo for the correct priviliges. On
#' Windows, you should expose daemon on tcp://localhost:2375 without TLS (e.g.,
#' from Docker Desktop setting).
#'
#' @export
docker_ping <- function() {
  res <- try(
    docker_base_req() |>
      httr2::req_url_path_append("_ping") |>
      httr2::req_method("get") |>
      httr2::req_perform()
  )
  if (methods::is(res, "try-error")) {
    stop("The Docker daemon is not reachble (check if it is running and if you have the correct permissions)")
  }
  invisible(!methods::is(res, "try-error"))
}

# safe pull string from list
s_pull <- function(x, ...) {
  vapply(x, function(x) ifelse(length(x[[...]]) == 0, "", toString(x[[...]])), character(1))
}

#' List docker containers
#'
#' @param ... named values are used as filters.
#' @param all include stopped containers.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' docker_lc(name = c("amcat", "elastic"))
#' }
docker_lc <- function(..., all = FALSE) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("/containers/json") |>
    httr2::req_method("get") |>
    httr2::req_url_query(all = tolower(all))

  dots <- list(...)
  if (length(dots) > 0) req <- httr2::req_url_query(req, filters = jsonlite::toJSON(dots))

  res <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  out <- tibble::tibble(
    name = s_pull(res, "Names"),
    image = s_pull(res, "Image"),
    status = s_pull(res, "Status"),
    id = s_pull(res, "Id"),
    ports = s_pull(res, "Ports")
  )
  out$name = gsub("^/", "", out$name)
  return(out)
}


#' List docker images
#'
#' @param ... named values are used as filters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' docker_li(reference = "amcat4docker")
#' }
docker_li <- function(...) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("/images/json") |>
    httr2::req_method("get")

  dots <- list(...)
  if (length(dots) > 0) req <- httr2::req_url_query(req, filters = jsonlite::toJSON(dots))

  res <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  tibble::tibble(
    labels = s_pull(res, "Labels"),
    tags = s_pull(res, "RepoTags"),
    id = s_pull(res, "Id")
  )
}

#' List docker networks
#'
#' @param ... named values are used as filters.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' docker_ln()
#' }
docker_ln <- function(...) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("/networks") |>
    httr2::req_method("get")

  dots <- list(...)
  if (length(dots) > 0) req <- httr2::req_url_query(req, filters = jsonlite::toJSON(dots))

  res <- req |>
    httr2::req_perform() |>
    httr2::resp_body_json()

  tibble::tibble(
    name = sapply(res, function(x) x[["Name"]]),
    labels = sapply(res, function(x) toString(x[["Labels"]])),
    driver = sapply(res, function(x) x[["Driver"]]),
    id = sapply(res, function(x) x[["Id"]])
  )
}


#' Inspect Docker container
#'
#' Retrieves existing container as json string
#'
#' @param id ID or name of container.
#' @param parse parse json string
#'
#' @export
docker_inspect_container <- function(id, parse = FALSE) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("containers", id, "json") |>
    httr2::req_method("get") |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  if (parse) {
    return(httr2::resp_body_json(res))
  } else {
    return(httr2::resp_body_string(res))
  }

}

docker_inspect_network <- function(id, as_json = TRUE) {

  req <- docker_base_req() |>
    httr2::req_url_path_append("networks", id) |>
    httr2::req_method("get") |>
    httr2::req_error(is_error = function(resp) FALSE)

  res <- httr2::req_perform(req)

  if (as_json) {
    return(httr2::resp_body_string(res))
  } else {
    return(httr2::resp_body_json(res))
  }

}
