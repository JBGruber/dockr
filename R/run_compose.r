#' Pull, create and start all conainters defined in a Docker Compose file
#'
#' @param compose Path to a Docker Compose file. You can also parse the file
#'   first, change it and supply it as an object.
#' @param force_install If TRUE, removes all containers and re-creates them from
#'   the compose file. If 2, the images are also re-downloaded. Danger: this
#'   will destroy the indexes in your containers!
#'
#' @export
compose_up <- function(compose, force_install = FALSE) {

  docker_ping()
  if (any(methods::is(compose) != "docker_config")) {
    config <- parse_yml(x = compose)
  } else {
    config <- compose
  }

  # set up networks
  for (n in names(config$networks)) {
    if (!n %in% docker_ln()$name) {
      docker_create_network(n)
    }
  }

  # check missing images and containers
  conf_imgs <- s_pull(config$services, "Image")
  conf_cont <- s_pull(config$services, "container_name")
  conf_cont <- ifelse(conf_cont == "", praise::praise("${adjective}_${creating}"), conf_cont)

  imgs <- docker_li(reference = conf_imgs)$tags
  containers <- docker_lc(name = conf_cont, all = TRUE)$name

  if (force_install > 0) {
    lapply(containers, function(c) {
      docker_stop(c)
      docker_rmc(c, force = TRUE)
    })
    containers <- character()
    if (force_install > 1) {
      lapply(imgs, function(i) docker_rmi(i, force = TRUE))
      imgs <- character()
    }
  }

  # create images
  lapply(config$services, function(service)
    if(!service$Image %in% imgs)
      docker_create_image(image = service$Image))

  # create containers
  ids <- lapply(config$services, function(service)
    if (!service$container_name %in% containers)
      docker_create_container(name = service$container_name, body = service)
    else
      return(NA))

  if (any(sapply(ids, is.null))) stop("container(s) ", toString(names(ids)[sapply(ids, is.null)]), " could not be created")

  # start containers
  lapply(config$services, function(service)
    docker_start(service$container_name))

  if (all(conf_cont %in% docker_lc(name = conf_cont)$name)) {
    message("all done!")
  }
}
