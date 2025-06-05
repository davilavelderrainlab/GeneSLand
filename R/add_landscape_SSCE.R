#' Add landscape view
#'
#' @param ssce The specificity SingleCellExperiment object
#' @param addClusters A boolean variable to define if clusters should be computed
#'
#' @return The specificity SingleCellExperiment object with the UMAP coordinates
#' and colors saved in colData
#' (if addClusters is TRUE, also the membership vector saved in 'cluster')
#' @export
#'
#' @examples
#' set.seed(123)
#' profile <- matrix(sample(seq(1, 100, by = 0.1), size = 1000, replace = TRUE), ncol=100)
#' rownames(profile) <- paste0('Gene-', seq(1, nrow(profile)))
#' out <- initSSCE(profile)
#' view <- add_landscape_SSCE(out)
add_landscape_SSCE <- function(ssce,
                               addClusters=FALSE) {

  # check the availability of a containerization platform:
  check_docker <- system("which docker",
                         ignore.stdout = TRUE,
                         ignore.stderr = TRUE) == 0
  check_singularity <- system("which singularity",
                              ignore.stdout = TRUE,
                              ignore.stderr = TRUE) == 0
  check_moduleLoad <- system("which module",
                             ignore.stdout = TRUE,
                             ignore.stderr = TRUE) == 0

  possibleError <- "Error: neither Docker nor Singularity are present, please follow instructions at https://github.com/davilavelderrainlab/GeneSLand"

  if (!(check_docker || check_singularity || check_moduleLoad)){
    print(possibleError)
    return()
  } else if (check_moduleLoad){
    check_moduleLoadSingularity <-  system("module load singularity",
                                           ignore.stdout = TRUE,
                                           ignore.stderr = TRUE) == 0
    if(!check_moduleLoadSingularity){
      print(possibleError)
      return()
    }
  }


  # tmp dir and I/O
  temp_dir <- tempdir()
  temp_input <- file.path(temp_dir, "input.rds")
  temp_output <- file.path(temp_dir, "output.rds")

  saveRDS(ssce, temp_input)

  # build a temporary script to run ACTIONet commands in R - container
  temp_script <- file.path(temp_dir, "process_ssce.R")

  script_content <- c(
    "library(ACTIONet)",
    "ssce <- readRDS('/data/input.rds')",
    "G <- ACTIONet::buildNetwork(as.matrix(SingleCellExperiment::counts(ssce)), distance_metric = 'l2')",
    "o <- irlba::irlba(as.matrix(SingleCellExperiment::counts(ssce)), k = 20)",
    "Coords <- ACTIONet::layoutNetwork(G, initial_position = o$v, n_epochs = 500, spread = 1)",
    "ssce@colData$x <- Coords$coordinates[,1]",
    "ssce@colData$y <- Coords$coordinates[,2]",
    "ssce@colData$col <- grDevices::rgb(Coords$colors)",
    if(addClusters) {
      c("C <- ACTIONet::clusterNetwork(G)",
        "C[C%in%as.numeric(names(which(table(C)==1)))] <- 0",
        "ssce@colData$cluster <- C")
    } else NULL,
    "saveRDS(ssce, '/data/output.rds')"
  )
  writeLines(script_content, temp_script) # copy R commands in the script file


  #

  if (check_docker){
    system2("docker", args = c(
      "run", "--rm",
      "--platform=linux/amd64",
      "-v", paste0(normalizePath(temp_dir), ":/data"),
      "beatricepaolucci/actionet_r_release:latest",
      "Rscript /data/process_ssce.R"
    ))
  } else if (check_singularity){
    system2("singularity", args = c(
      "exec",
      "--bind", paste0(normalizePath(temp_dir), ":/data"),
      "docker://beatricepaolucci/actionet_r_release:latest",
      "Rscript /data/process_ssce.R"
    ))
  } else if (check_moduleLoad){ # if singularity is not available, try loading it

    full_cmd <- paste0("bash -c 'module load singularity && singularity exec --cleanenv --no-home --bind ",
                       normalizePath(temp_dir), ":/data docker://beatricepaolucci/actionet_r_release:latest Rscript /data/process_ssce.R'")
    system(full_cmd)
  }

  readRDS(temp_output)
}
