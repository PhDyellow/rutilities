#' Print state of system
#'
#' Outputs a set of details about the system that is currently running
#'
#'
#' Outputs:
#'   System time,
#'   R version
#'   OS details
#'   
#' 
track_system_state <- function(){
  sysState <- list()
  sysState$system <- paste0(capture.output(sessionInfo()), collapse="\n")
  sysState$time <- Sys.time()
  return(sysState)
}

#' Check that a git project is clean
#'
#' Check that a git project has no uncommitted changes or untracked files, and stop if
#' they are found
is_git_repo_clean <- function(git_repo = '.', stop_if_false = TRUE, verbose = FALSE){
  if(!dir.exists(git_repo)){
    stop(paste0("Specified repo path does not exist: (", git_repo, ")"))
  }
  if(!git2r::in_repository(git_repo)){
    stop(paste0("Not a git repository: " , git_repo))
  }

  gitState <- system2("git", c("-C", git_repo, "status", "--porcelain"),stderr = TRUE, stdout = TRUE)

  if (length(grep("\\?\\?", gitState, invert = TRUE)) != 0){
    if (verbose){
      message("Git directory has uncommitted changes")
      print(git2r::status())
    }
    if( stop_if_false){
       stop("Git directory has uncommitted changes", git2r::status())
    } else {
      return(FALSE)
    }
  }



  if (length(grep("\\?\\?", gitState, invert = FALSE)) != 0) {
    if (verbose){
      message("Git directory has untracked changes")
      print(git2r::status())
    }
    if(stop_if_false){
      stop("Git directory has untracked changes", git2r::status())
    } else {
      return(FALSE)
    }
 }
  return(TRUE)
}


#' Print Git state of a project

track_git_status <- function(git_repo = '.'){

  if(!dir.exists(git_repo)){
    stop(paste0("Specified repo path does not exist: (", git_repo, ")"))
  }
  gitRepo <- list()
  gitRepo$repo <-capture.output(git2r::repository_head(repo=git_repo))

  gitRepo$status <- capture.output(git2r::status(repo=git_repo))
  return(gitRepo)

}

#' Print R and package states
track_packages <- function(){
  installed.packages()[, c("Package", "Version", "Built")]
}


#' Bring together all state information
track_all_states <- function(git_repo = "."){
  state <- list()
  state$git <- track_git_status(git_repo = git_repo)
  state$packages <- track_packages()
  state$system <- track_system_state()
  return(state)
}
