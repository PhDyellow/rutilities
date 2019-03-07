#' @title copy_getafix_sshfs
#'
#' @description Copy archivist entries from dogmatix/getafix mounted over sshfs
#' @param sshfsrepo Path the sshfs mounted archivist repo. Defaults to  "/home/phil/dogmatix/R_projects/cache/archivist_working/"
#' @param localrepo Path to local archivist repo. Defaults to "/vmshare/phd/projects/aus_bioregions/cache/archivist_working/"
#' @param md5hashes Array of archivist hashes to move to local repo.
#' @keywords archivist results getafix
#' @export 
#' @examples
#' # sshfs getafix ~/getafix
#' copy_getafix_sshfs(md5hashes = c("eae307e4a90cf7c95542bab0efb0fb24",
#'                                  "33bb343805360785e5160bba740be9e5"
#'                                 )


copy_getafix_sshfs <- function( sshfsrepo = "/home/phil/dogmatix/R_projects/cache/archivist_working/",
localrepo = "/vmshare/phd/projects/aus_bioregions/cache/archivist_working/",
md5hashes = c("eae307e4a90cf7c95542bab0efb0fb24",
              "33bb343805360785e5160bba740be9e5",
              "1eedbac6b2c10f809130ce69a6588b59",
              "0f42df0187adda39f8b79b2d11db18a4",
              "1449840cc47a1d2cd4d2720f56d48205",
              "439113eee5e6acd1627fc7970d6ae9d3",
              "b361716d77c5887f6ac53f96829053e6",
              "f560be26db0f1c9bec16d4bd041f3d09"
              )
){
  archivist::copyLocalRepo(repoFrom = sshfsrepo, repoTo = localrepo, md5hashes = md5hashes)
}
