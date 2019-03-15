context("test-track_state")

#libraries go here



test_that("System state is properly captured", {
  expect_match(track_system_state()[[1]], "R version")
  expect_match(track_system_state()[[1]], "Platform")
  expect_is(track_system_state()[[2]], "POSIXct")
})


test_that("package list is produced", {
  expect_equal(colnames(track_packages()), c("Package", "Version", "Built"))
  expect_equal(track_packages()[row.names(track_packages()) == "grDevices", 1], "grDevices")
})

context("Git")
local_clean <- "local_test_clean"
local_dirty <- "local_test_dirty"
local_untracked <- "local_test_untracked"
not_a_dir <- "not_a_dirr"
empty_dir <- "/vmshare/phd/test_empty"
dir.create(empty_dir)
dir.create(local_clean)
dir.create(local_dirty)
dir.create(local_untracked)

#clean repo
repo_clean <- git2r::init(local_clean)
git2r::config(repo_clean, user.name="Alice", user.email="alice@example.org")
writeLines("Hello world!", file.path(local_clean, "test.txt"))
git2r::add(repo_clean, "test.txt")
git2r::commit(repo_clean, "First commit message")
git2r::status(repo_clean)

#dirty repo
repo_dirty <- git2r::init(local_dirty)
git2r::config(repo_dirty, user.name="Alice", user.email="alice@example.org")
writeLines("Hello world!", file.path(local_dirty, "test.txt"))
git2r::add(repo_dirty, "test.txt")
git2r::commit(repo_dirty, "First commit message")
git2r::status(repo_dirty)
writeLines(c("Hello again!", "Here is a second line", "And a third"),
          file.path(local_dirty, "test.txt"))

#untracked files
repo_untracked <- git2r::init(local_untracked)
git2r::config(repo_untracked, user.name="Alice", user.email="alice@example.org")
writeLines("Hello world!", file.path(local_untracked, "test.txt"))
git2r::add(repo_untracked, "test.txt")
git2r::commit(repo_untracked, "First commit message")
git2r::status(repo_untracked)
writeLines(c("Hello again! New file!", "Here is a second line", "And a third"),
           file.path(local_untracked, "test2.txt"))


test_that("Git detects bad repo path",{
  expect_error(is_git_repo_clean(empty_dir, stop_if_false = TRUE), "Not a git repo")
  expect_error(is_git_repo_clean(not_a_dir, stop_if_false = TRUE), "Specified repo path does not exist")

})

test_that("Git handles clean/dirty states", {
  expect_error(is_git_repo_clean(local_dirty, stop_if_false = TRUE), "Git directory has uncommitted changes")
  expect_error(is_git_repo_clean(local_untracked, stop_if_false = TRUE), "Git directory has untracked changes")
  expect_equal(is_git_repo_clean(local_dirty, stop_if_false = FALSE), FALSE)
  expect_equal(is_git_repo_clean(local_untracked, stop_if_false = FALSE), FALSE)
  expect_equal(is_git_repo_clean(local_clean, stop_if_false = TRUE), TRUE)
})


test_that("Git returns status", {
  expect_error(track_git_status(empty_dir), "The 'path' is not in a git repository")
  expect_error(track_git_status(not_a_dir), "Specified repo path does not exist")
  expect_match(track_git_status(local_clean)[["repo"]], "HEAD")
  expect_match(track_git_status(local_clean)[["status"]], "working directory clean")
  expect_match(track_git_status(local_dirty)[["repo"]], "HEAD")
  expect_match(track_git_status(local_dirty)[["status"]], "Unstaged changes")
  expect_match(track_git_status(local_untracked)[["repo"]], "HEAD")
  expect_match(track_git_status(local_untracked)[["status"]], "Untracked files")
})

context("combined tracking")
test_that("track_all_stages contains all entries", {
  expect_error(track_all_states(c(local_untracked, local_dirty, local_clean)), "Git directory has untracked changes")
  expect_named(track_all_states(c(local_clean)), c("system", "packages", "git"))
  expect_named(track_all_states(), c("system", "packages"))
})

test_that("track_all_stages loads in package list", {
  #not actually a named data.frame or list
  expect_equal(colnames(track_all_states(c(local_clean))[["packages"]]), c("Package", "Version", "Built"))
  expect_equal(track_all_states(c(local_clean))[["packages"]][row.names(track_packages()) == "grDevices", 1], "grDevices")
})

test_that("track_all_stages loads in system state", {
  expect_match(track_all_states(c(local_clean))[["system"]][[1]], "R version")
  expect_match(track_all_states(c(local_clean))[["system"]][[1]], "Platform")
  expect_is(track_all_states(c(local_clean))[["system"]][[2]], "POSIXct")
})

test_that("track_all_stages handles git repos properly", {
  expect_error(track_all_states(git_repos = list("a"=5, b = "6")), "Invalid git repo")
  expect_length(track_all_states(c(local_clean))[["git"]], 1)
  expect_null(track_all_states()[["git"]])

})


unlink(local_clean, recursive = TRUE,force = TRUE)
unlink(local_dirty, recursive = TRUE, force = TRUE)
unlink(local_untracked, recursive = TRUE, force = TRUE)
unlink(empty_dir, recursive = TRUE)
