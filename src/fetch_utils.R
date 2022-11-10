get_parent_repo <- function(full_name){
  repo <- gh('GET /repos/{full_name}', full_name = full_name)
  return(repo$parent$full_name)
}
## get and parse commits
null_list <- function(x){
  map_chr(x, ~{ifelse(is.null(.x), NA, .x)})
}
## extract API response
parse_commit <- function(commits, repo){
  commit_by <- map(commits, c("commit", "author", "name"))
  username <- map(commits, c("committer", "login"))
  commit_time <- map(commits, c("commit", "author", "date"))
  message <- map(commits, c("commit", "message"))
  
  out <- tibble(
    repo = repo,
    commit_by = null_list(commit_by),
    username = null_list(username),
    commit_time = null_list(commit_time),
    message = null_list(message)
  )
  
  out <- mutate(out, commit_time = as.POSIXct(commit_time, format = "%Y-%m-%dT%H:%M:%SZ"))
  return(out)
}

gh_safe <- purrr::possibly(gh, otherwise = NULL)

munge_repos <- function(repos){
  repo_info <- tibble(
    owner = map_chr(repos, c("owner", "login")),
    name = map_chr(repos, "name"),
    full_name = map_chr(repos, "full_name"),
    fork = map_lgl(repos, "fork"),
    forks_count = map_dbl(repos, "forks_count"),
    size = map_dbl(repos, "size")#,
    #language = map_chr(repos, "language")
  ) %>%
    mutate(parent_repo = ifelse(!fork, full_name, NA))
  
  for (i in 1:length(repo_info$full_name)){
    if(is.na(repo_info[i, 'parent_repo'])){
      repo_info[i, 'parent_repo'] <- get_parent_repo(repo_info$full_name[i])
    }
  }
}