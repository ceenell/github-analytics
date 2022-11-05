library(tidyverse)
library(gh)
library(lubridate)
library(glue)

repos <- gh("GET /users/{username}/repos", 
            .token = '',
            username = "ceenell", .limit = Inf,
            sort = "created")
repos

repo_info <- tibble(
  owner = map_chr(repos, c("owner", "login")),
  name = map_chr(repos, "name"),
  full_name = map_chr(repos, "full_name")
)
repo_info

length(unique(repo_info$full_name)) #165

## Get and parse commits
null_list <- function(x){
  map_chr(x, ~{ifelse(is.null(.x), NA, .x)})
}

## to extract API responses
parse_commit <- function(commits, repo){
  # browser()
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

all_commits <- map_dfr(repo_info$full_name, function(z){
  name_split <- str_split(z, "/")
  owner <- name_split[[1]][1]
  repo <- name_split[[1]][2]
  
  repo_commits <- gh_safe("/repos/:owner/:repo/commits", 
                          owner = owner, 
                          repo = repo, 
                          author = "ceenell",
                          since = "2012-01-01T00:00:00Z",
                          until = "2022-11-05T00:00:00Z",
                          .limit = Inf)
  repo_commits
  out <- parse_commit(repo_commits, repo = z)
  
  return(out)
})
all_commits

my_commits <- all_commits |> 
  mutate(commit_time = commit_time - hours(5)) |> 
  mutate(
    date = date(commit_time),
    wday = wday(date, label = TRUE),
    year = year(date),
    week = week(date)
  ) |> 
  left_join(
    repo_info, 
    by = c("repo" = "full_name")
  ) 
my_commits

## number of repos created, followed, contribted to, most popular repos
## number of commits, PRs, iissues, code changed through time
## network of collaborators
## common words used in commit messages, commit times
## nu
## most use functions? composition of languages?
