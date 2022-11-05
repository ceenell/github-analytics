library(tidyverse)
library(gh)
library(lubridate)
library(glue)
library(usethis)

usethis::create_github_token()
gitcreds::gitcreds_set()
gh_token_help()


# fetch github commits ----------------------------------------------------

repos <- gh("GET /users/{username}/repos", 
            #.token = '',
            username = "ceenell", 
            .limit = Inf,
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
                          since = "2013-01-01T00:00:00Z",
                          until = "2015-11-05T00:00:00Z",
                          .limit = Inf, .progress = TRUE)
  out <- parse_commit(repo_commits, repo = z)
  
  return(out)
})

all_commits

my_commits <- all_commits %>% 
  mutate(commit_time = commit_time - hours(5)) %>% 
  mutate(
    date = date(commit_time),
    wday = wday(date, label = TRUE),
    year = year(date),
    month = month(date),
    week = week(date)
  ) %>% 
  left_join(
    repo_info, 
    by = c("repo" = "full_name")
  ) 
my_commits$wday
my_commits$username %>%unique
my_commits$name %>% unique # repos committed to
my_commits$owner %>% unique 

# create github heatmap ---------------------------------------------------

gh_pal <- c(blue = "#0366d6", yellow = "#ffd33d", red = "#d73a49", green = "#28a745", purple = "#6f42c1", light_green = "#dcffe4")

label_month <- my_commits %>% 
  group_by(month) %>%
  summarize(week = min(week)) %>%
  mutate(month_label = month(month, label = TRUE))

my_commits %>% 
  group_by(year, month, week, wday) %>% 
  summarize(n = length(message)) %>%
  mutate(label_month = ifelse(week %in% label_month$week, month, NA)) %>%
  ggplot() + 
  geom_tile(aes(x = week, y = wday, width = 0.9, height = 0.9, fill = n), 
            #fill = '#39d353'
            ) + 
  theme_classic() + 
  scale_y_discrete(expand = c(0, 0), 
                    limits = rev(c('Sun','Mon','Tue','Wed','Thu','Fri','Sat'))) + 
  scale_x_discrete(expand = c(0, 0), 
                   breaks = label_month$week,
                   labels = label_month$month_label) + 
  theme(
    axis.title = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    axis.text.y = element_blank(), 
    axis.text.x = element_text(color = 'red'),
    panel.grid.minor.x = element_blank(),
    legend.position = 'none',
    strip.background = element_rect(color = NA),
    #strip.text.y = element_text(angle = 0)
  ) +
  facet_grid(year~., switch = 'both') +
  coord_fixed(ratio = 1) + 
  scale_fill_gradient(low = gh_pal["light_green"], high = gh_pal["green"])


## year, week and day with most commits
my_commits %>% 
  group_by(year, month, week, wday) %>%
  summarize(n = length(message)) %>%
  arrange(desc(n))

## most repos 
my_commits %>% 
  group_by(year) %>%
  summarize(n_repos = length(unique(name))) %>%
  arrange(desc(n_repos))

## most popular repos
## number of PRs, issues, code changed through time
## network of collaborators
## commit messages and issues