library(tidyverse)
library(gh)
library(lubridate)
library(glue)
library(usethis)

usethis::create_github_token()
gitcreds::gitcreds_set()
gh_token_help()

source('src/fetch_utils.R')

# fetch github repos for a user ----------------------------------------------------

user <- 'jordansread'

repos <- gh("GET /users/{username}/repos", 
            username = user, 
            .limit = Inf,
            sort = "created")
repos
repo_info <- munge_repos(repos)
repo_info

write_csv(repo_info, 'out/gh_repos.csv')

# get list of contributors for each repo
## contributors, language
repo$language

# look at user commits ----------------------------------------------------


## to get repo-specific info need to know the owner
gh('GET /repos/USGS-R/dataRetrieval/contributors') # contributions
gh('GET /repos/USGS-R/dataRetrieval/languages') # breakdown 
gh('GET /repos/USGS-R/dataRetrieval')
gh('GET /repos/jordansread/dataRetrieval')
# parent, full_name is the main repo that's forked


## fetch all commits by a user
all_commits <- map_dfr(repo_info$full_name, function(z){
  name_split <- str_split(z, "/")
  owner <- name_split[[1]][1]
  repo <- name_split[[1]][2]
  
  repo_commits <- gh_safe("/repos/:owner/:repo/commits", 
                          owner = owner, 
                          repo = repo, 
                          author = user,
                          since = "2011-01-01T00:00:00Z",
                          until = "2022-11-06T00:00:00Z",
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

my_commits$name %>% unique # repos committed to

# create github heatmap ---------------------------------------------------

gh_pal <- c(green = "#28a745",light_green = "#dcffe4")

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