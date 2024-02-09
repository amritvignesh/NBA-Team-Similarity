library(nbastatR)
library(hoopR)
library(dplyr)
library(tidyverse)
library(factoextra)
library(cluster)
library(ggplot2)
library(gt)
library(gtExtras)
library(ggpath)
library(ggtext)

Sys.setenv("VROOM_CONNECTION_SIZE" = 131072 * 10)

lineups <- nba_leaguedashlineups(league_id = '00', season = year_to_season(most_recent_nba_season() - 1))$Lineups

lineups <- lineups %>% 
  group_by(team = TEAM_ABBREVIATION) %>%
  filter(row_number() == 1)

lineups$GROUP_ID <- gsub("^-|-$", "", lineups$GROUP_ID)

lineups <- separate(lineups, GROUP_ID, into = c("player1", "player2", "player3", "player4", "player5"), sep = "-") %>% select(player1, player2, player3, player4, player5, team)

lineups <- pivot_longer(lineups, cols = starts_with("player"), values_to = "id") %>% select(-name)

players <- nba_playerindex(league_id = '00', season = year_to_season(most_recent_nba_season() - 1))$PlayerIndex

players$HEIGHT <- sapply(strsplit(players$HEIGHT, "-"), function(x) as.numeric(x[1]) * 12 + as.numeric(x[2]))

players <- players %>%
  mutate(name = paste(PLAYER_FIRST_NAME, PLAYER_LAST_NAME)) %>%
  select(id = PERSON_ID, name, height = HEIGHT, weight = WEIGHT)

lineups <- left_join(lineups, players, by = "id")

stats <- bref_players_stats(2024)

stats$idPlayerNBA[which(stats$namePlayer == "Xavier Tillman Sr.")] <- 1630214

stats <- stats %>% mutate(mpg = round(minutesTotals/countGames, 3))

stats <- stats[,c(14, 23:32, 44:45, 47, 68)] %>% select(-pctTRB)

lineups$id <- as.numeric(lineups$id)
lineups <- left_join(lineups, stats, by = c("id"="idPlayerNBA"))
lineups$weight <- as.numeric(lineups$weight)
scaled <- scale(lineups[,c(4:18)])

pca <- prcomp(scaled)
pca_summary <- summary(pca)

twopcas <- as.data.frame(pca$x[,1:2])
variance_1 <- 100 *round(pca_summary$importance[2,1], 4) 
variance_2 <- 100 *round(pca_summary$importance[2,2], 4) 

plot <- twopcas %>%
  ggplot(aes(x=PC1, y=PC2)) + 
  geom_point(alpha=0.3) + 
  labs(x = paste0('PC1 (Accounts for ', variance_1, '% of Variance)'), 
       y = paste0('PC2 (Accounts for ', variance_2, '% of Variance)'), 
       title = 'PCA Plot for NBA Starting Players') +
  theme(plot.title = element_text(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"))

twopcas <- cbind(twopcas, lineups$team) %>% select(PC1, PC2, team = 'lineups$team')

proximity_vals <- data.frame()

unique_teams <- unique(twopcas$team)
for (t in unique_teams) {
  data <- twopcas %>% filter(team == t) %>% select(PC1, PC2)
  
  distances <- dist(data, method = 'euclidean')
  
  proximity <- sum(distances)
  cat("Team:", t, "Proximity:", proximity, "\n")
  
  proximity_vals <- rbind(proximity_vals, data.frame(team = t, proximity = proximity))
}

wide_lineups <- lineups %>% 
  select(team, id) %>%
  group_by(team) %>%
  mutate(player = paste0("player", row_number())) %>%
  pivot_wider(names_from = player, values_from = id)

wide_lineups <- left_join(wide_lineups, proximity_vals, by = "team") %>% ungroup()

wide_lineups$proximity <- round(scale(wide_lineups$proximity) * 100) * (-1)

p1 <- unique(wide_lineups$player1)
p2 <- unique(wide_lineups$player2)
p3 <- unique(wide_lineups$player3)
p4 <- unique(wide_lineups$player4)
p5 <- unique(wide_lineups$player5)

gt_nice_stats <- wide_lineups

for(p in p1) {
  gt_nice_stats$player1[which(wide_lineups$player1 == p)] <- nba_playerheadshot(p)
}
for(p in p2) {
  gt_nice_stats$player2[which(wide_lineups$player2 == p)] <- nba_playerheadshot(p)
}
for(p in p3) {
  gt_nice_stats$player3[which(wide_lineups$player3 == p)] <- nba_playerheadshot(p)
}
for(p in p4) {
  gt_nice_stats$player4[which(wide_lineups$player4 == p)] <- nba_playerheadshot(p)
}
for(p in p5) {
  gt_nice_stats$player5[which(wide_lineups$player5 == p)] <- nba_playerheadshot(p)
}

logos <- espn_nba_teams() %>%
  select(team = abbreviation, logo)

logos$team[which(logos$team == "GS")] <- "GSW"
logos$team[which(logos$team == "NO")] <- "NOP"
logos$team[which(logos$team == "NY")] <- "NYK"
logos$team[which(logos$team == "SA")] <- "SAS"
logos$team[which(logos$team == "UTAH")] <- "UTA"
logos$team[which(logos$team == "WSH")] <- "WAS"

gt_nice_stats <- left_join(gt_nice_stats, logos, by = "team") %>% select(-team) %>% select(logo, player1, player2, player3, player4, player5, proximity) %>% arrange(-proximity)

gt_align_caption <- function(left, right) {
  caption <- paste0(
    '<span style="float: left;">', left, '</span>',
    '<span style="float: right;">', right, '</span>'
  )
  return(caption)
}

caption = gt_align_caption("Data from <b>nbastatR</b> & <b>hoopR</b>", "Amrit Vignesh | <b>@avsportsanalyst</b>")

nice_table <- gt_nice_stats %>% gt() %>%
  gt_img_rows(columns = logo, height = 50) %>%
  gt_img_rows(columns = player1, height = 50) %>%
  gt_img_rows(columns = player2, height = 50) %>%
  gt_img_rows(columns = player3, height = 50) %>%
  gt_img_rows(columns = player4, height = 50) %>%
  gt_img_rows(columns = player5, height = 50) %>%
  gt_theme_538() %>%
  cols_align(
    align = "center",
    columns = c(logo, player1, player2, player3, player4, player5, proximity)
  ) %>%
  gt_hulk_col_numeric(proximity) %>%
  cols_label(
    logo = md("**TEAM**"),
    player1 = md(""),
    player2 = md(""),
    player3 = md("**PLAYERS**"),
    player4 = md(""),
    player5 = md(""),
    proximity = md("**SIMILARITY**")
  ) %>%
  tab_header(
    title = "NBA Teams Ranked By How Similar They Are in 2023/24",
    subtitle = md("*Based off **Most Used Lineup** - Similarity Metric Based on **Scaled Value Multiplied by 100***")
  ) %>%
  tab_source_note(html(caption)) %>%
  opt_align_table_header(align = "center") %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = cells_body(
      columns = proximity
    )
  )

gtsave(nice_table, "nice_table.png", vwidth = 1000, vheight = 2500, zoom = 1)

ids <- lineups %>% ungroup() %>% select(id)

twopcas <- cbind(twopcas, ids)

for(id in twopcas$id) {
  twopcas$headshot_link[which(twopcas$id == id)] <- nba_playerheadshot(id)
}

twopcas <- left_join(twopcas, logos, by = "team")

subfolder_path <- "plots/"
dir.create(subfolder_path, showWarnings = FALSE)

for (tm in unique_teams) {
  team_data <- plot_df %>% filter(team == tm)
  
  team_plot <- twopcas %>%
    ggplot(aes(x=PC1, y=PC2)) + 
    geom_point(data = ~filter(.x, team != tm), alpha=0.3) + 
    geom_from_path(data = ~filter(.x, team == tm), aes(x = PC1, y = PC2, path = headshot_link), width = 0.1, height = 0.1) +
    labs(x = paste0('PC1 (Accounts for ', variance_1, '% of Variance)'), 
         y = paste0('PC2 (Accounts for ', variance_2, '% of Variance)'),
         title = paste0("<img src='", twopcas$logo[which(twopcas$team == tm)], "' height='25' width='25' align='center'> <img src='https://www.pngall.com/wp-content/uploads/5/Combat-Versus-PNG-Clipart.png' height = '25' align = 'center'> <img src='https://cdn.nba.com/manage/2020/10/NBA20Secondary20Logo-784x462.jpg' height='25' align='center'>"),
         caption = "Data from **nbastatR** & **hoopR** | Amrit Vignesh | **@avsportsanalyst**") + 
    theme(plot.title = element_markdown(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"), plot.caption = element_markdown(hjust = 0.5))
  
  ggsave(file.path(subfolder_path, paste0(tm, ".png")), team_plot)
}

team_data <- plot_df %>% filter(team == "DEN")

team_plot <- twopcas %>%
  ggplot(aes(x=PC1, y=PC2)) + 
  geom_point(data = ~filter(.x, team != tm), alpha=0.3) + 
  geom_from_path(data = ~filter(.x, team == tm), aes(x = PC1, y = PC2, path = headshot_link), width = 0.1, height = 0.1) +
  labs(x = paste0('PC1 (Accounts for ', variance_1, '% of Variance)'), 
       y = paste0('PC2 (Accounts for ', variance_2, '% of Variance)'),
       title = paste0("<img src='", twopcas$logo[which(twopcas$team == tm)], "' height='25' width='25' align='center'> <img src='https://www.pngall.com/wp-content/uploads/5/Combat-Versus-PNG-Clipart.png' height = '25' align = 'center'> <img src='https://cdn.nba.com/manage/2020/10/NBA20Secondary20Logo-784x462.jpg' height='25' align='center'>")) +
  theme(plot.title = element_markdown(size=14, hjust=0.5, face="bold"), axis.title = element_text(face="bold"))

ggsave("test.png", team_plot)