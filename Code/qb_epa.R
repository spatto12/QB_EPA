

library(tidyverse) # Data Cleaning, manipulation, summarization, plotting
library(nflplotR)
library(gt)
library(ggthemes) # custom pre-built themes
library(scales)
library(ggrepel) # better labels
library(ggtext)
library(ggimage)
library(viridis)
library(gtExtras)
library(multicon)
library(performance)
library(tidymodels)
library(ggpmisc)

#load data
final <- read.csv("final.csv")

#run model fit and residual plots
model <- linear_reg() %>%
  set_engine("lm") %>%
  fit(EPA~Exp + WR + OL + nHC + nTM + lagEPA, data=final)

check_model(model)

#run linear regression model
PEPA_model <- lm(EPA~Exp + lagEPA + nHC + nTM + WR + OL, data = final)

#predict EPA per Drive
PEPA <- predict(PEPA_model, final, type = "response")
pred_EPA <- data.frame(final, PEPA) %>%
  mutate(OE = EPA - PEPA) %>%
  arrange(-OE) %>%
  mutate(Rank = paste0(row_number()))

#Plot Predicted and Actual EPA per Drive
pred_EPA %>%
  ggplot(aes(x=PEPA, y=EPA)) +
  #fit line
  stat_poly_line() +
  #r squared & adj r squared
  stat_poly_eq(aes(label = paste("atop(", after_stat(rr.label), ",", after_stat(adj.rr.label), ")", 
                                 sep = ""))) +
  #Pearson's R
  stat_correlation(label.x = "right", label.y = "bottom") +
  geom_point() + 
  labs(x = "PEPA per Drive",
       y = "EPA per Drive",
       title = "Predicted and Actual EPA per Drive",
       subtitle = "Min. 250 regular season plays (2015-2021)",
       caption = "Data: @nflreadr, @nflfastR & @Jason_OTC Plot: @PattonAnalytics") +
  scale_alpha_identity() +
  scale_color_identity() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 10)) +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8))+
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = 0.01)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = 0.01))

#Filter MVPs
mvp <- pred_EPA %>%
  filter(name=='C.Newton' & season==2015| name=='M.Ryan' & season==2016| 
           name=='T.Brady' & season==2017 | name=='P.Mahomes' & season==2018|
           name=='L.Jackson' & season==2019| name=='A.Rodgers' & season==2020| 
           name=='A.Rodgers' & season==2021) %>%
  select(Season = season, Name = name, Team = team, Drives, EPA, PEPA, OER = Rank) %>%
  mutate(OER = as.numeric(OER)) %>%
  as.data.frame(row.names = 1:nrow(.)) %>%
  arrange(Season) %>% ungroup()

#MVP Table
mvp %>%
  gt::gt() %>%
  tab_header(title = md("**Most Valuable Players in the NFL since 2015**"),
             subtitle = "EPA, PEPA, and Over Expectation Rank (OER)") %>%
  cols_label(
    Season = md("**Season**"),
    Name = md("**Name**"),
    Team = md("**Team**"),
    Drives = md("**Drives**"),
    EPA = md("**EPA**"),
    PEPA = md("**PEPA**"),
    OER = md("**OER**")
  ) %>%
  fmt_number(columns = c(EPA, PEPA), decimals = 2) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = c(Season, Name, Team, Drives, EPA, PEPA, OER))) %>%
  cols_align(align = "center", columns = c(Season, Name, Team, Drives, EPA, PEPA, OER)) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"),
            locations = cells_title(groups = "title")) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
            locations = list(cells_column_labels(everything()))) %>%
  tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Rajdhani"),
                                       default_fonts())),
            locations = cells_body(columns = everything())) %>%
  text_transform(locations = cells_body(c(Team)),
                 fn = function(x) web_image(url = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", x, ".png"))) %>%
  cols_width(c(Team) ~ px(45)) %>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) %>%
  tab_options(data_row.padding = px(0.5)) %>%
  data_color(columns = c(OER),
             colors = col_numeric(palette = viridis(20, direction = 1, option ="B"),
                                  domain = c(-25, 125)))

#Top 25 Season Performances in EPA over Expectation
pred_EPA %>%
  mutate(name_yr = paste0(last_name, " '", 
                          sprintf('%02d', season %% 100), sep = "")) %>%
  arrange(-OE) %>%
  slice(1:25) %>%
  ggplot(aes(x = PEPA, y = EPA)) +
  geom_mean_lines(aes(v_var = PEPA , h_var = EPA)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.0275) +
  geom_text_repel(aes(label=name_yr), segment.color = 'grey80') +
  #titles and caption
  labs(x = "PEPA per Drive",
       y = "EPA per Drive",
       title = "Mahomes' ('18) and Jackson's ('19) MVP Campaigns took the League by Storm",
       subtitle = "Top 25 QBs in EPA over Expectation, min. 250 plays (2015-2021)",
       caption = "Data: @nflreadr, @nflfastR & @Jason_OTC Plot: @PattonAnalytics") +
  scale_alpha_identity() +
  scale_color_identity() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 10)) +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8))+
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = 0.01)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = 0.01))

#Bottom 25 Season Performances in EPA over Expectation
pred_EPA %>%
  mutate(name_yr = paste0(last_name, " '", 
                          sprintf('%02d', season %% 100), sep = "")) %>%
  arrange(OE) %>%
  slice(1:25) %>%
  ggplot(aes(x = PEPA, y = EPA)) +
  geom_mean_lines(aes(v_var = PEPA , h_var = EPA)) +
  geom_nfl_logos(aes(team_abbr = team), width = 0.0275) +
  geom_text_repel(aes(label=name_yr), segment.color = 'grey80') +
  #titles and caption
  labs(x = "PEPA per Drive",
       y = "EPA per Drive",
       title = "Goff's ('16) and Rosen's ('18) Rookie Years were Historically Bad",
       subtitle = "Bottom 25 QBs in EPA over Expectation, min. 250 plays (2015-2021)",
       caption = "Data: @nflreadr, @nflfastR & @Jason_OTC Plot: @PattonAnalytics") +
  scale_alpha_identity() +
  scale_color_identity() +
  theme_fivethirtyeight() +
  theme(axis.title = element_text(size = 10)) +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        plot.caption = element_text(size = 8))+
  #make ticks look nice
  scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = 0.01)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(accuracy = 0.01))

#2022 Predictions
qb22 <- read.csv("qb22.csv") %>%
  select(-c(nHC, nTM)) %>%
  arrange(-pepa) %>%
  mutate(rank = as.numeric(paste0(row_number())))

#Data Cleaning
qb1 <- qb22 %>%
  slice(1:16) %>%
  mutate(ID = row_number())

qb2 <- qb22 %>%
  slice(17:32) %>%
  rename(name2 = name, team2 = team, exp2 = exp, 
         lag_epa2 = lag_epa, wr2 = wr, ol2 = ol, pepa2 = pepa, rank2 = rank) %>%
  mutate(ID = row_number())

qb0 <- merge(qb1, qb2, by = c('ID'))

rm(qb1, qb2)

qb0 <- qb0 %>%
  select(-c(ID))

#2022 Predictions Table
qb0 %>%
  gt::gt() %>%
  tab_header(title = md("**Predicted EPA per Drive for Projected NFL Starters (2022)**"),
             subtitle = "Years Experience (Exp), Prior Efficiency (lag EPA), Active Cap Hits in Millions") %>%
  cols_move_to_start(columns = c(rank, name, team, exp, lag_epa, wr, ol, pepa, rank2)) %>%
  cols_label(
    rank = md(""),
    name = md("**Name**"),
    team = md("**Team**"),
    exp = md("**Exp**"),
    lag_epa = md("**Lag EPA**"),
    wr = md("**WR**"),
    ol = md("**OL**"),
    pepa = md("**PEPA**"),
    rank2 = md(""),
    name2 = md("**Name**"),
    team2 = md("**Team**"),
    exp2 = md("**Exp**"),
    lag_epa2 = md("**Lag EPA**"),
    wr2 = md("**WR**"),
    ol2 = md("**OL**"),
    pepa2 = md("**PEPA**")
  ) %>%
  fmt_number(columns = c(lag_epa, pepa, lag_epa2, pepa2), decimals = 2) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = c(rank, name, team, exp, lag_epa, wr, ol, pepa, 
                                               rank2, name2, team2, exp2, lag_epa2, wr2, ol2, pepa2))) %>%
  cols_align(align = "center", columns = c(rank, name, team, exp, lag_epa, wr, ol, pepa, rank2,
                                           name2, team2, exp2, lag_epa2, wr2, ol2, pepa2)) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"),
            locations = cells_title(groups = "title")) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
            locations = list(cells_column_labels(everything()))) %>%
  tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) %>%
  tab_style(style = cell_text(font = c(google_font(name = "Rajdhani"),
                                       default_fonts())),
            locations = cells_body(columns = everything())) %>%
  text_transform(locations = cells_body(c(team, team2)),
                 fn = function(x) web_image(url = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", x, ".png"))) %>%
  cols_width(c(team, team2) ~ px(45)) %>%
  tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
            locations = list(cells_column_labels(columns = everything()))) %>%
  tab_options(data_row.padding = px(0.5)) %>%
  data_color(columns = c(pepa, pepa2),
             colors = col_numeric(palette = viridis(20, direction = -1, option ="D"),
                                  domain = c(-0.1, 1.5))) %>%
  tab_style(style = list(cell_borders(sides = "right", color = "black", weight = px(3))),
            locations = list(cells_body(columns = c(pepa, pepa2)))) %>%
  tab_source_note(source_note = md("**Table**: @PattonAnalytics | **Data**: @nflreadr, @nflfastR & @Jason_OTC")) 
