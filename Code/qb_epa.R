rm(list = ls(all.names = TRUE)) #will clear all objects includes hidden objects.
gc() #free up memrory and report the memory usage.
nflreadr::.clear_cache()

library(tidyverse) # Data Cleaning, manipulation, summarization, plotting
library(nflplotR) # Team Logos
library(gt) # gt tables
library(ggthemes) # custom pre-built themes
library(scales) # ggplot scaling
library(ggrepel) # better labels
library(ggtext) # ggplot text
library(ggimage) # ggplot images
library(viridis) #gt table colors
library(gtExtras) #gt extras
library(multicon) #multivariate constructs
library(performance) # check model
library(tidymodels) # run linear tests
library(ggpmisc) #ggplot equations and lines

#Set Working Directory & Load Data
setwd("~/NFL/QB EPA")
final <- read.csv("final.csv")

#Run Model Fit and Residual Plots
model <- linear_reg() |>
  parsnip::set_engine("lm") |>
  parsnip::fit(EPA~Exp + WR + OL + nHC + nTM + lagEPA, data=final)

performance::check_model(model)

#run linear regression model
PEPA_model <- lm(EPA~Exp + lagEPA + nHC + nTM + WR + OL, data = final)

#predict EPA per Drive
PEPA <- predict(PEPA_model, final, type = "response")
pred_EPA <- data.frame(final, PEPA) |>
  #Over Expectation
  dplyr::mutate(OE = EPA - PEPA) |>
  #Arrange & Rank
  dplyr::arrange(-OE) |>
  dplyr::mutate(Rank = paste0(row_number()))

#Plot Predicted and Actual EPA per Drive
pred_EPA |>
  ggplot2::ggplot(aes(x=PEPA, y=EPA)) +
  #Fit Line
  ggpmisc::stat_poly_line() +
  #R Squared & Adj R Squared
  ggpmisc::stat_poly_eq(aes(label = paste("atop(", after_stat(rr.label), ",", 
                                          after_stat(adj.rr.label), ")", sep = ""))) +
  #Pearson's R
  ggpmisc::stat_correlation(label.x = "right", label.y = "bottom") +
  #Points
  ggplot2::geom_point() + 
  #Plot Labels
  ggplot2::labs(x = "PEPA per Drive",
                y = "EPA per Drive",
                title = "Predicted and Actual EPA per Drive",
                subtitle = "Min. 250 regular season plays (2015-2021)",
                caption = "Data: @nflreadr, @nflfastR & @Jason_OTC Plot: @PattonAnalytics") +
  #538 Theme
  ggthemes::theme_fivethirtyeight() +
  #Axis Themes
  ggplot2::theme(axis.title = element_text(size = 10)) +
  ggplot2::theme(plot.title = element_text(size = 10, face = "bold"),
                 plot.subtitle = element_text(size = 8),
                 plot.caption = element_text(size = 8))+
  #Make Ticks Look Nice
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8), 
                              labels = label_number(accuracy = 0.01)) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8), 
                              labels = label_number(accuracy = 0.01))

#MVPs
mvp <- pred_EPA |>
  #Filter MVPs
  dplyr::filter(name=='C.Newton' & season==2015| name=='M.Ryan' & season==2016| 
                name=='T.Brady' & season==2017 | name=='P.Mahomes' & season==2018|
                name=='L.Jackson' & season==2019| name=='A.Rodgers' & season==2020| 
                name=='A.Rodgers' & season==2021) |>
  #Select Variables
  dplyr::select(Season = season, Name = name, Team = team, Drives, EPA, PEPA, OER = Rank) |>
  #OER from character to numeric
  dplyr::mutate(OER = as.numeric(OER)) |>
  #Arrange and Ungroup
  dplyr::arrange(Season) |> 
  dplyr::ungroup()

#MVP Table
mvp |>
  gt::gt() |>
  #title and subtitle
  gt::tab_header(title = md("**Most Valuable Players in the NFL since 2015**"),
             subtitle = "EPA, PEPA, and Over Expectation Rank (OER)") |>
  #column names
  gt::cols_label(
        Season = md("**Season**"),
        Name = md("**Name**"),
        Team = md("**Team**"),
        Drives = md("**Drives**"),
        EPA = md("**EPA**"),
        PEPA = md("**PEPA**"),
        OER = md("**OER**")
  ) |>
  #2 decimal places
  gt::fmt_number(columns = c(EPA, PEPA), decimals = 2) |>
  #bold columns
  gt::tab_style(style = cell_text(weight = "bold"),
                locations = cells_body(columns = c(Season, Name, Team, Drives, EPA, PEPA, OER))) |>
  #center column names
  gt::cols_align(align = "center", columns = c(Season, Name, Team, Drives, EPA, PEPA, OER)) |>
  #font title
  gt::tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"),
            locations = cells_title(groups = "title")) |>
  #font column names
  gt::tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
            locations = list(cells_column_labels(everything()))) |>
  #center columns
  gt::tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) |>
  #font body
  gt::tab_style(style = cell_text(font = c(google_font(name = "Rajdhani"),
                                           default_fonts())),
                locations = cells_body(columns = everything())) |>
  #team logos
  gt::text_transform(locations = cells_body(c(Team)),
                     fn = function(x) web_image(url = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", x, ".png"))) |>
  #column width
  gt::cols_width(c(Team) ~ px(45)) |>
  #borders
  gt::tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
                locations = list(cells_column_labels(columns = everything()))) |>
  #padding
  gt::tab_options(data_row.padding = px(0.5)) |>
  #column color
  gt::data_color(columns = c(OER),
                 colors = col_numeric(palette = viridis::viridis(20, direction = 1, option ="B"),
                                      domain = c(-25, 125)))

#Top 25 Season Performances in EPA over Expectation
pred_EPA |>
  #Name & Year
  dplyr::mutate(name_yr = paste0(last_name, " '", 
                          sprintf('%02d', season %% 100), sep = "")) |>
  dplyr::arrange(-OE) |>
  #Top 25
  dplyr::slice(1:25) |>
  ggplot2::ggplot(aes(x = PEPA, y = EPA)) +
  #Mean Horizontal and Vertical Lines
  nflplotR::geom_mean_lines(aes(v_var = PEPA , h_var = EPA)) +
  #NFL Logos
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.0275) +
  #Plot Text
  ggrepel::geom_text_repel(aes(label=name_yr), segment.color = 'grey80') +
  #titles and caption
  ggplot2::labs(x = "PEPA per Drive",
                y = "EPA per Drive",
                title = "Mahomes' ('18) and Jackson's ('19) MVP Campaigns took the League by Storm",
                subtitle = "Top 25 QBs in EPA over Expectation, min. 250 plays (2015-2021)",
                caption = "Data: @nflreadr, @nflfastR & @Jason_OTC Plot: @PattonAnalytics") +
  #538 Theme
  ggthemes::theme_fivethirtyeight() +
  #Axis Themes
  ggplot2::theme(axis.title = element_text(size = 10)) +
  ggplot2::theme(plot.title = element_text(size = 10, face = "bold"),
                 plot.subtitle = element_text(size = 8),
                 plot.caption = element_text(size = 8))+
  #make ticks look nice
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8), 
                              labels = label_number(accuracy = 0.01)) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8), 
                              labels = label_number(accuracy = 0.01))

#Bottom 25 Season Performances in EPA over Expectation
pred_EPA |>
  #Name & Year
  dplyr::mutate(name_yr = paste0(last_name, " '", 
                                 sprintf('%02d', season %% 100), sep = "")) |>
  dplyr::arrange(OE) |>
  #Bottom 25
  dplyr::slice(1:25) |>
  ggplot2::ggplot(aes(x = PEPA, y = EPA)) +
  #Mean Horizontal & Vertical Lines
  nflplotR::geom_mean_lines(aes(v_var = PEPA , h_var = EPA)) +
  #NFL Logos
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.0275) +
  #Plot Text
  ggrepel::geom_text_repel(aes(label=name_yr), segment.color = 'grey80') +
  #titles and caption
  ggplot2::labs(x = "PEPA per Drive",
                y = "EPA per Drive",
                title = "Goff's ('16) and Rosen's ('18) Rookie Years were Historically Bad",
                subtitle = "Bottom 25 QBs in EPA over Expectation, min. 250 plays (2015-2021)",
                caption = "Data: @nflreadr, @nflfastR & @Jason_OTC Plot: @PattonAnalytics") +
  #538 Theme
  ggthemes::theme_fivethirtyeight() +
  #Axis Themes
  ggplot2::theme(axis.title = element_text(size = 10)) +
  ggplot2::theme(plot.title = element_text(size = 10, face = "bold"),
                 plot.subtitle = element_text(size = 8),
                 plot.caption = element_text(size = 8))+
  #Make Ticks Look Nice
  ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(n = 8), 
                              labels = label_number(accuracy = 0.01)) +
  ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(n = 8), 
                              labels = label_number(accuracy = 0.01))

#2022 Predictions
qb22 <- read.csv("qb22.csv") |>
  dplyr::select(-c(nHC, nTM)) |>
  dplyr::arrange(-pepa) |>
  dplyr::mutate(rank = as.numeric(paste0(row_number())))

#Data Cleaning
qb1 <- qb22 |>
  dplyr::slice(1:16) |>
  dplyr::mutate(ID = row_number())

qb2 <- qb22 |>
  dplyr::slice(17:32) |>
  dplyr::rename(name2 = name, team2 = team, exp2 = exp, 
                lag_epa2 = lag_epa, wr2 = wr, ol2 = ol, 
                pepa2 = pepa, rank2 = rank) |>
  dplyr::mutate(ID = row_number())

#Merge qb1 & qb2
qb0 <- merge(qb1, qb2, by = c('ID')) %>%
  dplyr::select(-c(ID))

rm(qb1, qb2)

#2022 Predictions Table
qb0 |>
  gt::gt() |>
  #title and subtitle
  gt::tab_header(title = md("**Predicted EPA per Drive for Projected NFL Starters (2022)**"),
                 subtitle = "Years Experience (Exp), Prior Efficiency (lag EPA), Active Cap Hits in Millions") |>
  #arrange rows
  gt::cols_move_to_start(columns = c(rank, name, team, exp, lag_epa, wr, ol, pepa, rank2)) |>
  #column names
  gt::cols_label(
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
  ) |>
  #two decimal places
  gt::fmt_number(columns = c(lag_epa, pepa, lag_epa2, pepa2), decimals = 2) |>
  #bold columns
  gt::tab_style(style = cell_text(weight = "bold"),
                locations = cells_body(columns = c(rank, name, team, exp, lag_epa, wr, ol, pepa, 
                                                   rank2, name2, team2, exp2, lag_epa2, wr2, ol2, pepa2))) |>
  #center column names
  gt::cols_align(align = "center", columns = c(rank, name, team, exp, lag_epa, wr, ol, pepa, rank2,
                                               name2, team2, exp2, lag_epa2, wr2, ol2, pepa2)) |>
  #font title
  gt::tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size = "large"),
                locations = cells_title(groups = "title")) |>
  #font column names
  gt::tab_style(style = cell_text(font = c(google_font(name = "Karla"), default_fonts()), size='small'),
                locations = list(cells_column_labels(everything()))) |>
  #center columns
  gt::tab_style(style = cell_text(align = "center", size = "medium"), locations = cells_body()) |>
  #font body
  gt::tab_style(style = cell_text(font = c(google_font(name = "Rajdhani"),
                                           default_fonts())),
                locations = cells_body(columns = everything())) |>
  #team logos
  gt::text_transform(locations = cells_body(c(team, team2)),
                     fn = function(x) web_image(url = paste0("https://a.espncdn.com/i/teamlogos/nfl/500/", x, ".png"))) |>
  #column spacing
  gt::cols_width(c(team, team2) ~ px(45)) |>
  #border
  gt::tab_style(style = list(cell_borders(sides = "bottom", color = "black", weight = px(3))),
                locations = list(cells_column_labels(columns = everything()))) |>
  #row spacing
  gt::tab_options(data_row.padding = px(0.5)) |>
  #column color
  gt::data_color(columns = c(pepa, pepa2),
                 colors = col_numeric(palette = viridis::viridis(20, direction = -1, option ="D"),
                                      domain = c(-0.1, 1.5))) |>
  #border
  gt::tab_style(style = list(cell_borders(sides = "right", color = "black", weight = px(3))),
                locations = list(cells_body(columns = c(pepa, pepa2)))) |>
  #note
  gt::tab_source_note(source_note = md("**Table**: @PattonAnalytics | **Data**: @nflreadr, @nflfastR & @Jason_OTC")) 
