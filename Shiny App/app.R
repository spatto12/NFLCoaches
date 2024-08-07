library(shiny)
library(shinydashboard)
library(rsconnect)
library(shinythemes)
library(tidyverse) # Data Cleaning, manipulation, summarization, plotting
library(nflfastR)
library(nflplotR)
library(scales)
library(ggrepel) # better labels
library(ggthemes) # custom pre-built themes
library(ggtext)
library(ggimage)
library(viridis)
library(igraph)
library(tibble)
library('extrafont')
library(visNetwork)
library(reactablefmtr)
library(ggpmisc)

ch_NFL <- read.csv("tree.csv")

from <- ch_NFL |>
  select(Head_Coach) |>
  group_by(Head_Coach) |>
  mutate(Occurrences = n()) |>
  ungroup() |>
  distinct() |>
  rename(label = Head_Coach)

to <- ch_NFL |>
  select(Coach) |>
  distinct() |>
  rename(label = Coach)

nodes0 <- full_join(from, to, by="label")

#Add coaches with zero NFL background or tree
#c(282, 283, 284, 285)
college <- data.frame(id = c(287, 288, 289, 290),
                      label = c('John Ralston', 'Mike Riley', 'Kliff Kingsbury', 'Urban Meyer'),
                      Occurrences = c(0, 0, 0, 0))

nodes0 <- nodes0 |>
  mutate(Occurrences = ifelse(is.na(Occurrences), 0, Occurrences)) |>
  rowid_to_column("id")

nodes <- nodes0 |>
  bind_rows(college)

rm(nodes0)

edges <- ch_NFL |> 
  left_join(nodes, by = c("Head_Coach" = "label")) |> 
  rename(from = id) |>
  left_join(nodes, by = c("Coach" = "label")) |> 
  rename(to = id) |>
  select(from, to, weight, group, lrole) 

nodes$name <- nodes$label
nodes$shape <- "dot"  
nodes$shadow <- FALSE # Nodes will drop shadow
nodes$size <- nodes$Occurrences + 1 # Node size
nodes$borderWidth <- 2 # Node border width

nodes$color.background <- "black"
nodes$color.border <- "black"
nodes$color.highlight.background <- "black"
nodes$color.highlight.border <- "black"
nodes$color.hover <- "black"

nodes$font.face <- "Times New ROman"

edges$width <- 1+edges$weight/8 # line width
edges$color <- ifelse(edges$group=="aC", "gold",
                      ifelse(edges$group=="aDC", "red",
                             ifelse(edges$group=="aHC", "goldenrod4",
                                    ifelse(edges$group=="aOC", "deepskyblue",
                                           ifelse(edges$group=="aSTC", "chartreuse2",
                                                  ifelse(edges$group=="DC", "darkred",
                                                         ifelse(edges$group=="Intern", "gold",
                                                                ifelse(edges$group=="OC", "midnightblue",
                                                                       ifelse(edges$group=="Scout", "gold",
                                                                              ifelse(edges$group=="STC", "darkgreen", NA))))))))))
edges$arrows <- "to" # arrows: 'from', 'to', or 'middle'
edges$smooth <- TRUE    # should the edges be curved?
edges$shadow <- FALSE    # edge shadow

ledges <- data.frame(color = c("midnightblue", "darkred", "darkgreen", 
                               "goldenrod", "deepskyblue", "red", 
                               "chartreuse", "gold"),
                     label = c("Offensive Coordinator", "Defensive Coordinator", "Special Teams Coordinator",
                               "Assistant Head Coach", "Offensive Assistant", "Defensive Assistant", 
                               "Special Teams Assistant", "Assistant"))

szn <- read.csv("wins.csv")

names <- szn |>
  pull(Coach)

career <- szn |>
  mutate(PD = PFg - PAg,
         Record = paste(tW, "-", tL, "-", tT),
         Pct = (tW + 0.5 * tT)/(tW + tL + tT),
         teams = ifelse(Coach=="Jeff Fisher", 2, teams)) |>
  select(Coach, Teams = teams, Years = yrs_hc, Games = tg_hc, Record, Pct, PF = PFg, PA = PAg, PD) |>
  distinct() 

games <- read.csv("games.csv")

names0 <- games |>
  pull(coach)

ui <- dashboardPage(
  # tags$style(HTML("
  #         .navbar .navbar-header {float: left; }
  #         .navbar .navbar-nav {float: left;}
  #         .container {min-width: 100%}
  #       ")
  # ),
  dashboardHeader(title = "NFL Head Coaches"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Tree", tabName = "Tree"),
      menuItem("Breakdown", tabName = "Breakdown"),
      menuItem("Comparison", tabName = "Comparison"),
      menuItem("Career", tabName = "Career")
    )
  ),
  
  dashboardBody(
    tabItems(#"by Patton Analytics",
      
      tabItem("Breakdown",
              fluidRow(
                column(4, align = "center",
                       selectInput('Coach', 'Head Coach:', c(sort(unique(names))), 
                                   selected = "Andy Reid"),
                ),
                column(4, align = "center",
                       radioButtons("win", label = "Wins",
                                    choices = list("Actual" = 1, "Pythagorean" = 2), 
                                    selected = 1, inline=TRUE)
                )#,
                # column(3, align = "center",
                #        downloadButton(outputId = 'download', label = 'Download',
                #             style = "padding: 5px 5px 5px 5px; margin: 25px 5px 10px 5px;"
                #             ))
              ),
              mainPanel(plotOutput(outputId = "coach_szn", width = "100%", height = "100%"), style = "width: 100%;")),
      
      tabItem("Comparison",
              fluidRow(
                column(4, align = "center",
                       selectInput('coach_1', 'Coach 1:', c(sort(unique(names0))),
                                   selected="Bill Belichick"),
                       selectizeInput('coach_2', 'Coach 2 (Optional):', c(sort(unique(names0))), 
                                      selected = "Don Shula", 
                                      multiple = TRUE, options = list(maxItems = 1)),
                       selectizeInput('coach_3', 'Coach 3 (Optional):', c(sort(unique(names0))), 
                                      selected = "Andy Reid", 
                                      multiple = TRUE, options = list(maxItems = 1)),
                )#,
                # column(4, align = "center",
                #        sliderInput("season", "Seasons Coached:",
                #                    value = c(1966, 2022), min = 1966, max = 2022, sep = ""))
                
                # column(4, align = "center",
                #        downloadButton(outputId = 'download0', label = 'Download',
                #                       style = "padding: 5px 5px 5px 5px; margin: 25px 5px 10px 5px;"))
              ),
              mainPanel(plotOutput(outputId = "comparison", width = "100%", height = "100%"), style = "width: 100%;")),
      
      
      tabItem("Tree",
              fluidRow(
                column(4, align = "center",
                       selectizeInput('name', 'Search for Head Coach:', choices = nodes$label,
                                      multiple = TRUE, options = list(maxItems = 1)),
                ),
                # column(4, align = "center",
                #        radioButtons("role", label = "Role",
                #                     choices = list("Main" = 1, "Last" = 2),
                #                     selected = 1, inline=TRUE))
              ),
              mainPanel(visNetworkOutput("coach_tree", width = "100%", height = "725px"), 
                        style = "height:775px; width: 100%; background-color: #eeeeee;")),
      
      tabItem("Career",
              fluidRow(
                column(4, align = "center",
                       sliderInput("game", "Games Coached:",
                                   min = 0, max = 450, value = 14))
              ),
              mainPanel(reactableOutput(outputId = "coach_career", width = "100%", height = "100%"), style = "width: 100%;")
      )
      
    ))
)

server <- function(input, output) { 
  
  #TREE
  output$coach_tree <- renderVisNetwork({
    
    visNetwork(nodes, edges,
               main = "The NFL Coaching Tree",
               submain = "Zoom in and Search for distinct Trees (1966 - 2024)",
               footer = list(text="node size is sum of coaches from, edge size is sum of positions held",
                             style = "text-align:left; font-size:9px")) |>
      visIgraphLayout(layout = "layout_with_fr") |>
      visOptions(highlightNearest = list(enabled = TRUE, degree = 1, labelOnly = TRUE, hover = TRUE)) |>
      visLegend(addEdges = ledges, useGroups = TRUE, position = "right",
                main = list(text = "Main Role"), zoom = FALSE)  |>
      visInteraction(zoomSpeed = 2)
  })
  
  observe({
    name <- input$name
    nodesid = which(nodes$label %in% name)
    visNetworkProxy("coach_tree") |> 
      visSelectNodes(id = nodesid)
  })
  
  # observe({
  #   name <- input$name
  #   nodesid = which(nodes$label %in% name)
  #   visNetworkProxy("coach_tree") |> 
  #     visFocus(id = nodesid)
  # })
  
  # observe({
  #   new_color <- ifelse(input$role==1, edges$color, edges$color2)
  #   visNetworkProxy("coach_tree") |>
  #     visEdges(edges, color = new_color)
  # 
  # })
  
  
  #COMPARISON  
  comparison_plot <- function(){
    
    coaches_selected <- c(input$coach_1, input$coach_2, input$coach_3)
    
    games1 <- games |>
      #filter(Season >= input$season[1] & Season <= input$season[2]) |>
      filter(coach %in% coaches_selected) |>
      arrange(Season, Week) |>
      group_by(coach) |>
      mutate(games = row_number(),
             par = games/2,
             cum_wins = cumsum(win),
             cum_aw = cumsum(awin),
             cum_ties = cumsum(tie),
             wins_over_par = cum_wins - par,
             record = paste0(cum_aw, " - ", abs(games - cum_aw), " - ", cum_ties, sep = "")
      ) |>
      ungroup() |>
      #Team Colors
      left_join(teams_colors_logos, by = c('team' = 'team_abbr')) |>
      #Coach ID
      arrange(coach, Season, team) |> group_by(tm_wt = cumsum(c(1, diff(as.numeric(team_id)) != 0)), coach, team) |>
      mutate(ns_coach = gsub( " ", "", coach),
             ls = last(Season),
             id = paste0(team, "_", ns_coach, "_", ls, sep = "")) |>
      ungroup()
    
    #Table
    coach_stats <- games1 |>
      select(Coach = coach, Games = games, Record = record, WOP = wins_over_par) |>
      group_by(Coach) |>
      filter(Games == last(Games)) |> 
      ungroup() |>
      arrange(-Games)
    
    #Table Position
    max_x0 <- max(games1$games)
    max_xy0 <- mean(games1$wins_over_par[games1$games==(max_x0)])
    
    max_y0 <- max(games1$wins_over_par)
    min_y0 <- min(games1$wins_over_par)
    max_yx0 <- max(games1$games[games1$wins_over_par==(max_y0)])
    
    max_x <- ifelse((max_x0/2 > max_yx0), round(max_x0 * 0.65), round(max_x0 * 0.05))
    mean_y1 <- mean(games1$wins_over_par[games1$games>(max_x)])
    max_y <- ifelse(((max_y0 + min_y0)/2 < mean_y1) & (max_x0/2 > max_yx0), min_y0 * 0.7, max_y0 * 0.7)
    max_y <- ifelse((max_xy0 < 0) & max_y0<5, max_xy0 * 0.15, max_y)
    
    #Plot
    games1 |>
      ggplot(aes(x = games, y = wins_over_par, group = coach)) +
      #Table
      ggplot2::annotate(geom = "table", x = max_x, y = max_y, label = list(coach_stats),
                        hjust = 0, vjust = 0, size = 5) +
      #Team Line
      geom_line(aes(color = team_color, group = id), alpha = 0.6, size = 2) +
      #Team Logo
      geom_nfl_logos(data = games1 %>% group_by(id) %>%
                       filter(games == last(games)) %>% ungroup(),
                     aes(team_abbr = team), width = 0.05, alpha = 0.9) +
      #Labels
      labs(x = "Games Coached",
           y = paste0("Wins +/- Par"),
           title = "Cumulative Wins Over .500",
           subtitle = "Regular season games only",
           #subtitle = paste0("Regular season games (", input$season[1], " - ",  input$season[2], ")"),
           caption = "Data: @pfref Plot: @PattonAnalytics") +
      scale_alpha_identity() +
      scale_color_identity() +
      theme_fivethirtyeight() +
      theme(legend.position='none') +
      theme(axis.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 14, face="bold")) +
      theme(plot.title = element_text(size = 18, face = "bold"),
            plot.caption = element_text(size = 8))+
      #make ticks look nice
      scale_x_continuous(breaks = scales::pretty_breaks(n = 8)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 8), labels = label_number(style_negative = "minus",
                                                                                      style_positive = "plus"))
    
    
    
  }
  
  output$comparison <- renderPlot({
    
    req(comparison_plot())
    comparison_plot()
    
  }, height = 600, width = 850)
  
  # #DOWNLOAD COMPARISON
  # output$download0 <- downloadHandler(
  #   filename = "coach_WAP.png",
  #   content = function(file) {
  #     comparison_plot()
  #     ggsave(file, plot = comparison_plot(), device = "png", width = 11, height = 8)
  #   })   
  
  
  #BREAKDOWN
  coach_plot <- function(){
    
    gc <- szn |>
      filter(Coach==input$Coach) |>
      left_join(teams_colors_logos, by = c('Tm' = 'team_abbr')) |>
      arrange(Season, Tm) |>
      mutate(tot_yrs = ly_hc - fy_hc + 1,
             max_win = ifelse(input$win==1, max(awins), max(pywins)),
             min_win = ifelse(input$win==1, min(awins), min(pywins)),
             ybreaks = round((max_win - min_win)/1.75)) |>
      group_by(Tm) |> 
      mutate(deltaLag1 = Season - lag(Season, 1)) |> 
      group_by(Tm, Season) |> 
      mutate(sequence1 = case_when(is.na(deltaLag1) | deltaLag1 > 1 ~ 1,
                                   TRUE ~ 2)) |> 
      ungroup() |> 
      mutate(sequence = cumsum(sequence1==1)) |> 
      select(-deltaLag1, -sequence1) |>
      arrange(Season, Tm) |> group_by(tm_wt = cumsum(c(1, diff(as.numeric(team_id)) != 0)), Tm) |>
      mutate(id = last(Season),
             tm_grp = paste0(Tm, "_", id, "_", sequence, sep = "")) |> ungroup() |>
      rowwise() |>
      mutate(y0 = ifelse(input$win==1, awins, pywins),
             y1 = ifelse(input$win==1, W_pct, PyW_pct),
             ylab = ifelse(input$win==1, "", "Pythagorean ")) |>
      ungroup() 
    
    lombardi <- "lombardi_trophy.png"
    cc <- "cc_trophy.png"
    
    dif <- max(gc$y0) - min(gc$y0)
    
    gc |>
      # mutate(alpha = ifelse(app==1, 1, 0.6)) |>
      ggplot(aes(x = Season, y = y0)) +
      geom_line(aes(y = mean(y1*17), color = team_color2, group=tm_grp), linetype="dashed", size=1.25) +
      geom_text(aes(label = 'HC Avg', x = ly_hc + 1, y = mean(y1*17), color = team_color2)) + 
      geom_line(aes(y = 8.5), color='red2', linetype="dashed", size=1.25) +
      geom_text(aes(label = 'NFL Avg', x = ly_hc + 1, y = 8.5, color = 'red2')) + 
      geom_line(aes(color = team_color, group=tm_grp), size=1.75) +
      # geom_point(data = ~filter(., sb == 1), colour = "grey", size = 18) +
      geom_point(data = ~filter(., app == 1), aes(color = team_color), size = 18) +
      geom_image(data = filter(gc, sba == 1), aes(y = y0 + (0.1 * dif), image = cc), size = 0.05) +
      geom_image(data = filter(gc, sb == 1), aes(y = y0 + (0.125 * dif), image = lombardi), size = 0.035) +
      geom_nfl_logos(aes(team_abbr = Tm), width = 0.04) +
      geom_text(aes(label=ifelse(coy==1 & sb==0 & sba==0,'COY',''), fontface="bold"), hjust=0.5, vjust=-2.5, size=5) +
      geom_text(aes(label=ifelse(coy==1 & sba==1,'COY',''), fontface="bold"), hjust=0.5, vjust=-5.5, size=5) +
      geom_text(aes(label=ifelse(coy==1 & sb==1,'COY',''), fontface="bold"), hjust=0.5, vjust=-7.25, size=5) +
      #titles and caption
      labs(x = "Season",
           y = paste0(gc$ylab, "Wins"),
           title = paste0(gc$name, " Regular Season Wins as a NFL Head Coach", sep = ""),
           subtitle = paste0("Scaled to 17 games, career accolades are displayed (", gc$fy_hc, " - ", gc$ly_hc, ")", sep=""),
           #Seasons are scaled to 17 games; super bowl victories highlighted
           caption = "Data: @pfref Plot: @PattonAnalytics") +
      scale_alpha_identity() +
      scale_color_identity() +
      theme_fivethirtyeight() +
      theme(axis.title = element_text(size = 16, face = "bold"),
            axis.text = element_text(size = 14, face="bold")) +
      theme(plot.title = element_text(size = 18, face = "bold"),
            plot.subtitle = element_text(size = 14),
            plot.caption = element_text(size = 8))+
      #make ticks look nice
      scale_y_continuous(breaks = scales::pretty_breaks(n = gc$ybreaks), expand = expansion(mult = c(.1, .1))) +
      scale_x_continuous(breaks = scales::pretty_breaks(n = gc$tot_yrs), guide = guide_axis(angle = 55))
    
    
  }
  
  output$coach_szn <- renderPlot({
    
    req(coach_plot())
    coach_plot()
    
  }, height = 600, width = 850) 
  
  # #DOWNLOAD BREAKDOWN
  #   output$download <- downloadHandler(
  #     filename = "coach.png",
  #     content = function(file) {
  #       coach_plot()
  #       ggsave(file, plot = coach_plot(), device = "png", width = 11, height = 8)
  #     })   
  
  #CAREER
  output$coach_career <-  renderReactable({
    
    career %>%
      filter(Games>=input$game) %>%
      reactable(.,
                theme = fivethirtyeight(),
                columnGroups = list(colGroup(name = "OVERALL", columns = c("Record","Pct")),
                                    colGroup(name = "PPG", columns = c("PF","PA", "PD"))
                ),
                defaultPageSize = 10,
                showSortIcon = FALSE,
                defaultSorted = "Games",
                defaultSortOrder = "desc",
                defaultColDef = colDef(align = "center"),
                columns = list(
                  Coach = colDef(maxWidth = 250),
                  Teams = colDef(maxWidth = 60),
                  Years = colDef(maxWidth = 60),
                  Games = colDef(maxWidth = 70),
                  Record = colDef(maxWidth = 100),
                  Pct = colDef(maxWidth = 70,
                               cell = function(x)
                                 sprintf("%0.3f", x)
                  ),
                  PF = colDef(maxWidth = 80,
                              cell = function(x)
                                sprintf("%0.2f", x),
                              style = color_scales(career, colors = c("#fd84a9", "white", "#42c2ca"))
                  ),
                  PA = colDef(maxWidth = 80,
                              cell = function(x)
                                sprintf("%0.2f", x),
                              style = color_scales(career, colors = c("#42c2ca", "white", "#fd84a9"))
                  ),
                  PD = colDef(maxWidth = 80,
                              cell = function(x)
                                sprintf("%+0.2f", x),
                              style = color_scales(career, colors = c("#fd84a9", "white", "#42c2ca"))
                  )
                ))
    
  })
  
}


shinyApp(ui = ui, server = server)