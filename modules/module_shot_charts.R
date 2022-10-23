# --------------------- #
# --- shot chart UI --- #
# --------------------- #

shot_chart_ui <- function(id){
  
  tagList(
    fluidPage(
      title = "",
      br(),
      br(),
      br(),
      strong(
        "2015-2021 Shot Charts by Streak",style = "font-size:30px;font-family:Georgia; color:black"
      ),
      br(),
      strong(
        "Version: 2022-10-23",style = "font-size:16px;font-family:Georgia; color:red"
      ),
      strong(
        "Created by Matt Bolaños | matthew.a.bolanos@gmail.com |", 
        a(
          "Spinner", href = "https://media.tenor.com/_u-gDFZQuIQAAAAM/basketball-sports.gif", 
          target = "_blank"
        ), 
        style = "font-size:14px;font-family:Georgia; color:black"
      ),
      br(),
      br(),
      fluidRow(
        column(3, selectizeInput(NS(id, "player_choice"), choices = NULL, label = "Player")),
        column(2, offset = 0, selectizeInput(NS(id, "year_choice"), choices = NULL, label = "Season(s)")),
        column(2, offset = 0, selectizeInput(NS(id, "streak_choice"), choices = NULL, label = "Streak")),
        column(3, offset = 0, selectizeInput(NS(id, "scope_choice"), choices = NULL, label = "Scope")),
        column(2, offset = 0, uiOutput(NS(id, "update_choice"), choices = NULL, label = "Update"))
      ),
      # Shot chart plot
      fluidRow(
        column(
          width = 5,
          plotOutput(NS(id, "streak_plot"), height = "460px") %>% 
            withSpinner(image = "loading.gif", image.width = "150px", image.height = "150px")
        ),
        column(
          width = 6,
          reactableOutput(NS(id, "streak_table"), height = "460px") %>% 
            withSpinner(image = "loading.gif", image.width = "150px", image.height = "150px")
        )
      )
    )
  )  
  
  
}

# ------------------------- #
# --- shot chart Server --- #
# ------------------------- #

shot_chart_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    # Button Selections ----------------------------------------------------------------------------------------------------------------
    
    # Player
    updateSelectizeInput(
      session = session,
      'player_choice',
      label = 'Player',
      selected = 'Klay Thompson',
      choices = shooter_values,
      server = T,
      options = list(placeholder = 'Type to Search')
    )
    
    # Season
    updateSelectizeInput(
      session = session,
      'year_choice',
      label = 'Season(s)',
      selected = '2015-2021',
      choices = c(
        "2015-2021", sort(season_values)
      ),
      options = list(onDelete = I('function(value) {return false;}'))
    )
    
    # Streak
    updateSelectizeInput(
      session = session,
      'streak_choice',
      label = 'Streak',
      selected = '3+ Makes',
      choices = c(
        "1 Make", "2 Makes", "3+ Makes",
        "1 Miss", "2 Misses", "3+ Misses"
      ),
      options = list(onDelete = I('function(value) {return false;}'))
    )
    
    # Scope
    updateSelectizeInput(
      session = session,
      'scope_choice',
      label = 'Scope',
      selected = 'Compared to League Average',
      choices = c(
        "Compared to League Average", "Compared to Player's Own Average"
      ),
      options = list(onDelete = I('function(value) {return false;}'))
    )
    
    # 
    # # Update btn
    output$update_choice <- renderUI({
      
      actionButton(
        session$ns("updatebtn"), 
        HTML("Update &#128293"), 
        style="color: white; background-color: #232323; font-weight:bold; margin-top:10%; font-size: 18px"
      )
      
    })
    
    output$streak_table <- renderReactable({
      
      # Load intital table
      shot_sum_table(
        league_averages %>% 
          filter(
            streak_col == "3+ Makes", slug_season == "2015-2021"
          ) %>% 
          select(
            name_zone, zone_range, base_pct = league_pct
          ), 
        klay_table, 
        type = "League Avg"
      )
      
    })
    
    output$streak_plot <- renderImage({
      
      # Load intial plot
      plot_shot_chart(
        klay_plot, type = "League Avg", "3+ Makes", "2015-2021", "Klay Thompson"
      )
      
      list(src = "shot_chart.png", contentType = "image/png", height = 460, width = 500)
      
    }, deleteFile = T)
    
    
    observeEvent(input$updatebtn,{
      
      # Set outputs to null after button push
      output$streak_plot <- renderImage({NULL}, deleteFile = T)
      output$streak_table <- renderReactable({NULL})
      
      # Isolate user inputs
      player_sel <- isolate(gsub("'","''",input$player_choice))
      year_sel <- isolate(input$year_choice)
      streak_sel <- isolate(input$streak_choice)
      scope_sel <- isolate(input$scope_choice)
      
      if (scope_sel == "Compared to League Average"){
        
        # ------- Compared to league average ------- #
        
        if (year_sel == "2015-2021"){
          
          # Don't need to filter for year
          input_query <- paste0(
            "
              SELECT
                location_x / 10 AS loc_x,
                ((location_y / 10) + 5.25) AS loc_y,
                CAST(is_shot_made AS int) AS shot_made_numeric,
                CAST(is_shot_attempted AS int) AS shot_attempted_flag,
                CASE WHEN type_shot = '3PT Field Goal' THEN 3 ELSE 2 END AS shot_value,
                zone_range,
                name_zone,
                distance_shot
              FROM
                streak_shots_nba
              WHERE
                shooter='", player_sel,"' AND
                streak_col = '", streak_sel, "'
              "
          )
          
          
        }else{
          
          # Filter for year
          input_query <- paste0(
            "
              SELECT
                location_x / 10 AS loc_x,
                ((location_y / 10) + 5.25) AS loc_y,
                CAST(is_shot_made AS int) AS shot_made_numeric,
                CAST(is_shot_attempted AS int) AS shot_attempted_flag,
                CASE WHEN type_shot = '3PT Field Goal' THEN 3 ELSE 2 END AS shot_value,
                zone_range,
                name_zone,
                distance_shot
              FROM
                streak_shots_nba
              WHERE
                shooter='", player_sel, "' AND
                slug_season ='", year_sel, "' AND
                streak_col = '", streak_sel, "'
              "
          )
          
        }
        
      }else{
        
        # ------- Compared to own average ------- #
        
        if (year_sel == "2015-2021"){
          
          # Don't need to filter for year
          input_query <- paste0(
            "
              SELECT
                location_x / 10 AS loc_x,
                ((location_y / 10) + 5.25) AS loc_y,
                CAST(is_shot_made AS int) AS shot_made_numeric,
                CAST(is_shot_attempted AS int) AS shot_attempted_flag,
                CASE WHEN type_shot = '3PT Field Goal' THEN 3 ELSE 2 END AS shot_value,
                zone_range,
                name_zone,
                streak_col,
                distance_shot
              FROM
                streak_shots_nba
              WHERE
                shooter='", player_sel,"'
              "
          )
          
          
        }else{
          
          # Filter for year
          input_query <- paste0(
            "
              SELECT
                location_x / 10 AS loc_x,
                ((location_y / 10) + 5.25) AS loc_y,
                CAST(is_shot_made AS int) AS shot_made_numeric,
                CAST(is_shot_attempted AS int) AS shot_attempted_flag,
                CASE WHEN type_shot = '3PT Field Goal' THEN 3 ELSE 2 END AS shot_value,
                zone_range,
                name_zone,
                streak_col,
                distance_shot
              FROM
                streak_shots_nba
              WHERE
                shooter='", player_sel, "' AND
                slug_season ='", year_sel, "'
              "
          )
          
        }
        
      }
      
      # Get player shots
      player_shots <- isolate(dbGetQuery(con, input_query))
      
      output$streak_table <- renderReactable({
        
        if (scope_sel == "Compared to League Average"){
          
          # ------- Compared to league average ------- #
          # Calculate player streak averages
          streak_pcts <- player_shots %>% 
            group_by(
              name_zone, zone_range
            ) %>% 
            summarise(
              streak_pct = sum(shot_made_numeric) / n(),
              streak_pps = sum(shot_made_numeric * shot_value) / n(),
              streak_fga = n(),
              .groups = "drop"
            )
          
          # Create table
          shot_sum_table(
            league_averages %>% 
              filter(
                streak_col == streak_sel, slug_season == year_sel
              ) %>% 
              select(
                name_zone, zone_range, base_pct = league_pct
              ), 
            streak_pcts, 
            type = scope_sel
          )
          
        }else{
          
          # ------- Compared to own average ------- #
          
          # Calculate player streak averages
          streak_pcts <- player_shots %>% 
            filter(
              streak_col == streak_sel
            ) %>% 
            group_by(
              name_zone, zone_range
            ) %>% 
            summarise(
              streak_pct = sum(shot_made_numeric) / n(),
              streak_pps = sum(shot_made_numeric * shot_value) / n(),
              streak_fga = n(),
              .groups = "drop"
            )
          
          # Base %s
          base_pcts <- player_shots %>% 
            group_by(
              name_zone, zone_range
            ) %>% 
            summarise(
              base_pct = sum(shot_made_numeric) / n(),
              .groups = "drop"
            )
          
          # Create table
          shot_sum_table(base_pcts, streak_pcts, type = "Own Avg")
          
        }
        
      })
      
      # --- Streak shot chart plot --- #
      output$streak_plot <- renderImage({
        

        # If there are shots with specified inputs
        if (nrow(player_shots) > 0){
          
          if (scope_sel == "Compared to League Average"){
            
            # ----- Compared to league avg ----- #
            
            # Get league shots
            league_zone_stats <- league_averages %>%
              filter(
                streak_col == streak_sel, slug_season == year_sel
              )
            
            # Get hex bin data
            hex_data <- get_hexbin_data(
              type = "League Avg",
              player_shots,
              league_zone_stats
            )
          
            plot_shot_chart(
              hex_data, type = "League Avg", streak_sel, year_sel, player_sel
            )
            
            list(src = "shot_chart.png", contentType = "image/png", height = 460, width = 500)
            
          }else{
            
            # ----- Compared to own avg ----- #
            
            # Filter for streak
            streak_shots <- player_shots %>%
              filter(
                streak_col == streak_sel
              )
            
            # Check if shots exist
            if (nrow(streak_shots) > 0){
              
              # Get hexbin data
              hex_data <- get_hexbin_data(
                type = "Own Avg",
                player_shots,
                streak_shots
              )
              
              plot_shot_chart(
                hex_data, type = "Own Avg", streak_sel, year_sel, player_sel
              )
              
              list(src = "shot_chart.png", contentType = "image/png", height = 460, width = 500)
              
            }else{
              
              # No shots? Empty plot
              plot_empty(
                streak_sel, year_sel, player_sel
              )
              
              list(src = "shot_chart.png", contentType = "image/png", height = 460, width = 500)
              
            }
            
          }
          
          
        }else{
          
          # No shots? Empty plot
          plot_empty(
            streak_sel, year_sel, player_sel
          )
          
          list(src = "shot_chart.png", contentType = "image/png", height = 460, width = 500)
          
        }
      }, deleteFile = T)
    })
  })
}


# 
# 
# 
# output$table2 <- DT::renderDataTable({
#   
#   vals = reactiveValues(
#     streak = ""
#   )
#   
#   vals$streak <- as.vector(input$streak2)
#   
#   streaky_sel <- vals$streak
#   
#   tab_query <- paste0("SELECT shooter, streak_col, type_shot,zone_basic,name_zone,
#      zone_range, distance_shot, is_shot_attempted, is_shot_made,
#      closest_defender_distance_bin FROM full_hhand_shots_tracking WHERE streak_col = '", streaky_sel, "'")
#   
#   all_shots <- dbGetQuery(con, tab_query)
#   
#   total_shots <- dbGetQuery(con, "SELECT shooter, closest_defender_distance_bin,
#                           is_shot_attempted FROM full_hhand_shots_tracking")
#   
#   total_fgas <- total_shots %>% 
#     mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
#                                         closest_defender_distance_bin == "Very Tight" ~ "Tight",
#                                         TRUE ~ closest_defender_distance_bin)) %>% 
#     group_by(shooter) %>% 
#     summarise(total_fga = sum(is_shot_attempted),
#               total_tight_attempts = sum(defender_distance =="Tight"),
#               percent_fga_tight = total_tight_attempts/total_fga) 
#   
#   
#   
#   averages <- all_shots %>% 
#     group_by(shooter,streak_col) %>% 
#     summarise(fgm = sum(is_shot_made),
#               fga = sum(is_shot_attempted),
#               threes = sum(type_shot %like% "3PT"),
#               base_streak_efg = (fgm+(.5*threes))/fga) %>% 
#     select(shooter, base_streak_efg)
#   
#   final_data <- all_shots %>% 
#     mutate(defender_distance= case_when(closest_defender_distance_bin == "Wide Open" ~ "Open",
#                                         closest_defender_distance_bin == "Very Tight" ~ "Tight",
#                                         TRUE ~ closest_defender_distance_bin)) %>% 
#     group_by(shooter, streak_col, defender_distance) %>%
#     
#     summarise(fgm = sum(is_shot_made),
#               fga = sum(is_shot_attempted),
#               fg_percent = fgm/fga,
#               threes = sum(type_shot %like% "3PT"),
#               efg_percent = (fgm+(.5*threes))/fga) %>% 
#     ungroup() %>% 
#     select(shooter,streak_col, defender_distance, efg_percent,fga) %>%
#     pivot_wider(names_from = c("defender_distance","streak_col"),
#                 values_from = c("efg_percent", "fga"),values_fill = 0, names_sep = "_") %>%
#     left_join(averages, by = c("shooter")) %>%
#     clean_names() %>%
#     left_join(total_fgas, by = c("shooter")) %>%
#     mutate(streak_fga=Reduce("+",.[4:5]),
#            coverage_diff = Reduce("-",.[3:2])
#     ) %>%
#     mutate_if(is.numeric, round,digits=4) %>% 
#     select(1,7,10,9,6,2,3,11) %>% 
#     arrange(desc(streak_fga))
#   
#   color_col <-final_data[6:6]
#   
#   (brks <- quantile(color_col, probs = seq(0, 1, .01), na.rm = TRUE)) 
#   (max_val <-max(color_col,na.rm=TRUE))
#   
#   (clrs_rmp <- colorRamp(c("lightskyblue","red"))(c(0,brks/max_val)))
#   
#   (clrs_df <- clrs_rmp %>% 
#       as_tibble(.name_repair ="minimal") %>% 
#       setNames(nm=c("r","g","b")) %>% 
#       mutate_all(~as.character(round(.,digits=0)))  %>% mutate(mycolor=paste0("rgb(",
#                                                                               paste(r,g,b,sep = ","),
#                                                                               ")")))
#   (clrs <- pull(clrs_df,mycolor))
#   
#   
#   
#   DT::datatable(final_data,filter = "top",rownames = FALSE,options = list(pagelength = 15, scrollX=TRUE,
#                                                                           columnDefs = list(list(className = 'dt-center', targets = 0:7))),
#                 colnames= c("Player", "Total FGA", 'Total Streak FGA',
#                             "%Total FGA Tight", "Streak eFG%", "Streak Open eFG%", 
#                             "Streak Tight eFG%", "Coverage Diff")) %>%
#     formatStyle(c(5,7), backgroundColor = styleInterval(brks, clrs))
# }
# )

#   })
# }

