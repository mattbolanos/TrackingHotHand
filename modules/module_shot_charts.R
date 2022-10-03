# --------------------- #
# --- shot chart UI --- #
# --------------------- #

shot_chart_ui <- function(id){
  
  # Top text above panel
  fluidPage(
    # "Streak Shooting by Zone: 2015-2021",
    br(),
    br(),
    br(),
    strong(
      "2015-21 Shot Charts by Streak",style = "font-size:30px;font-family:Georgia; color:black"
    ),
    br(),
    strong(
      "Version: 2021-07-07",style = "font-size:16px;font-family:Georgia; color:red"
    ),
    strong(
      "Created by Matt Bolaños | matthew.a.bolanos@gmail.com |", 
      a(
        "Spinner", href = "https://i.pinimg.com/originals/27/3d/59/273d598e4856924672bf1a0ce62f0d54.gif", 
        target = "_blank"
      ), 
      style = "font-size:14px;font-family:Georgia; color:black"
    ),
    br(),
    strong(
      "**CHART AND TABLE WILL TAKE A FEW MOMENTS TO LOAD**",
      style = "font-size:24px;font-family:Georgia; color:red"
    ),
    br(),
    sidebarLayout(
      # Side bar
      sidebarPanel(
        # User input buttons
        uiOutput(NS(id, "player_choice")),
        uiOutput(NS(id, "year_choice")),
        uiOutput(NS(id, "streak_choice")),
        uiOutput(NS(id, "scope_choice")),
        uiOutput(NS(id, "update_choice")),
        width = 4
      ),
      # Main panel
      mainPanel(
        # Shot chart plot
        fluidRow(
          column(
            width = 6,
            plotOutput(NS(id, "streak_plot")) %>% 
              withSpinner(image = "loading.gif", image.width = "100px", image.height = "100px")
          ),
          column(
            width = 6,
            reactableOutput(NS(id, "streak_table"))
          )
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
    
    # Player btn
    output$player_choice <- renderUI({
      
      selectizeInput(
        session$ns('player'), 
        'Player', 
        choices = "Klay Thompson",
        selected = "Klay Thompson", 
        options = list(onDelete = I('function(value) {return false;}')),
        # options = pickerOptions(
        #   liveSearch = T, liveSearchStyle = 'contains', mobile = T
        # ),
        width = '95%'
      )
      
    })
    
    observeEvent(input$player, {
      
      updateSelectizeInput(
        session,
        inputId = "player",
        label = "Player",
        selected = "Klay Thompson",
        choices = shooter_values,
        server = TRUE
      )
      
    }, once = TRUE)
    
    
    # Year btn
    output$year_choice <- renderUI({
      
      selectizeInput(
        session$ns('year'), 
        'Season(s)', 
        choices = c(
          "2015-2021", sort(season_values)
        ),
        selected = '2016-17', 
        width = '95%', 
        
      )
      
    })
    
    # Streak btn
    output$streak_choice <- renderUI({
      
      selectizeInput(
        session$ns('streak'), 
        'Consecutive Makes/Misses',
        choices = c(
          "1 Make", "2 Makes", "3+ Makes", 
          "1 Miss", "2 Misses", "3+ Misses"
        ),
        selected = "3+ Makes", 
        multiple = FALSE,
        options = list(onDelete = I('function(value) {return false;}')),
        width = '95%'
      )
      
    })
    
    # Scope btn
    output$scope_choice <- renderUI({
      
      selectizeInput(
        session$ns('scope'), 
        "League Average/Against Self", 
        choices = c(
          "Compared to League Average", "Compared to Player's Own Average"
        ),
        selected = "Compared to League Average",
        width = '95%'
      )
      
    })
    
    # Update btn
    output$update_choice <- renderUI({
      
      actionBttn(
        session$ns('updatebtn'), 
        label = "Update",
        style = "pill",
        size = "sm",
        color = "warning"
      )
      
    })
    
    # --- Streak shot chart plot --- #
    output$streak_plot <- renderPlot({
      
      # Require go button
      req(input$updatebtn)
      
      ## User selections
      # Make player name SQL friendly
      player_sel <- isolate(gsub("'","''",input$player))
      year_sel <- isolate(input$year)
      streak_sel <- isolate(input$streak)
      
      # # Pull headshot url of selected player
      headshot <- isolate(
        players %>%
          filter(namePlayer == input$player) %>%
          pull(urlPlayerHeadshot)
      )
      
      # Download headshot file
      z <- isolate(tempfile())
      download.file(headshot,z,mode="wb")
      pic <- isolate(readPNG(z))
      file.remove(z)
      
      isolate({
  
        if(input$scope == "Compared to League Average"){
          
          # ------- Compared to league average ------- #
          
          if (input$year == "2015-2021"){
            
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
                name_zone
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
                name_zone
              FROM
                streak_shots_nba
              WHERE
                shooter='", player_sel, "' AND 
                slug_season ='", year_sel, "' AND 
                streak_col = '", streak_sel, "'
              "
            )
            
          }
          
          # Get player shots
          player_shots <- dbGetQuery(con, input_query)
          
          # If there are shots with specified inputs
          if (nrow(player_shots) > 0){
          
            # Get league shots
            league_zone_stats <- league_averages %>%
              filter(
                streak_col == streak_sel, slug_season == input$year
              )
            
            # Get hex bin data
            hex_data <- get_hexbin_data(
              type = "League Avg",
              player_shots,
              league_zone_stats
            )
            
            plot_shot_chart(
              hex_data, type = "League Avg", streak_sel, year_sel, player_sel, pic
            )
            
            
          }else{
            
            plot_empty(pic)
            
          }
          
        }else{
          
          # ------- Compared to own average ------- #
          
          if (input$year == "2015-2021"){
            
            # Get all player shots
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
                shooter='", player_sel, "'
              "
            )
            
          }else{
            
            # Get all player shots
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
                shooter='", player_sel, "'", " AND 
                slug_season ='", year_sel, "'
              "
            )
            
          }
          
          # Get player shots
          player_shots <- dbGetQuery(con, input_query)
          
          # Filter for streak
          streak_shots <- player_shots %>%
            filter(
              streak_col == streak_sel
            )
          
          # Check if shots exists
          if (nrow(streak_shots) > 0){
            
            # Get hexbin data
            hex_data <- get_hexbin_data(
              type = "Own Avg",
              player_shots,
              streak_shots
            )
            
            plot_shot_chart(
              hex_data, type = "Own Avg", streak_sel, year_sel, player_sel, pic
            )
            
          }else{
            plot_empty(pic)
          }
        }
        
        
      })

    })
    
    # --- Streak summary table --- #
    output$streak_table <- renderReactable({
      
      # Require go button
      req(input$updatebtn)
      
      ## User selections
      # Make player name SQL friendly
      player_sel <- isolate(gsub("'","''",input$player))
      year_sel <- isolate(input$year)
      streak_sel <- isolate(input$streak)
      
      if(input$scope == "Compared to League Average"){
        
        # ------- Compared to league average ------- #
        
        
      }else{
        
        # ------- Compared to own average ------- #
        if (input$year == "2015-21"){
          
          ## Don't need to filter for year
          # Base %s
          base_query <- paste0(
            "
            SELECT
              CAST(SUM(CASE WHEN is_shot_made THEN 1 ELSE 0 END) AS decimal(10,2)) / 
              CAST(SUM(CASE WHEN is_shot_attempted THEN 1 ELSE 0 END) AS decimal(10,2)) as base_pct,
              zone_range,
              name_zone
            FROM
              streak_shots_nba
            WHERE
              shooter='", player_sel, "'
            GROUP BY
              zone_range,
              name_zone
            "
          )
          
          # Streak %s
          streak_query <- paste0(
            "
            SELECT
              CAST(SUM(CASE WHEN is_shot_made THEN 1 ELSE 0 END) AS decimal(10,2)) / 
              CAST(SUM(CASE WHEN is_shot_attempted THEN 1 ELSE 0 END) AS decimal(10,2)) as streak_pct,
              zone_range,
              name_zone
            FROM
              streak_shots_nba
            WHERE
              shooter='", player_sel, "' AND
              streak_col = '", streak_sel, "'
            GROUP BY
              zone_range,
              name_zone
            "
          )
          
        }else{
          
          ## Filter for year
          # Base %s
          base_query <- paste0(
            "
            SELECT
              CAST(SUM(CASE WHEN is_shot_made THEN 1 ELSE 0 END) AS decimal(10,2)) / 
              CAST(SUM(CASE WHEN is_shot_attempted THEN 1 ELSE 0 END) AS decimal(10,2)) as base_pct,
              zone_range,
              name_zone
            FROM
              streak_shots_nba
            WHERE
              shooter='", player_sel, "' AND
              slug_season ='", year_sel, "'
            GROUP BY
              zone_range,
              name_zone
            "
          )
          
          # Streak %s
          streak_query <- paste0(
            "
            SELECT
              CAST(SUM(CASE WHEN is_shot_made THEN 1 ELSE 0 END) AS decimal(10,2)) / 
              CAST(SUM(CASE WHEN is_shot_attempted THEN 1 ELSE 0 END) AS decimal(10,2)) as streak_pct,
              zone_range,
              name_zone
            FROM
              streak_shots_nba
            WHERE
              shooter='", player_sel, "' AND
              slug_season ='", year_sel, "' AND
              streak_col = '", streak_sel, "'
            GROUP BY
              zone_range,
              name_zone
            "
          )
        }
        
        # Player base %s
        base_pcts <- dbGetQuery(con, base_query)
        
        # Player streak %s
        streak_pcts <- dbGetQuery(con, streak_query)
        
      }
      
    })
    
    
    # zone_sum <- zone_stats %>%
    #   select(
    #     zone_range, name_zone, zone_pct, zone_points_per_shot
    #   ) %>%
    #   left_join(
    #     league_zone_stats %>% select(name_zone, zone_range, league_pps, max_dist),
    #     by = c("zone_range", "name_zone")
    #   ) %>%
    #   mutate(
    #     across(
    #       .cols = c(zone_pct, zone_points_per_shot),
    #       ~ round(.x, 2)
    #     )
    #   ) %>%
    #   arrange(
    #     max_dist
    #   ) %>%
    #   select(
    #     -max_dist
    #   )
    # 
    # output$statTable <- render_gt({
    #   
    #   shot_sum_table(zone_sum, type = "League Avg", streak_sel)
    #   
    # })
    # ----- COMPARED TO PLAYER'S OWN AVERAGE
    
    
    #     
    #     
    #     
    #     zone_sum <- zone_stats %>%
    #       select(
    #         zone_range, name_zone, zone_pct, zone_points_per_shot
    #       ) %>%
    #       left_join(
    #         player_zone_stats, 
    #         by = c("zone_range", "name_zone")
    #       ) %>%
    #       mutate(
    #         fg_diff = zone_pct - league_pct,
    #         across(
    #           .cols = c(
    #             zone_pct, zone_points_per_shot, fg_diff
    #           ),
    #           ~ round(.x, 2)
    #         )
    #       ) %>% 
    #       arrange(
    #         max_dist
    #       ) %>% 
    #       select(
    #         zone_range, name_zone, streak_fg = zone_pct, zone_points_per_shot, fg_diff
    #       )
    #     
    #     output$statTable <- render_gt({
    #       
    #       shot_sum_table(zone_sum, type = "Own Avg", streak_sel)
    #       
    #     }, width = 200)
    #     
    #   }else{
    #     
    #     output$plot_streak <- renderPlot({
    #       
    #       # No shots? Plot empty court with headshot
    #       plot_empty(pic)
    #       
    #     }, height = 550, width = 800)
    #     
    #     output$statTable <- render_gt({
    #       
    #     })
    #     
    #   }
    # }
    # 
    # 
    # 
    # 
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
    
  })
}

