# --------------------- #
# --- shot chart UI --- #
# --------------------- #

shot_chart_ui <- function(id){
  
  tagList(
    fluidPage(
      title = "",
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
        column(2, offset = 0, uiOutput(NS(id, "update_choice"), label = "Update"))
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

# -------------- #
# --- server --- #
# -------------- #

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
    
    # Update btn
    output$update_choice <- renderUI({
      
      actionButton(
        session$ns("updatebtn"), 
        HTML("Update &#128293"), 
        style="color: white; background-color: #232323; font-weight:bold; margin-top:10%; font-size: 18px"
      )
      
    })
    
    output$streak_table <- renderReactable({
      
      # Load initial table
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
    
    
    observeEvent(input$updatebtn, {
      
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
