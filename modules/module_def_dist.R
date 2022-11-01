# ----------------------- #
# --- Def distance UI --- #
# ----------------------- #

distance_ui <- function(id){
  
  browsable(
    tagList(
      fluidPage(
        title = "",
        br(),
        br(),
        strong(
          "2014-2016 Streak Efficiency and Defender Distance",style = "font-size:30px;font-family:Georgia; color:black"
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
        fluidRow(
          column(2, offset = 0, selectizeInput(NS(id, "year_choice"), choices = NULL, label = "Season(s)")),
          column(2, offset = 0, selectizeInput(NS(id, "streak_choice"), choices = NULL, label = "Streak")),
          column(2, offset = 0, uiOutput(NS(id, "update_choice"), label = "Update"))
        ),
        # fluidRow(
        #   # slider min FGA
        #   column(
        #     5,
        #     offset = 0,
            # rangeFilter(
            #   "def-table-range",
            #   "streak_fga",
            #   "Minimum Streak FGA",
            #   1,
            #   380
            # )
        #   )
        # ),
        fluidRow(
          # slider min FGA
          column(
            5,
            offset = 0,
            uiOutput(NS(id, "slider"))
          )
        ),
        ## Output table
        fluidRow(
          column(
            width = 12,
            reactableOutput(NS(id, "def_table"), height = "460px") %>%
              withSpinner(image = "loading.gif", image.width = "150px", image.height = "150px")
          )
        )
      )
    )
  )
  
}

# -------------- #
# --- server --- #
# -------------- #

distance_server <- function(id){
  
  moduleServer(id, function(input, output, session){
    
    # Season
    updateSelectizeInput(
      session = session,
      'year_choice',
      label = 'Season(s)',
      selected = '2014-2016',
      choices = c(
        '2014-2016', '2014-2015', '2015-2016'
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
    
    # Update btn
    output$update_choice <- renderUI({
      
      actionButton(
        session$ns("updatebtn"), 
        HTML("Update &#128293"), 
        style="color: white; background-color: #232323; font-weight:bold; margin-top:10%; font-size: 18px"
      )
      
    })
    
    # Slider
    output$slider <- renderUI({
      
      rangeFilter(
        "def-table-range",
        "streak_fga",
        "Minimum Streak FGA",
        1,
        168
      )
      
    })
    
    
    # Start up table
    output$def_table <- renderReactable({
      
      start_up_def_table(dist_table)
      
    })
    
    observeEvent(input$updatebtn, {
      
      # Set outputs to NULL
      output$def_table <- NULL
      output$slider <- NULL
      
      # Save user inputs
      year_sel <- isolate(input$year_choice)
      streak_sel <- isolate(input$streak_choice)
      
      if (year_sel == "2014-2016") {
        
        # --- All years --- #
        input_query <- paste0(
          "
            SELECT
              shooter,
              CASE WHEN closest_defender_distance_bin IN ('Tight', 'Very Tight') THEN 'Tight' ELSE 'Open' END AS def_dist,
              is_shot_made,
              CASE WHEN type_shot IN ('3PT', '3PT Field Goal') THEN 3 ELSE 2 END AS shot_value
            FROM
              full_hhand_shots_tracking
            WHERE
              streak_col = '", streak_sel, "'
          "
        )
        
      }else {
        
        # --- By Year --- #
        # Set nba game id substring
        if (year_sel == "2014-2015"){
          
          yr_sub <- "214"
          
        }else{
          
          yr_sub <- "215"
          
        }
        
        input_query <- paste0(
          "
            SELECT
              shooter,
              CASE WHEN closest_defender_distance_bin IN ('Tight', 'Very Tight') THEN 'Tight' ELSE 'Open' END AS def_dist,
              is_shot_made,
              CASE WHEN type_shot IN ('3PT', '3PT Field Goal') THEN 3 ELSE 2 END AS shot_value
            FROM
              full_hhand_shots_tracking
            WHERE
              streak_col = '", streak_sel, "' AND
              SUBSTRING(CAST(game_id AS varchar) from 1 for 3) = '", yr_sub, "'
          "
        )
        
      }
      
      # Fetch shots
      shots <- dbGetQuery(con, input_query)
      
      # Quickly get max and min for slider
      shot_counts <- shots %>% 
        group_by(
          shooter
        ) %>% 
        summarise(
          streak_fga = n(),
          .groups = "drop"
        )
      
      # Slider
      output$slider <- renderUI({
        
        rangeFilter(
          "def-table-range",
          "streak_fga",
          "Minimum Streak FGA",
          min(shot_counts$streak_fga),
          max(shot_counts$streak_fga)
        )
        
      })
      
      # Create reactable
      output$def_table <- renderReactable({
        
        def_distance_table(shots)
        
      })
      
      
    })
    
  })
  
}