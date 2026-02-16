library(shiny)
library(bslib)
library(pool)
library(RPostgres)
library(ggplot2)
library(png)
library(grid)


# Connect to DB
pool <- dbPool(
  Postgres(),
  host     = Sys.getenv("SUPABASE_HOST"),
  port     = as.integer(Sys.getenv("SUPABASE_PORT")),
  dbname   = Sys.getenv("SUPABASE_DB"),
  user     = Sys.getenv("SUPABASE_USER"),
  password = Sys.getenv("SUPABASE_PASS"),
  sslmode  = "require"
)

onStop(function() {
  poolClose(pool)
})


# UI
ui <- navbarPage(
  "Intended Zones Application",
  id = "main_nav",
  
  # Set color themes
  theme = bs_theme(
    version = 5,
    primary = "#1e4d2b",      
    secondary = "#d4af37",
    bg = "#212529",
    fg = "#F8F9FA",
    base_font = font_google("Inter")
  ),
  
  # Home page
  tabPanel(
    "Home",
    value = "home",
    fluidRow(
      
      # Column if active session exists
      uiOutput("dynamic_active_column"),
      
      # Column to create new session
      column(
        width = 4,
        card(
          card_header("Create New Tracking Session", style = "background-color: #1e4d2b; color: white;"),
          card_body(
            style = "border: 2px solid white;",
            textInput(
              "new_session_name",
              "Session Name",
              placeholder = "e.g. Griffin Naess bullpen"
            ),
            actionButton(
              "submit_new_session",
              "Create New Session"
            )
          )
        )
      ),
      
      # Column if passive sessions in last 24 hrs exist
      uiOutput("dynamic_pause_column"),
    )
  ),
  
  # IZ Page
  tabPanel(
    "Strike Zone",
    value = "zone",
    h3(textOutput("zone_title"), class = "text-center"),
    fluidRow(
      # Button UI/UX
      column(
        width = 2,
        card(
          card_header("Select Pitch Type"),
          card_body(
            actionButton(
              "select_FF",
              "FF",
              class = "w-60"
            ),
            actionButton(
              "select_SI",
              "SI",
              class = "w-60"
            ),
            actionButton(
              "select_CT",
              "CT",
              class = "w-60"
            ),
            actionButton(
              "select_SL",
              "SL",
              class = "w-60"
            ),
            actionButton(
              "select_SW",
              "SW",
              class = "w-60"
            ),
            actionButton(
              "select_CB",
              "CB",
              class = "w-60"
            ),
            actionButton(
              "select_CH",
              "CH",
              class = "w-60"
            ),
            actionButton(
              "select_SP",
              "SP",
              class = "w-60"
            ),
            fluidRow(
              column(
                6,
                actionButton(
                  "pause_active_session2",
                  "Pause"
                )
              ),
              column(
                6,
                actionButton(
                  "exit",
                  "Exit",
                  style = "border-color: red;"
                )
              )
            )
          )
        )
      ),
      column(
        width = 10,
        card(
          card_body(
            plotOutput("strike_zone", click = "zone_click", height = 800)
          )
        )
      )
    )
  )
  
  
)


server <- function(input, output, session) {
  
  ##########################
  ### Session Management ###
  ##########################
  
  active_session <- reactiveVal(NULL)
  paused_sessions <- reactiveVal(NULL)
  selected_resume_id <- reactiveVal(NULL)
  
  refresh_active_session <- function() {
    df <- dbGetQuery(pool, "
    SELECT *
    FROM iz_sessions
    WHERE is_active = TRUE
    ORDER BY created_at DESC NULLS LAST
    LIMIT 1
  ")
    if (nrow(df) == 0) active_session(NULL) else active_session(df[1, , drop = FALSE])
  }
  
  refresh_active_session()
  
  refresh_pause_session <- function() {
    df <- dbGetQuery(pool, "
    SELECT *
    FROM iz_sessions
    WHERE is_active = FALSE AND created_at > NOW() - INTERVAL '1 day'
    ORDER BY created_at DESC
  ")
    paused_sessions(df)
  }
  
  refresh_pause_session()
  
  output$dynamic_active_column <- renderUI({
    s <- active_session()
    if (is.null(s)) return(NULL)
    
    column(
      width = 4,
      card(
        card_header("Active Session", style = "background-color: #1e4d2b; color: white;"),
        card_body(
          style = "border: 2px solid white;",
          p(s$name),
          layout_column_wrap(
            width = 1/2,
            actionButton(
              "resume_active_session", 
              "Resume",
              style = "border-color: green",
              width = "110%"
            ),
            actionButton(
              "pause_active_session", 
              "Pause",
              style = "border-color: #d4af37",
              width = "90%"
            )
          )
        )
      )
    )
  })
  
  output$dynamic_pause_column <- renderUI({
    p <- paused_sessions()
    req(!is.null(p), nrow(p) > 0)
    
    column(
      width = 4,
      card(
        card_header("Resume Previous Session", style = "background-color: #1e4d2b; color: white;"),
        card_body(
          style = "border: 2px solid white;",
          tagList(
            lapply(seq_len(nrow(p)), function(i) {
              row <- p[i, , drop = FALSE]
              id_chr <- as.character(row$id)
              
              actionButton(
                inputId = paste0("resume_", id_chr),
                label   = paste("Resume:", row$name)
              )
            })
          )
        )
      )
    )
  })
  
  observeEvent(input$main_nav, {
    if (identical(input$main_nav, "home")) {
      session$onFlushed(function() {
        refresh_active_session()
        refresh_pause_session()
      }, once = TRUE)
    }
  }, ignoreInit = TRUE)
  
  
  observeEvent(input$resume_active_session, {
    updateNavbarPage(session, "main_nav", selected = "zone")
  })
  
  observeEvent(input$exit, {
    updateNavbarPage(session, "main_nav", selected = "home")
  })
  
  observeEvent(input$pause_active_session2, {
    poolWithTransaction(pool, function(db) {
      dbExecute(db, "UPDATE iz_sessions SET is_active = FALSE WHERE is_active = TRUE")
    })
    updateNavbarPage(session, "main_nav", selected = "home")
    refresh_active_session()
    refresh_pause_session()
  })
  
  observeEvent(input$pause_active_session, {
    poolWithTransaction(pool, function(db) {
      dbExecute(db, "UPDATE iz_sessions SET is_active = FALSE WHERE is_active = TRUE")
    })
    refresh_active_session()
    refresh_pause_session()
  })
  
  observe({
    p <- paused_sessions()
    req(!is.null(p), nrow(p) > 0)
    
    lapply(seq_len(nrow(p)), function(i) {
      row <- p[i, , drop = FALSE]
      id_chr <- as.character(row$id)
      btn_id <- paste0("resume_", id_chr)
      
      observeEvent(input[[btn_id]], {
        selected_resume_id(row$id)  # store it
        
        poolWithTransaction(pool, function(db) {
          dbExecute(db, "UPDATE iz_sessions SET is_active = FALSE WHERE is_active = TRUE")
          dbExecute(db, "UPDATE iz_sessions SET is_active = TRUE WHERE id = $1",
                    params = list(row$id))
        })
        
        updateNavbarPage(session, "main_nav", selected = "zone")
      }, ignoreInit = TRUE)
      
      
      refresh_active_session()
      refresh_pause_session()
    })
  })
  
  
  observeEvent(input$submit_new_session, {
    req(input$new_session_name)
    
    poolWithTransaction(pool, function(db) {
      dbExecute(db, "UPDATE iz_sessions SET is_active = FALSE WHERE is_active = TRUE")
      dbExecute(
        db,
        "INSERT INTO iz_sessions (name, is_active) VALUES ($1, TRUE)",
        params = list(input$new_session_name)
      )
    })
    
    refresh_active_session()
    refresh_pause_session()
    updateNavbarPage(session, "main_nav", selected = "zone")
  })
  
  

  
  #################
  ## Strike Zone ##
  #################
  
  segments_df <- within(data.frame(
    x    = c(-0.85, -0.85, -0.85,  0.85, -0.283,  0.283, -0.85, -0.85,  0.00,  0.00, -1.092, -1.092, -1.092,  1.092,  0.85,  -1.092),
    xend = c( 0.85,  0.85, -0.85,  0.85, -0.283,  0.283,  0.85,  0.85,  0.00,  0.00,  1.092,  1.092, -1.092,  1.092,  1.092, -0.85 ),
    y    = c( 1.60,  3.50,  1.60,  1.60,  1.60,  1.60,  2.233, 2.867, 3.7417, 1.60, 1.358, 3.7417, 1.358, 1.358, 2.55, 2.55),
    yend = c( 1.60,  3.50,  3.50,  3.50,  3.50,  3.50,  2.233, 2.867, 3.50,   1.358,1.358, 3.7417, 3.7417, 3.7417,2.55, 2.55)
  ), {kind <- "grid"})
  
  glove_width <- 0.8
  glove_height <- 0.8
  
  ball_width <- 1.25
  ball_height <- 1.25
  
  glove_img <- readPNG("www/glove.png")
  
  cx <- reactiveVal(0)
  cy <- reactiveVal(2.55)
  ball_img <- reactiveVal(NULL)
  
  calc_bounds <- function(cx, cy, img_width, img_height){
    xmin <- cx - img_width/2
    xmax <- cx + img_width/2
    ymin <- cy - img_height/2
    ymax <- cy + img_height/2
    return(c(xmin, xmax, ymin, ymax))
  }
  
  base_zone_plot <- ggplot() +
    geom_segment(
      data = segments_df,
      aes(x = x, y = y, xend = xend, yend = yend),
      linewidth = 1.2,
      color = "#d4af37",
      lineend = "round"
    ) +
    coord_fixed(xlim = c(-1.4, 1.4), ylim = c(1.0, 4.1), expand = FALSE) +
    theme_void() +
    theme(
      plot.background  = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA)
    )
  
  output$strike_zone <- renderPlot({
    img_grob_glove <- rasterGrob(glove_img, interpolate = TRUE)
    glove_bounds <- calc_bounds(cx(), cy(), glove_width, glove_height)
    
    p <- base_zone_plot +
      annotation_custom(img_grob_glove,
                        xmin=glove_bounds[1], xmax=glove_bounds[2],
                        ymin=glove_bounds[3], ymax=glove_bounds[4]
      )
    
    if (!is.null(ball_img())) {
      img_grob_ball <- rasterGrob(ball_img(), interpolate = TRUE)
      ball_bounds <- calc_bounds(cx(), cy(), ball_width, ball_height)
      
      p <- p + annotation_custom(img_grob_ball,
                                 xmin=ball_bounds[1], xmax=ball_bounds[2],
                                 ymin=ball_bounds[3], ymax=ball_bounds[4]
      )
    }
    
    p
  }, bg = "transparent")
  
  
  observeEvent(input$zone_click, {
    cx(input$zone_click$x)
    cy(input$zone_click$y)
  })
  
  observeEvent(input$select_FF, {
    ball_img(readPNG("www/FF.png"))
  })
  
  observeEvent(input$select_SI, {
    ball_img(readPNG("www/SI.png"))
  })
  
  observeEvent(input$select_CT, {
    ball_img(readPNG("www/CT.png"))
  })
  
  observeEvent(input$select_SL, {
    ball_img(readPNG("www/SL.png"))
  })
  
  observeEvent(input$select_SW, {
    ball_img(readPNG("www/SW.png"))
  })
  
  observeEvent(input$select_CB, {
    ball_img(readPNG("www/CB.png"))
  })
  
  observeEvent(input$select_CH, {
    ball_img(readPNG("www/CH.png"))
  })
  
  observeEvent(input$select_SP, {
    ball_img(readPNG("www/SP.png"))
  })
}
  

shinyApp(ui, server)
