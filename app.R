library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(DT)
library(glue)

con <- dbConnect(
  RPostgres::Postgres(),
  dbname = Sys.getenv("PGDATABASE", "postgres"),
  host = Sys.getenv("PGHOST", "localhost"),
  port = as.integer(Sys.getenv("PGPORT", "5432")),
  user = Sys.getenv("PGUSER", "postgres"),
  password = Sys.getenv("PGPASSWORD", "postgres")
)

onStop(function() {
  if (DBI::dbIsValid(con)) DBI::dbDisconnect(con)
})

run_query <- function(sql) {
  DBI::dbGetQuery(con, sql)
}

ui <- fluidPage(
  titlePanel("College Baseball TrackMan Dashboard"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("dates", "Date range", start = Sys.Date() - 30, end = Sys.Date()),
      numericInput("velo", "High velo threshold", value = 92, min = 80, max = 105),
      selectInput("pitcher", "Pitcher", choices = c()),
      actionButton("refresh", "Refresh")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Pitch Shapes", DTOutput("pitch_shapes")),
        tabPanel("Command Watch", DTOutput("command_watch")),
        tabPanel("Velo Up Zone", DTOutput("velo_up")),
        tabPanel("EV Monthly", DTOutput("ev_monthly")),
        tabPanel("EV 14d Risers", DTOutput("ev_risers")),
        tabPanel("Pitcher-Catcher", DTOutput("pitcher_catcher")),
        tabPanel("Matchups", DTOutput("matchups"))
      )
    )
  )
)

server <- function(input, output, session) {
  pitchers <- reactive({
    run_query("SELECT DISTINCT pitcher_id, pitcher_name FROM v_pitch_events ORDER BY pitcher_name")
  })

  observe({
    p <- pitchers()
    updateSelectInput(session, "pitcher", choices = setNames(p$pitcher_id, p$pitcher_name), selected = p$pitcher_id[1])
  })

  date_clause <- reactive({
    glue("game_date BETWEEN '{input$dates[1]}' AND '{input$dates[2]}'")
  })

  pitch_shapes_data <- eventReactive(input$refresh, {
    sql <- glue("WITH shaped AS (
      SELECT pitcher_id, pitcher_name, pitch_type,
             width_bucket(induced_vert_break, -25, 25, 10) AS ivb_bin,
             width_bucket(horz_break, -25, 25, 10) AS hb_bin,
             width_bucket(rel_speed, 60, 100, 8) AS velo_bin,
             is_swing, is_whiff, is_ooz, is_called_strike
      FROM v_pitch_events
      WHERE {date_clause()} AND pitch_type IS NOT NULL
    )
    SELECT pitcher_name, pitch_type, ivb_bin, hb_bin, velo_bin,
           COUNT(*) AS pitches,
           ROUND(SUM(is_swing)::numeric / NULLIF(COUNT(*),0), 3) AS swing_pct,
           ROUND(SUM(is_whiff)::numeric / NULLIF(SUM(is_swing),0), 3) AS whiff_pct,
           ROUND(SUM(CASE WHEN is_swing=1 AND is_ooz=1 THEN 1 ELSE 0 END)::numeric / NULLIF(SUM(is_ooz),0), 3) AS chase_pct,
           ROUND((SUM(is_called_strike)+SUM(is_whiff))::numeric / NULLIF(COUNT(*),0), 3) AS csw_pct
    FROM shaped GROUP BY 1,2,3,4,5 HAVING COUNT(*) >= 20
    ORDER BY whiff_pct DESC NULLS LAST")
    run_query(sql)
  }, ignoreInit = FALSE)

  command_watch_data <- eventReactive(input$refresh, {
    sql <- glue("WITH season AS (
      SELECT pitcher_id, pitcher_name,
             AVG((1-is_ooz)::numeric) AS zone_pct_season,
             AVG((CASE WHEN pitch_call LIKE 'Ball%' THEN 1 ELSE 0 END)::numeric) AS ball_pct_season
      FROM v_pitch_events
      WHERE game_date >= date_trunc('year', current_date)
      GROUP BY 1,2
    ),
    last14 AS (
      SELECT pitcher_id, pitcher_name,
             AVG((1-is_ooz)::numeric) AS zone_pct_14d,
             AVG((CASE WHEN pitch_call LIKE 'Ball%' THEN 1 ELSE 0 END)::numeric) AS ball_pct_14d,
             COUNT(*) AS pitches_14d
      FROM v_pitch_events
      WHERE game_date >= current_date - interval '14 days'
      GROUP BY 1,2
    )
    SELECT l.pitcher_name, l.pitches_14d,
           ROUND(l.zone_pct_14d,3) AS zone_pct_14d,
           ROUND(s.zone_pct_season,3) AS zone_pct_season,
           ROUND(l.zone_pct_14d - s.zone_pct_season,3) AS zone_delta,
           ROUND(l.ball_pct_14d - s.ball_pct_season,3) AS ball_delta,
           CASE WHEN l.pitches_14d < 50 THEN 'LOW_SAMPLE'
                WHEN (l.zone_pct_14d - s.zone_pct_season) <= -0.05
                 AND (l.ball_pct_14d - s.ball_pct_season) >= 0.04 THEN 'ALERT'
                ELSE 'OK' END AS status
    FROM last14 l JOIN season s USING (pitcher_id, pitcher_name)
    ORDER BY status DESC, zone_delta ASC")
    run_query(sql)
  }, ignoreInit = FALSE)

  velo_up_data <- eventReactive(input$refresh, {
    sql <- glue("SELECT batter_name, COUNT(*) AS pitches_seen,
           ROUND(SUM(is_swing)::numeric / NULLIF(COUNT(*),0),3) AS swing_pct,
           ROUND(SUM(is_whiff)::numeric / NULLIF(SUM(is_swing),0),3) AS whiff_pct,
           ROUND(AVG(exit_speed) FILTER (WHERE is_in_play=1),2) AS avg_ev_on_contact,
           ROUND(SUM(is_hard_hit)::numeric / NULLIF(SUM(is_in_play),0),3) AS hard_hit_pct
    FROM v_pitch_events
    WHERE {date_clause()} AND rel_speed >= {input$velo}
      AND plate_loc_height >= 2.9
      AND pitch_type IN ('Fastball','FourSeamFastBall','TwoSeamFastBall','Sinker')
    GROUP BY 1 HAVING COUNT(*) >= 20
    ORDER BY hard_hit_pct DESC NULLS LAST")
    run_query(sql)
  }, ignoreInit = FALSE)

  ev_monthly_data <- reactive({ run_query("SELECT * FROM mart_hitter_ev_monthly_delta ORDER BY avg_ev_delta ASC") })
  ev_risers_data <- reactive({ run_query("SELECT * FROM mart_hitter_ev_14d_risers ORDER BY avg_ev_delta DESC") })
  pitcher_catcher_data <- reactive({ run_query("SELECT * FROM mart_pitcher_catcher_pairs ORDER BY pitcher_id, csw_pct DESC") })

  matchup_data <- eventReactive(input$refresh, {
    req(input$pitcher)
    sql <- glue("WITH pitcher_profile AS (
      SELECT pitch_type, COUNT(*)::numeric / SUM(COUNT(*)) OVER () AS usage_rate
      FROM v_pitch_events
      WHERE pitcher_id = '{input$pitcher}'
      GROUP BY pitch_type
    ),
    hitter_vs_type AS (
      SELECT batter_name, pitch_type,
             AVG(CASE WHEN is_whiff=1 THEN 1 ELSE 0 END)::numeric AS whiff_per_pitch,
             AVG(CASE WHEN is_in_play=1 THEN 1 ELSE 0 END)::numeric AS inplay_rate,
             AVG(exit_speed) FILTER (WHERE is_in_play=1) AS avg_ev
      FROM v_pitch_events
      WHERE game_date >= current_date - interval '180 days'
      GROUP BY batter_name, pitch_type
    )
    SELECT h.batter_name,
           ROUND(SUM(p.usage_rate * COALESCE(h.whiff_per_pitch,0)),3) AS weighted_whiff_risk,
           ROUND(SUM(p.usage_rate * COALESCE(h.avg_ev,0)),2) AS weighted_ev,
           ROUND((0.6*SUM(p.usage_rate*COALESCE(h.whiff_per_pitch,0))
                 -0.3*SUM(p.usage_rate*COALESCE(h.avg_ev,0))/100.0
                 -0.1*SUM(p.usage_rate*COALESCE(h.inplay_rate,0))),3) AS matchup_risk_score
    FROM pitcher_profile p
    LEFT JOIN hitter_vs_type h ON h.pitch_type = p.pitch_type
    GROUP BY h.batter_name
    HAVING h.batter_name IS NOT NULL
    ORDER BY matchup_risk_score ASC")
    run_query(sql)
  }, ignoreInit = FALSE)

  output$pitch_shapes <- renderDT(datatable(pitch_shapes_data(), options = list(pageLength = 15)))
  output$command_watch <- renderDT(datatable(command_watch_data(), options = list(pageLength = 15)))
  output$velo_up <- renderDT(datatable(velo_up_data(), options = list(pageLength = 15)))
  output$ev_monthly <- renderDT(datatable(ev_monthly_data(), options = list(pageLength = 15)))
  output$ev_risers <- renderDT(datatable(ev_risers_data(), options = list(pageLength = 15)))
  output$pitcher_catcher <- renderDT(datatable(pitcher_catcher_data(), options = list(pageLength = 15)))
  output$matchups <- renderDT(datatable(matchup_data(), options = list(pageLength = 15)))
}

shinyApp(ui, server)
