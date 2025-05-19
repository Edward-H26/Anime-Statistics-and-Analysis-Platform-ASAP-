library(shiny)
library(shinyjs)
library(httr)
library(jsonlite)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)
library(plotly)
library(ggplot2)
library(shinythemes)
library(tidyr)
library(xgboost)

conn <- dbConnect(SQLite(), "anime_database.sqlite")

fetch_genres <- function() {
  
  genre_url <- "https://api.jikan.moe/v4/genres/anime"
  response <- GET(genre_url)
  
  if (status_code(response) == 200) {
    
    genre_data <- fromJSON(content(response, "text", encoding = "UTF-8"))$data
    
    genre_df <- data.frame(genre_id = genre_data$mal_id, genre_name = genre_data$name, stringsAsFactors = FALSE)
    
    return(genre_df)
    
  } else {
    
    print("Failed to fetch genres")
    return(NULL)
    
  }
}

fetch_data <- function(pages) {
  
  withProgress(message = "Fetching latest anime data", value = 0, {
    
    init_resp <- GET("https://api.jikan.moe/v4/anime")
    
    if (status_code(init_resp) != 200) {
      
      print("Failed to retrieve pagination info")
      return(NULL)
      
    }
    
    init_data <- fromJSON(content(init_resp, "text", encoding = "UTF-8"))
    last_page <- init_data$pagination$last_visible_page
    
    start_page <- last_page
    end_page <- max(1, last_page - pages + 1)
    
    genres_df <- fetch_genres()
    
    all_anime_df <- data.frame()
    
    for (page in start_page:end_page) {
      
      tryCatch({
        
        url <- paste0("https://api.jikan.moe/v4/anime?page=", page)
        response <- GET(url)
        
        if (status_code(response) == 200) {
          
          data <- content(response, "text", encoding = "UTF-8")
          parsed_data <- fromJSON(data)
          
          if (!is.null(parsed_data$data) && is.data.frame(parsed_data$data) && nrow(parsed_data$data) > 0) {
            
            anime_data_json <- parsed_data$data
            
            genres_vec <- sapply(seq_len(nrow(anime_data_json)), function(i) {
              
              genres_list <- anime_data_json$genres[[i]]
              
              if (is.null(genres_list) || length(genres_list) == 0) {
                
                return("")
                
              } else {
                
                return(paste(genres_list$name, collapse = ", "))
                
              }
            })
            
            studios_vec <- sapply(seq_len(nrow(anime_data_json)), function(i) {
              
              studios_list <- anime_data_json$studios[[i]]
              
              if (is.null(studios_list) || length(studios_list) == 0) {
                
                return("")
                
              } else {
                
                return(paste(studios_list$name, collapse = ", "))
                
              }
            })
            
            producers_vec <- sapply(seq_len(nrow(anime_data_json)), function(i) {
              
              producers_list <- anime_data_json$producers[[i]]
              
              if (is.null(producers_list) || length(producers_list) == 0) {
                
                return("")
                
              } else {
                
                return(paste(producers_list$name, collapse = ", "))
                
              }
            })
            
            anime_df <- data.frame(
              mal_id = anime_data_json$mal_id,
              title = anime_data_json$title,
              type = replace(anime_data_json$type, is.na(anime_data_json$type), NA),
              episodes = replace(anime_data_json$episodes, is.na(anime_data_json$episodes), NA),
              status = replace(anime_data_json$status, is.na(anime_data_json$status), NA),
              rating = replace(anime_data_json$rating, is.na(anime_data_json$rating), NA),
              score = replace(anime_data_json$score, is.na(anime_data_json$score), NA),
              scored_by = replace(anime_data_json$scored_by, is.na(anime_data_json$scored_by), NA),
              rank = replace(anime_data_json$rank, is.na(anime_data_json$rank), NA),
              popularity = replace(anime_data_json$popularity, is.na(anime_data_json$popularity), NA),
              members = replace(anime_data_json$members, is.na(anime_data_json$members), NA),
              favorites = replace(anime_data_json$favorites, is.na(anime_data_json$favorites), NA),
              genres = genres_vec,
              studios = studios_vec,
              producers = producers_vec,
              stringsAsFactors = FALSE
            )
            
            all_anime_df <- rbind(all_anime_df, anime_df)
            
          } else {
            
            message("Skipping page ", page, ": no valid data or empty data frame returned.")
            
          }
          
        } else {
          
          message("Skipping page ", page, ": status code ", status_code(response))
          
        }
      }, error = function(e) {
        
        print(paste("Error fetching page", page, ":", e$message))
        
      })
      
      Sys.sleep(1)
      incProgress(1 / pages)
      
    }
    
    if (nrow(all_anime_df) > 0) {
      
      if (dbExistsTable(conn, "anime")) {
        
        dbExecute(conn, "DROP TABLE anime")
        
      }
      
      dbWriteTable(conn, "anime", all_anime_df, overwrite = TRUE)
      
    }
  })
}

ui <- fluidPage(
  
  theme = shinytheme("united"),
  
  useShinyjs(),
  
  tags$head(
    
    tags$style(HTML("
      .summary-card {
      
      display: inline-block;
      width: 125px;
      vertical-align: top;
      cursor: pointer;
      transition: transform 0.2s;
      padding: 20px;
      border-radius: 10px;
      background: #1DB954; 
      color: white;
      text-align: center;
      margin: 10px;
      box-shadow: 2px 2px 5px rgba(0,0,0,0.3);
      font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
      box-sizing: border-box;
      
    }
      .summary-card:hover {
      
      transform: scale(1.05);
      
      }
    "))
  ),
  
  titlePanel("Anime Statistics and Analysis Platform (ASAP)"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      numericInput("pages", "Total Pages To Fetch:", value = NULL, min = 1),
      actionButton("fetch_show", "Fetch"),
      
      div(
        id = "filter_inputs",
        
        textInput("search", "Search by title:"),
        sliderInput("score", "Score (0–10)", min = 0, max = 10, value = c(0, 10)),
        
        uiOutput("rating"),
        uiOutput("type"),
        uiOutput("genre_filter")
      ),
      
      actionButton("reset_filters", "Reset Filters")
      
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Anime Information",
                 DT::dataTableOutput("animeTable")
        ),
        
        tabPanel("Interactive Data Visualization",
                 fluidRow(
                   column(12, plotlyOutput("scatterPlot")),
                   column(12, plotlyOutput("areaPlot")),
                   column(12, plotlyOutput("polarBarChart")),
                   column(12, plotlyOutput("studioBar"))
                 )
        ),
        
        tabPanel("Explore New Animes Based On Your Preference",
                 DT::dataTableOutput("recommendTable")
        ),
        
        tabPanel("Future Trend Predicition Based On Score",
                 DT::dataTableOutput("predictTable"),
                 br(),
                 plotlyOutput("lm_plot")
        ),
        
        tabPanel("Your Anime Highlights",
                 h3("Your Stats at a Glance"),
                 uiOutput("wrapped_overviewUI"),
                 
                 h3("Your Top 5 Anime Based On Their Scores"),
                 DT::dataTableOutput("topAnimeScoreTable"),
                 
                 h3("Your Favorite Genres"),
                 plotlyOutput("topGenresPlot"),
                 
                 h3("Your Favorite Studios"),
                 plotlyOutput("topStudiosPlot"),
                 
                 hr(),
                 
                 fluidRow(
                   column(3, uiOutput("card_total_ui")),
                   column(3, uiOutput("card_avgScore_ui")),
                   column(3, uiOutput("card_avgMembers_ui")),
                   column(3, uiOutput("card_avgFavorites_ui"))
                 ),
                 
                 fluidRow(
                   column(3, uiOutput("card_avgEpisodes_ui")),   
                   column(3, uiOutput("card_ratingDist_ui")),
                   column(3, uiOutput("card_typeDist_ui")),
                   column(3, uiOutput("card_popularity_ui"))      
                 )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  data_version <- reactiveVal(0)
  
  observeEvent(input$fetch_show, {
    
    shinyjs::disable("fetch_show")
    fetch_data(input$pages)
    
    data_version(data_version() + 1)
    shinyjs::enable("fetch_show")
    
  })
  
  observeEvent(input$reset_filters, {
    
    reset("filter_inputs")
    
  })
  
  rating_type <- reactive({
    
    if (!dbExistsTable(conn, "anime")) return(NULL)
    dbGetQuery(conn, "SELECT DISTINCT rating FROM anime WHERE rating IS NOT NULL")
    
  })
  
  output$rating <- renderUI({
    
    selectInput("rating", "Rating (PG, PG13, etc)", 
                choices = rating_type() %>% pull(rating),
                selected = NULL, multiple = TRUE)
    
  })
  
  type_type <- reactive({
    
    if (!dbExistsTable(conn, "anime")) return(NULL)
    dbGetQuery(conn, "SELECT DISTINCT type FROM anime WHERE type IS NOT NULL")
    
  })
  
  output$type <- renderUI({
    
    selectInput("type", "Type (OVA, TV, etc)", 
                choices = type_type() %>% pull(type),
                selected = NULL, multiple = TRUE)
    
  })
  
  available_genres <- reactive({
    
    query <- "
    SELECT DISTINCT trim(json_each.value, '\"') AS genre
    FROM anime,
         json_each('[' || '\"' || replace(genres, ', ', '\",\"') || '\"' || ']')
    WHERE genres <> ''
  "
    res <- dbGetQuery(conn, query)
    
    if(nrow(res) > 0) res$genre else character(0)
    
  })
  
  output$genre_filter <- renderUI({
    
    genres <- available_genres()
    selectInput("genre_filter", "Filter by Genre", choices = genres, 
                selected = NULL, multiple = TRUE)
    
  })
  
  filtered_data <- reactive({
    
    data_version()
    
    if (!dbExistsTable(conn, "anime")) {
      return(data.frame(
        mal_id = integer(0),
        title = character(0),
        type = character(0),
        episodes = integer(0),
        status = character(0),
        rating = character(0),
        score = numeric(0),
        scored_by = integer(0),
        rank = integer(0),
        popularity = integer(0),
        members = integer(0),
        favorites = integer(0),
        genres = character(0),
        studios = character(0),
        producers = character(0),
        stringsAsFactors = FALSE
      ))
      
    }
    
    base_query <- "SELECT * FROM anime"
    conditions <- c()
    params <- list()
    
    if (!is.null(input$search) && input$search != "") {
      
      conditions <- c(conditions, "LOWER(title) LIKE ?")
      params <- c(params, paste0("%", tolower(input$search), "%"))
      
    }
    if (!is.null(input$score) && length(input$score) == 2) {
      
      conditions <- c(conditions, "score BETWEEN ? AND ?")
      params <- c(params, input$score[1], input$score[2])
      
    }
    if (!is.null(input$rating) && length(input$rating) > 0) {
      
      placeholder <- paste(rep("?", length(input$rating)), collapse = ", ")
      conditions <- c(conditions, paste0("rating IN (", placeholder, ")"))
      params <- c(params, input$rating)
      
    }
    if (!is.null(input$type) && length(input$type) > 0) {
      
      placeholder <- paste(rep("?", length(input$type)), collapse = ", ")
      conditions <- c(conditions, paste0("type IN (", placeholder, ")"))
      params <- c(params, input$type)
      
    }
    if (length(conditions) > 0) {
      
      query <- paste(base_query, "WHERE", paste(conditions, collapse = " AND "))
      
    } else {
      
      query <- base_query
      
    }
    
    dbGetQuery(conn, query, params = params)
    
  })
  
  output$animeTable <- DT::renderDataTable({
    
    orig_data <- filtered_data() %>% mutate(row_id = row_number())
    
    onehot <- orig_data %>%
      mutate(genres_list = strsplit(genres, ",\\s*")) %>%
      unnest(genres_list) %>%
      mutate(dummy = 1) %>%
      pivot_wider(id_cols = row_id, 
                  names_from = genres_list, 
                  values_from = dummy, 
                  values_fill = list(dummy = 0))
    
    final_data <- left_join(orig_data, onehot, by = "row_id") %>% 
      select(-row_id) %>%
      mutate(
        fav_pct = ifelse(members > 0, round((favorites / members * 100), 2), NA),
        scored_pct = ifelse(members > 0, round((scored_by / members * 100), 2), NA)
      )
    
    if (!is.null(input$genre_filter) && length(input$genre_filter) > 0) {
      
      final_data <- final_data %>% filter(if_all(all_of(input$genre_filter), ~ . == 1))
      
    }
    
    final_data <- final_data %>% 
      select(title, type, episodes, status, rating, rank, popularity,
             members, score, scored_by, scored_pct, favorites, fav_pct, genres, studios, producers, mal_id) %>%
      rename(
        "Anime Title" = title,
        "Anime Type" = type,
        "Episodes" = episodes,
        "Release Status" = status,
        "Anime Rating" = rating,
        "Anime Rank" = rank,
        "Anime Popularity"  = popularity,
        "Viewers"   = members,
        "Anime Score" = score,
        "Viewers Who Scored" = scored_by,
        "Scored Percentage" = scored_pct,
        "Favorites" = favorites,
        "Favorited Rate"      = fav_pct,
        "Genres"              = genres,
        "Studios"             = studios,
        "Producers"           = producers,
        "Anime ID"            = mal_id
      )
    
    DT::datatable(final_data, options = list(searching = FALSE))
    
  })
  
  output$scatterPlot <- renderPlotly({
    
    data <- filtered_data()
    data <- data[!is.na(data$score) & !is.na(data$members), ]
    
    if(nrow(data) == 0) return(NULL)
    
    r_val <- cor(data$score, data$members, use = "complete.obs")
    r_label <- paste0("r = ", round(r_val, 2))
    
    p <- plot_ly(
      data,
      x = ~score,
      y = ~members,
      type = 'scatter',
      mode = 'markers',
      marker = list(
        size      = 10,
        color     = ~score,
        colorscale = list(
          c(0,   "#F4F7F7"),
          c(0.3, "#D5E5E5"),
          c(0.5, "#AACFD0"),
          c(0.7, "#79A8A9"),
          c(0.9, "#1F4E5F"),
          c(1,   "#18230F")
        ),
        cmin       = min(data$score),
        cmax       = max(data$score),
        colorbar   = list(title = "Score")
      ),
      
      text = ~paste("Title:", title,
                    "<br>Score:", score,
                    "<br>Viewers:", members),
      
      hoverinfo = "text"
      
    ) %>%
      layout(
        title = "Relationship between Anime Score and Number of Viewers",
        
        xaxis = list(title = "Anime Score", showgrid = TRUE, zeroline = FALSE),
        yaxis = list(title = "Number of Viewers", showgrid = TRUE, zeroline = FALSE),
        
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        
        annotations = list(
          list(
            xref      = "paper", yref = "paper",
            x         = 0.02,    y    = 0.98,
            text      = r_label,
            showarrow = FALSE,
            font      = list(size = 14, family = "Arial", color = "black")
          )
        )
      )
    
    p
    
  })
  
  output$areaPlot <- renderPlotly({
    
    data <- filtered_data()
    data <- data %>% filter(!is.na(score), !is.na(members), members > 0)
    
    if(nrow(data) == 0) return(NULL)
    
    data <- data %>%
      mutate(
        fav_pct = favorites / members * 100,
        scored_pct = scored_by / members * 100,
        score_bin = round(score, 1)
      )
    
    area_data <- data %>%
      group_by(score_bin) %>%
      summarize(
        avg_fav_pct = mean(fav_pct, na.rm = TRUE),
        avg_scored_pct = mean(scored_pct, na.rm = TRUE)
      ) %>%
      arrange(score_bin)
    fig <- plot_ly(type = "scatter", mode = "none", fill = "tozeroy")
    
    fig <- fig %>% add_trace(
      x          = ~area_data$score_bin,
      y          = ~area_data$avg_fav_pct,
      name       = "Favorited Rated",   
      fillcolor  = "rgba(168,216,234,0.5)"
    )
    
    fig <- fig %>% add_trace(
      x          = ~area_data$score_bin,
      y          = ~area_data$avg_scored_pct,
      name       = "Scored Rate",        
      fill       = "tozeroy",
      fillcolor  = "rgba(255,212,96,0.5)"
    )
    
    fig <- fig %>% layout(
      title  = list(
        text  = "Engagement Proportions by Anime User Score", 
        x     = 0.5,
        xanchor = "center"
      ),
      xaxis  = list(title = "Anime Score"),
      yaxis  = list(title = "Engagement Proportion (%)"),
      legend = list(title = list(text = "Metric")),           
      margin = list(t = 100)
    )
    
    fig
    
  })
  
  output$polarBarChart <- renderPlotly({
    
    data <- filtered_data()
    data <- data[!is.na(data$score) & data$score > 0, ]
    
    if(nrow(data) == 0) return(NULL)
    
    expanded <- strsplit(data$genres, ",\\s*")
    
    df_list <- lapply(seq_along(expanded), function(i) {
      
      if(length(expanded[[i]]) == 0 || expanded[[i]][1] == "") return(NULL)
      data.frame(
        genre = expanded[[i]],
        score = data$score[i],
        stringsAsFactors = FALSE
      )
      
    })
    
    merged_df <- do.call(rbind, df_list)
    
    if(is.null(merged_df) || nrow(merged_df) == 0) return(NULL)
    
    summary_df <- merged_df %>%
      group_by(genre) %>%
      summarize(
        freq = n(),
        avg_score = mean(score, na.rm = TRUE)
      ) %>%
      arrange(desc(freq))
    top15_df <- summary_df %>%
      slice_head(n = 15) %>%
      arrange(desc(avg_score))
    plot_ly(type = 'barpolar') %>%
      add_trace(
        r = top15_df$avg_score,
        theta = top15_df$genre,
        text = paste("Genre:", top15_df$genre,
                     "<br>Avg Score:", round(top15_df$avg_score, 2),
                     "<br>Frequency:", top15_df$freq),
        
        hoverinfo = "text",
        
        marker = list(
          color = top15_df$avg_score,
          colorscale = "RdPu",
          cmin = min(top15_df$avg_score, na.rm = TRUE),
          cmax = max(top15_df$avg_score, na.rm = TRUE)
        )
        
      ) %>%
      layout(
        title  = list(text = "Average Scores of the 15 Most Frequent Anime Genres",
                      x = 0.5,      
                      xanchor = "center"),
        margin = list(t = 100,              
                      l = 80, r = 80, b = 80),
        polar  = list(
          domain      = list(x = c(0, 1), y = c(0, 1)),  
          radialaxis  = list(range = c(0,
                                       max(top15_df$avg_score, na.rm = TRUE) * 1.2),
                             visible = TRUE, showline = FALSE),
          angularaxis = list(direction = "clockwise",
                             rotation  = 90)  
        ),
        
        showlegend = FALSE
        
      )
  })
  
  output$studioBar <- renderPlotly({
    
    d <- filtered_data() |>
      filter(!is.na(score))
    
    if (nrow(d) == 0) return(NULL)
    
    studio_vec <- strsplit(d$studios, ",\\s*")
    
    df_list <- lapply(seq_along(studio_vec), function(i) {
      
      if (length(studio_vec[[i]]) == 0 || studio_vec[[i]][1] == "") return(NULL)
      
      data.frame(
        studio = studio_vec[[i]],
        score  = d$score[i],
        stringsAsFactors = FALSE
      )
      
    })
    
    d_expanded <- do.call(rbind, df_list)
    
    stats <- d_expanded |>
      group_by(studio) |>
      summarise(
        avg_score = mean(score),
        n_titles  = n(),
        .groups   = "drop"
      ) |>
      arrange(desc(avg_score)) |>
      slice_head(n = 15)
    
    plot_ly(
      stats,
      x = ~avg_score,
      y = ~reorder(studio, avg_score),
      type = "bar",
      orientation = "h",
      marker = list(
        color      = ~(avg_score),
        colorscale = "Portland", 
        reversescale = TRUE, 
        cmin       = min(stats$avg_score),
        cmax       = max(stats$avg_score),
        colorbar   = list(title = "Average Scores")
      ),
      
      text = ~paste0(
        "<b>", studio, "</b>",
        "<br>Average Score: ", round(avg_score, 2),
        "<br>Number Of Titles: ", n_titles
      ),
      
      hoverinfo = "text"
      
    ) |>
      layout(
        title         = list(text = "Top 10 Studios By Score"),
        xaxis         = list(title = "Average Scores"),
        yaxis         = list(title = "Studios"),
        plot_bgcolor  = "white",
        paper_bgcolor = "white",
        margin        = list(l = 120)
      )
    
  })
  
  output$recommendTable <- DT::renderDataTable({
    
    orig_data <- filtered_data() %>% 
      filter(!is.na(score)) %>%
      mutate(row_id = row_number())
    
    onehot <- orig_data %>%
      mutate(genres_list = strsplit(genres, ",\\s*")) %>%
      unnest(genres_list, keep_empty = TRUE) %>%
      mutate(dummy = 1) %>%
      pivot_wider(id_cols = row_id, 
                  names_from = genres_list, 
                  values_from = dummy, 
                  values_fill = list(dummy = 0))
    
    final_data <- left_join(orig_data, onehot, by = "row_id") %>% 
      select(-row_id) %>%
      mutate(
        fav_pct = ifelse(members > 0, favorites/members*100, NA),
        scored_pct = ifelse(members > 0, scored_by/members*100, NA)
      ) %>%
      distinct(mal_id, .keep_all = TRUE)
    
    if (!is.null(input$genre_filter) && length(input$genre_filter) > 0) {
      
      final_data <- final_data %>% filter(if_all(all_of(input$genre_filter), ~ . == 1))
      
    }
    
    genre_cols <- setdiff(colnames(onehot), "row_id")
    
    X <- as.matrix(final_data[, genre_cols])
    y <- final_data$score
    
    dtrain <- xgb.DMatrix(data = X, label = y)
    params <- list(objective = "reg:squarederror", eval_metric = "rmse", eta = 0.1, max_depth = 6)
    
    xgb_model <- xgb.train(
      params = params,
      data = dtrain,
      nrounds = 50,
      verbose = 0, 
      gamma = 1
    )
    
    final_data$recommendation_score <- predict(xgb_model, X)
    
    final_data <- final_data %>% 
      arrange(desc(recommendation_score)) %>%
      head(50) %>%
      select(title, type, episodes, status, rating, rank, popularity,
             members, score, scored_by, scored_pct, favorites, fav_pct, genres, studios, producers, mal_id, everything()) %>%
      rename(
        "Anime Title" = title,
        "Anime Type" = type,
        "Episodes" = episodes,
        "Release Status" = status,
        "Anime Rating" = rating,
        "Anime Rank" = rank,
        "Anime Popularity" = popularity,
        "Viewers" = members,
        "Anime Score" = score,
        "Viewers Who Scored" = scored_by,
        "Scored Rate" = scored_pct,
        "Favorites" = favorites,
        "Favorited Rate" = fav_pct,
        "Genres" = genres,
        "Studios" = studios,
        "Producers" = producers,
        "Anime ID" = mal_id,
        "Recommendation Score" = recommendation_score
      )
    
    datatable(final_data, options = list(searching = FALSE))
    
  })
  
  lm_model <- reactive({
    
    orig <- filtered_data() %>%
      filter(
        !is.na(score),
        !is.na(episodes),
        !is.na(scored_by),
        !is.na(popularity),
        !is.na(members),
        !is.na(favorites)
      )
    
    req(nrow(orig) >= 10)
    
    lm(score ~ episodes + scored_by + popularity + members + favorites, data = orig)
    
  })
  
  output$predictTable <- DT::renderDataTable({
    
    orig <- filtered_data() %>%
      filter(
        !is.na(score),
        !is.na(episodes),
        !is.na(scored_by),
        !is.na(popularity),
        !is.na(members),
        !is.na(favorites)
      )
    
    fit <- lm_model()
    
    orig$PredictedScore <- predict(fit, newdata = orig)
    
    tbl <- orig %>%
      arrange(desc(PredictedScore)) %>%
      slice_head(n = 10) %>%
      select(
        title, type, episodes, status, rating,
        rank, popularity, members, favorites,
        score, PredictedScore, genres, studios, producers, mal_id
      ) %>%
      rename(
        "Anime Title"     = title,
        "Anime Type"      = type,
        "Episodes"        = episodes,
        "Release Status"  = status,
        "Anime Rating"    = rating,
        "Anime Rank"      = rank,
        "Anime Popularity"= popularity,
        "Viewers"         = members,
        "Favorites"       = favorites,
        "Current Score"    = score,
        "Predicted Score" = PredictedScore,
        "Genres"          = genres,
        "Studios"         = studios,
        "Producers"       = producers,
        "Anime ID"        = mal_id
      )
    
    DT::datatable(
      tbl,
      options = list(pageLength = 10, searching = FALSE),
      rownames = FALSE
    )
    
  })
  
  output$lm_plot <- renderPlotly({
    
    fit <- lm_model()
    req(fit)
    
    df_plot <- data.frame(
      Fitted = fitted(fit),
      Actual = fit$model$score
    )
    
    p <- ggplot(df_plot, aes(x = Fitted, y = Actual)) +
      geom_point() +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
      labs(
        x     = "Predicted Score",
        y     = "Current Score",
        title = "Current Score vs. Predicted Score Based On Our Model"
      )
    
    ggplotly(p)
    
  })
  
  
  summary_details <- reactive({
    
    df <- filtered_data()
    total <- nrow(df)
    
    members <- as.numeric(df$members)
    favorites <- as.numeric(df$favorites)
    episodes <- as.numeric(df$episodes)
    score <- as.numeric(df$score)
    popularity <- as.numeric(df$popularity)
    
    avgScore    <- if(total>0) round(mean(df$score,      na.rm=TRUE), 2) else NA
    avgMembers  <- if(total>0) round(mean(df$members,    na.rm=TRUE), 0) else NA
    avgFavorites<- if(total>0) round(mean(df$favorites,  na.rm=TRUE), 0) else NA
    avgPopularity<-if(total>0) round(mean(df$popularity, na.rm=TRUE), 0) else NA
    avgEpisodes <- if(total>0) round(mean(df$episodes,   na.rm=TRUE), 0) else NA
    
    list(
      total        = total,
      avgScore     = avgScore,
      avgMembers   = avgMembers,
      avgFavorites = avgFavorites,
      avgEpisodes  = avgEpisodes,
      avgPopularity= avgPopularity
    )
  })
  
  output$card_total_ui <- renderUI({
    actionLink("card_total", 
               div(class = "summary-card",
                   h4("Total Anime"),
                   h3(summary_details()$total)
               )
    )
    
  })
  
  output$card_avgScore_ui <- renderUI({
    
    actionLink("card_avgScore", 
               div(class = "summary-card",
                   h4("Anime Average Score"),
                   h3(summary_details()$avgScore)
               )
    )
    
  })
  
  output$card_avgMembers_ui <- renderUI({
    
    actionLink("card_avgMembers", 
               div(class = "summary-card",
                   h4("Anime Average Viewers"),
                   h3(summary_details()$avgMembers)
               )
    )
    
  })
  
  output$card_avgFavorites_ui <- renderUI({
    
    actionLink("card_avgFavorites", 
               div(class = "summary-card",
                   h4("Anime Average Favorites"),
                   h3(summary_details()$avgFavorites)
               )
    )
    
  })
  
  output$card_avgEpisodes_ui <- renderUI({
    
    actionLink("card_avgEpisodes", 
               div(class = "summary-card",
                   h4("Anime Average Episodes"),
                   h3(summary_details()$avgEpisodes)
               )
    )
    
  })
  
  output$card_ratingDist_ui <- renderUI({
    
    actionLink("card_ratingDist",
               div(class = "summary-card",
                   h4("Anime MPAA Rating Distribution")
               )
    )
    
  })
  
  output$card_typeDist_ui <- renderUI({
    
    actionLink("card_typeDist",
               div(class = "summary-card",
                   h4("Anime Type Distribution")
               )
    )
    
  })
  
  output$card_popularity_ui <- renderUI({
    
    actionLink("card_popularity",
               div(class = "summary-card",
                   h4("Anime Average Popularity"),
                   h3(summary_details()$avgPopularity)
               )
    )
    
  })
  
  observeEvent(input$card_total, {
    
    showModal(modalDialog(
      title = "Detailed Information: Total Anime",
      DT::dataTableOutput("modal_totalTable"),
      easyClose = TRUE,
      size = "l"
    ))
    
  })
  
  output$modal_totalTable <- DT::renderDataTable({
    
    df <- filtered_data()
    
    if(nrow(df) > 0) {
      
      df[, c("title", "score", "members", "favorites")]
      
    } else {
      
      data.frame(Message = "No data available.")
      
    }
    
  })
  
  observeEvent(input$card_avgScore, {
    
    showModal(modalDialog(
      title = "Detailed Information: Score Distribution",
      plotlyOutput("modal_avgScorePlot"),
      easyClose = TRUE,
      size = "l"
    ))
    
  })
  
  output$modal_avgScorePlot <- renderPlotly({
    
    df <- filtered_data()
    df <- df[!is.na(df$score), ]
    
    mean_score <- mean(df$score)
    sd_score   <- sd(df$score)
    
    hist_info  <- hist(df$score, breaks = seq(0, 10, by = 0.5), plot = FALSE)
    max_count  <- max(hist_info$counts)
    
    p <- ggplot(df, aes(x = score)) +
      geom_histogram(
        binwidth = 0.5,
        aes(fill = after_stat(count)),
        color = "white") +
      scale_fill_viridis_c(option = "plasma", name = "Count") +
      annotate(
        "rect",
        xmin = mean_score - sd_score,
        xmax = mean_score + sd_score,
        ymin = 0,
        ymax = Inf,
        alpha = 0.1,
        fill = "grey70") +
      geom_vline(xintercept = mean_score + sd_score,
                 linetype = "dashed",
                 linewidth = 1,
                 color = "magenta") +
      geom_vline(xintercept = mean_score - sd_score,
                 linetype = "dashed",
                 linewidth = 1,
                 color = "magenta") +
      annotate(
        "text",
        x     = 1,               
        y     = max_count * 0.8, 
        label = sprintf("Mean = %.2f\nSD   = %.2f", mean_score, sd_score),
        hjust = 0, 
        vjust = 1,
        size  = 4) +
      labs(
        title = "Anime Score Distribution",
        x     = "Score",
        y     = "Count") +
      scale_x_continuous(limits = c(0, 10), breaks = 0:10) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title    = element_text(face = "bold", hjust = 0.5),
        axis.title    = element_text(face = "bold")
      )
    
    ggplotly(p, tooltip = "none")
    
  })
  
  observeEvent(input$card_avgMembers, {
    
    showModal(modalDialog(
      title     = "Members Summary Statistics",
      tableOutput("modal_membersSummary"),
      easyClose = TRUE
    ))
    
  })
  
  output$modal_membersSummary <- renderTable({
    
    df <- filtered_data()
    vec <- as.numeric(df$members)
    stats <- summary(vec)
    data.frame(
      Statistic = names(stats),
      Value     = as.numeric(stats),
      row.names = NULL
    )
    
  })
  
  observeEvent(input$card_avgFavorites, {
    
    showModal(modalDialog(
      title     = "Favorites Summary Statistics",
      tableOutput("modal_favoritesSummary"),
      easyClose = TRUE
    ))
    
  })
  
  output$modal_favoritesSummary <- renderTable({
    
    df <- filtered_data()
    
    vec <- as.numeric(df$favorites)
    
    stats <- summary(vec)
    
    data.frame(
      Statistic = names(stats),
      Value     = as.numeric(stats),
      row.names = NULL
    )
    
  })
  
  observeEvent(input$card_avgEpisodes, {
    
    showModal(modalDialog(
      title     = "Episodes Summary Statistics",
      tableOutput("modal_episodesSummary"),
      easyClose = TRUE
    ))
    
  })
  
  output$modal_episodesSummary <- renderTable({
    
    df <- filtered_data()
    
    vec <- as.numeric(df$episodes)
    
    stats <- summary(vec)
    
    data.frame(
      Statistic = names(stats),
      Value     = as.numeric(stats),
      row.names = NULL
    )
    
  })
  
  observeEvent(input$card_popularity, {
    
    showModal(modalDialog(
      title     = "Popularity Summary Statistics",
      tableOutput("modal_popularitySummary"),
      easyClose = TRUE
    ))
    
  })
  output$modal_popularitySummary <- renderTable({
    
    df <- filtered_data()
    
    vec <- as.numeric(df$popularity)
    
    stats <- summary(vec)
    
    data.frame(
      Statistic = names(stats),
      Value     = as.numeric(stats),
      row.names = NULL
    )
    
  })
  
  observeEvent(input$card_ratingDist, {
    
    showModal(modalDialog(
      title    = "Rating Distribution",
      plotlyOutput("modal_ratingPie"),
      easyClose = TRUE,
      size     = "l"
    ))
    
  })
  
  output$modal_ratingPie <- renderPlotly({
    
    df <- filtered_data()
    
    if (!"rating" %in% names(df)) return(NULL)    
    
    rating_counts <- df %>%
      filter(!is.na(rating)) %>%
      count(rating)
    
    if (nrow(rating_counts) == 0) return(NULL)
    
    p <- ggplot(rating_counts, aes(x = "", y = n, fill = rating)) +
      geom_col(width = 1) +
      coord_polar(theta = "y") +
      labs(fill = "Rating", title = "Anime MPAA Rating Distribution") +
      theme_void() +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
    
    ggplotly(p, tooltip = c("fill", "y"))
    
  })
  
  observeEvent(input$card_typeDist, {
    
    showModal(modalDialog(
      title    = "Type Distribution",
      plotlyOutput("modal_typePie"),
      easyClose = TRUE,
      size     = "l"
    ))
    
  })
  
  output$modal_ratingPie <- renderPlotly({
    
    df <- filtered_data()
    
    if (!"rating" %in% names(df)) return(NULL)
    
    rating_counts <- df %>%
      filter(!is.na(rating)) %>%
      count(rating)
    
    if (nrow(rating_counts) == 0) {
      
      return(plotly::plotly_empty(type = "pie") %>%
               layout(title = "No Ratings Available"))
      
    }
    
    plot_ly(
      rating_counts,
      labels = ~rating,
      values = ~n,
      type   = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'label+value'
    ) %>%
      layout(title = "Anime MPAA Rating Distribution")
    
  })
  
  output$modal_typePie <- renderPlotly({
    
    df <- filtered_data()
    
    if (!"type" %in% names(df)) return(NULL)
    
    type_counts <- df %>%
      filter(!is.na(type)) %>%
      count(type)
    
    if (nrow(type_counts) == 0) {
      return(plotly::plotly_empty(type = "pie") %>%
               layout(title = "No Types Available"))
    }
    
    plot_ly(
      type_counts,
      labels = ~type,
      values = ~n,
      type   = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'label+value'
    ) %>%
      layout(title = "Anime Type Distribution")
  })
  
  wrapped_data <- reactive({
    
    df <- filtered_data()
    df$score <- as.numeric(df$score)
    df$members <- as.numeric(df$members)
    df$favorites <- as.numeric(df$favorites)
    df$episodes <- as.numeric(df$episodes)
    df
    
  })
  
  output$wrapped_overviewUI <- renderUI({
    
    df <- wrapped_data()
    
    if (is.null(df) || nrow(df) == 0) {
      
      return(h4("No anime in your current filter!"))
      
    }
    
    total_anime <- nrow(df)
    mean_score <- round(mean(df$score, na.rm = TRUE), 2)
    
    genre_vec <- df$genres[!is.na(df$genres) & df$genres != ""]
    genre_list <- strsplit(genre_vec, ",\\s*")
    distinct_genres <- length(unique(unlist(genre_list)))
    
    top_score <- round(max(df$score, na.rm = TRUE), 2)
    longest_series <- max(df$episodes, na.rm = TRUE)
    
    expanded <- df %>%
      filter(!is.na(genres) & genres != "") %>%
      mutate(genres_list = strsplit(genres, ",\\s*")) %>%
      unnest(cols = c(genres_list))
    topGenre <- expanded %>%
      group_by(genres_list) %>%
      tally(sort = TRUE) %>%
      slice_head(n=1) %>%
      pull(genres_list)
    
    if(is.na(topGenre)) {
      
      topGenre <- "Various"
      
    }
    
    tagList(
      p(paste("You’ve explored", total_anime,
              "accross", distinct_genres, "different genres")),
      p(paste("Your animes achieve an average score of ", mean_score)),
      p(paste("Your highest rated anime is",
              top_score, ". Excellent taste!")),
      p(paste("And let’s not forget the", longest_series,
              "episodes in a single series that you watched")),
      p(paste("You are a huge fan of ", topGenre, 
              "! Let's explore more animes based on your preference through our recommendation tab"))
    )
    
  })
  
  output$topAnimeScoreTable <- DT::renderDataTable({
    
    df <- wrapped_data()
    if(nrow(df) == 0) return(data.frame(Message = "No data available."))
    
    df <- df[!is.na(df$score), ]
    top5 <- head(df[order(-df$score), ], 5)
    
    top5 <- top5 %>%
      select(title, score, members, favorites, genres) %>%
      rename(
        "Anime Title" = title,
        "Anime Score" = score,
        "Viewers"     = members,
        "Favorites"   = favorites,
        "Genres"      = genres
      )
    
    DT::datatable(top5, options = list(searching = FALSE, paging = FALSE))
    
  })
  
  output$topGenresPlot <- renderPlotly({
    
    df <- wrapped_data()
    if(nrow(df) == 0) return(NULL)
    
    expanded <- df %>%
      filter(!is.na(genres) & genres != "") %>%
      mutate(genres_list = strsplit(genres, ",\\s*")) %>%
      unnest(cols = c(genres_list))
    
    genre_count <- expanded %>%
      group_by(genres_list) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      head(10)
    
    p <- ggplot(genre_count, aes(x = reorder(genres_list, count), y = count, fill = genres_list)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = count), hjust = -0.1, size = 3.5, fontface = "bold") +
      coord_flip() +
      scale_fill_viridis_d(option = "D") +
      labs(
        title = "Your Top 10 Anime Genres",
        x = "Genres",
        y = "Total Appearances"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p)
    
  })
  
  output$topStudiosPlot <- renderPlotly({
    
    df <- wrapped_data()
    if(nrow(df) == 0) return(NULL)
    
    expanded <- df %>%
      filter(!is.na(studios) & studios != "") %>%
      mutate(studios_list = strsplit(studios, ",\\s*")) %>%
      unnest(cols = c(studios_list))
    
    studio_count <- expanded %>%
      group_by(studios_list) %>%
      summarize(count = n()) %>%
      arrange(desc(count)) %>%
      head(10)
    
    p <- ggplot(studio_count, aes(x = reorder(studios_list, count), y = count, fill = studios_list)) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      geom_text(aes(label = count), hjust = -0.1, size = 3.5, fontface = "bold") +
      coord_flip() +
      scale_fill_viridis_d(option = "C") +
      labs(
        title = "Your Top 10 Anime Studios",
        x = "Studios",
        y = "Total Appearances"
      ) +
      theme_minimal(base_size = 13) +
      theme(
        plot.title = element_text(face = "bold", hjust = 0.5),
        axis.title = element_text(face = "bold"),
        panel.grid.major.y = element_blank()
      )
    
    ggplotly(p)
    
  })
}

shinyApp(ui = ui, server = server)