# Load necessary packages using the 'pacman' package
pacman::p_load(
  tidyverse, shiny, janitor, shinycssloaders, plotly, shinydashboard, shinyWidgets
)

# Read a CSV file and clean column names using 'janitor::clean_names()'
spotify <- read_csv("data/spotify-2023.csv", show_col_types = F) %>% janitor::clean_names()

# Create a dashboard header with a specific title and width
header <- dashboardHeader(title = "Analysis of Most Streamed Spotify Songs 2023", titleWidth = 1500)

# Create a collapsed dashboard sidebar
sidebar <- dashboardSidebar(collapsed = T)

# Create a dashboard body with multiple boxes containing plotlyOutput elements
body <- dashboardBody(
  fluidRow(
    # Create a box with a plotlyOutput element for "top_10_artists" and add a spinner
    box(width = 6, plotlyOutput("top_10_artists", height = 300) %>% withSpinner()),
    # Create a box with a plotlyOutput element for "contributions" and add a spinner
    box(width = 6, plotlyOutput("contributions", height = 300) %>% withSpinner()),
    # Create a box with a plotlyOutput element for "pop_over_time" and add a spinner
    box(width = 6, plotlyOutput("pop_over_time", height = 300) %>% withSpinner()),
    # Create a box with a plotlyOutput element for "avg_happiness" and add a spinner
    box(width = 6, plotlyOutput("avg_happiness", height = 300) %>% withSpinner()),
    # Create a box with a plotlyOutput element for "dist" and add a spinner
    box(width = 6, plotlyOutput("dist", height = 300) %>% withSpinner()),
    # Create a box with a plotlyOutput element for "key_signatures" and add a spinner
    box(width = 6, plotlyOutput("key_signatures", height = 300) %>% withSpinner()),
    # Create a box with a plotlyOutput element for "splom" and add a spinner
    box(width = 6, plotlyOutput("splom", height = 500) %>% withSpinner()),
    # Create a box with a plotlyOutput element for "dot" and add a spinner
    box(width = 6, plotlyOutput("dot", height = 500) %>% withSpinner()),
    # Create a box with a plotlyOutput element for "bpm_vs_dance" and add a spinner
    box(width = 12, plotlyOutput("bpm_vs_dance", height = 500) %>% withSpinner())
  )
)

# Create the UI for the Shiny dashboard, including the header, sidebar, and body, and specify the skin color
ui <- dashboardPage(
  header, sidebar, body,
  skin = "green"
)

# Define the server function that will generate the dynamic content of the Shiny app
server <- function(input, output, session){
  
  # Render a Plotly plot for the top 10 artists with the most songs
  output$top_10_artists <- renderPlotly({
    spotify %>% count(artist_s_name, sort = T) %>% 
      top_n(n = 10, wt = n) %>% 
      mutate(artist_s_name = fct_reorder(artist_s_name, n, .desc = T)) %>% 
      plotly::plot_ly(x = ~artist_s_name, y = ~n) %>% 
      add_bars(color = I("#1DB954")) %>% 
      layout(
        title = "Top 10 Artists with most songs",
        xaxis = list(title = "Artist Name"),
        yaxis = list(title = "Number of Songs")
      )%>%
      config(displayModeBar = FALSE)  # Hide the Plotly display mode bar
  })
  
  # Render a Plotly plot for the distribution of the number of artists contributing to songs
  output$contributions <- renderPlotly({
    spotify %>% 
      mutate(artist_count = as_factor(artist_count)) %>% 
      count(artist_count, sort = T) %>% 
      plot_ly(x = ~artist_count, y = ~ n) %>% 
      add_bars(color = I("#1DB954")) %>% 
      layout(
        title = "How Many Artists are Contributing to Songs",
        xaxis = list(title = "Number of Artists"),
        yaxis = list(title = "Number of Songs")
      )%>%
      config(displayModeBar = FALSE)  # Hide the Plotly display mode bar
  })
  
  # Render a Plotly plot to visualize the popularity of songs on Spotify over the years
  output$pop_over_time <- renderPlotly({
    spotify %>% 
      group_by(released_year) %>% 
      summarise(pop = sum(in_spotify_playlists)) %>% 
      plot_ly(x = ~released_year, y = ~pop, hoverinfo = "text",
              text = ~paste("Year:", released_year, "<br>", 
                            "Playlists:", pop)
      ) %>% 
      add_lines(color = I("#1DB954")) %>% 
      add_markers(color = I("#1DB954"), showlegend = F) %>% 
      layout(
        title = "Popularity of songs on Spotify over the years",
        xaxis = list(title = "Year"),
        yaxis = list(title = "Number of Playlists")
      )%>%
      config(displayModeBar = FALSE)  # Hide the Plotly display mode bar
  })
  
  # Render a Plotly plot to visualize the average happiness score for songs over time
  output$avg_happiness <- renderPlotly({
    spotify %>% 
      dplyr::summarise(
        avg_valence = mean(valence_percent), .by = released_year
      ) %>% 
      plot_ly(
        x = ~released_year, y =~ avg_valence,
        hoverinfo = "text",
        text =  ~paste(
          "Year:", released_year, "<br>", "Average Happiness Score:", paste0(round(avg_valence), "%")
        )
      ) %>% 
      add_lines(color = I("#1DB954")) %>% 
      add_markers(color = I("#1DB954")) %>% 
      layout(
        showlegend = F,
        title = "Average valence (happiness) score for songs over time",
        xaxis = list(title = "Year", zeroline = T),
        yaxis = list(title = "Happiness Score")
      )%>%
      config(displayModeBar = FALSE)  # Hide the Plotly display mode bar
  })
  
  # Render a Plotly plot to visualize the distribution of song streams across the dataset
  output$dist <- renderPlotly({
    spotify %>% 
      mutate(streams = as.numeric(streams)) %>% 
      plot_ly(x = ~streams, hoverinfo = "x") %>% 
      add_histogram(color = I("#1DB954")) %>% 
      layout(
        title = "Distribution of song streams across the dataset",
        xaxis = list(title = "Streams"),
        yaxis = list(title = "Frequency")
      )%>%
      config(displayModeBar = FALSE)  # Hide the Plotly display mode bar
  })
  
  # Render a Plotly plot to visualize the relationship between song tempo (BPM) and danceability (faceted by key signatures and colored by mode)
  output$bpm_vs_dance <- renderPlotly({
    spotify %>% 
      group_by(key) %>% 
      nest() %>% 
      mutate(
        plot = map2(
          data, key,
          \(data, key)
          plot_ly( 
            data = data,
            x = ~ bpm, 
            y = ~ danceability_percent,
            color = ~mode,
            hoverinfo = "text",
            text =  ~paste(
              "BPM:", bpm, "<br>", "Danceability:", paste0(danceability_percent, "%"), "<br>", 
              "Key:", key,  "<br>", "Mode:", mode
            )
          ) %>% 
            add_markers(name = ~key, colors = c("#1DB954", "#191414")) %>% 
            layout(
              showlegend = F,
              title = "Relationship between song tempo (BPM) and Danceability (Faceted by Key Signatures & Colored by Mode)",
              xaxis = list(title = "BPM (Tempo)"),
              yaxis = list(title = "% Danceability")
            ) %>% 
            config(displayModeBar = F)
        )
      ) %>% 
      subplot(nrows = 3, shareX = T, shareY = T)
  })
  
  # Render a Plotly plot to visualize the most common key signatures in popular songs
  output$key_signatures <- renderPlotly({
    spotify %>% 
      filter(!is.na(key)) %>% 
      count(key) %>% 
      mutate(n = round(n/sum(n)*100, 2)) %>% 
      mutate(key = fct_reorder(key, n, .desc = F)) %>% 
      plot_ly(
        x = ~n, y = ~ key,
        hoverinfo = "text",
        text =  ~paste(
          "Key Signature:", key, "<br>", "Popularity:", paste0(n, "%")
        )
      ) %>% 
      add_bars(orientation = 'h', color = I("#1DB954")) %>% 
      layout(
        title = "Most common key signatures in popular songs",
        xaxis = list(zeroline = F, title = "% Popularity"), yaxis = list(title = "Key"))%>%
      config(displayModeBar = FALSE)  # Hide the Plotly display mode bar
  })
  
  # Render a Plotly plot to create a scatter plot matrix
  output$splom <- renderPlotly({
    spotify %>% 
      plot_ly(
        color = ~mode, colors = c("#1DB954", "#191414")
      ) %>% 
      add_trace(
        type = "splom",
        dimensions = list(
          list(label = "Streams", values = ~ streams),
          list(label = "in_spotify_charts", values = ~ in_spotify_charts),
          list(label = "Danceability Percent", values = ~ danceability_percent)
        )
      ) %>% 
      layout(
        title = "Scatter Plot Matrices"
      ) %>% 
      config(displayModeBar = F)  # Hide the Plotly display mode bar
  })
  
  # Render a Plotly plot to visualize the most famous key signatures in Spotify playlists
  output$dot <- renderPlotly({
    spotify %>% 
      filter(!is.na(key)) %>% 
      slice_max(in_spotify_playlists, n = 55) %>% distinct(key, .keep_all = T) %>% 
      mutate(key = fct_reorder(key, in_spotify_playlists, .desc = F)) %>% 
      plot_ly(x = ~ in_spotify_playlists, y = ~ key) %>% 
      add_markers(color = I("#1DB954"))  %>%
      layout(xaxis = list(title = "In Spotify Playlists"),
             yaxis = list(title = "Key", type = "category"),
             title = "Most Famous Key Signatures in Spotify Playlists",
             showlegend = F
      ) %>% 
      config(displayModeBar = F)  # Hide the Plotly display mode bar
  })
}

# Create the Shiny app UI and server
shinyApp(ui, server)
