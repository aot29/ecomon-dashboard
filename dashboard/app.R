# -----------------------------------------------------------------------------
# app.R
# This is the main entry point for the Shiny application. It initializes the
# app by loading required libraries, sourcing external files, and defining the
# server logic. The app dynamically adjusts its state based on URL parameters
# and provides interactive visualizations, including heatmaps and moon timelines.
#
# Purpose:
# - This file ties together the UI and server logic to create an interactive
#   dashboard for visualizing and analyzing acoustic activity data.
#
# IMPORTANT note:
# Do not add a server.R file to this app, as the Docker base image used expects
# an app.R and optionally a ui.R, but will not find the app if there is an
# additional server.R. So keep the main logic of the app here in app.R.
# -----------------------------------------------------------------------------

# Load required libraries
library(shiny)
library(bslib)
library(httpuv)
library(ggplot2)
library(viridis)
library(reshape2)
library(grid)
library(png)
library(httr)
library(jsonlite)
library(ini)
library(data.table)
library(suncalc)

# Source external files
source("ui.R")
source("utils/ephem.R")
source("components/heatmap.R")
source("utils/data_processing.R")
source("utils/url.R")
source("utils/hasura.R")
source("utils/download_data.R")
source("components/canvas_controls.R")
source("components/moon.R")

# -----------------------------------------------------------------------------
# Function: render_site_list_ui
# Description:
#   Dynamically renders the list of available sites based on the provided
#   site data frame and user input.
#
# Parameters:
#   - site_df (data.frame): Data frame containing site information.
#   - input (Shiny input): Shiny input object.
#
# Returns:
#   - A Shiny UI element (HTML list) for the site list.
# -----------------------------------------------------------------------------
render_site_list_ui <- function(site_df, input) {
  renderUI({
    selected <- input$selected_sites
    tags$ul(
      lapply(seq_len(nrow(site_df)), function(i) {
        if (is.null(selected) || !(site_df$id[i] %in% selected)) {
          tags$li(
            site_df$title[i],
            id = paste0("site_", site_df$id[i]),
            onclick = sprintf(
              "var sel = Shiny.shinyapp.$inputValues.selected_sites || [];
               if(sel.indexOf(%d) === -1) { sel.push(%d); }
               Shiny.setInputValue('selected_sites', sel, {priority: 'event'});",
              site_df$id[i], site_df$id[i]
            ),
            class = "site-list-item"
          )
        }
      })
    )
  })
}

# -----------------------------------------------------------------------------
# Function: render_selected_site_ui
# Description:
#   Dynamically renders the list of selected sites based on user input.
#
# Parameters:
#   - site_df (data.frame): Data frame containing site information.
#   - input (Shiny input): Shiny input object.
#
# Returns:
#   - A Shiny UI element (HTML list) for the selected site list.
# -----------------------------------------------------------------------------
render_selected_site_ui <- function(site_df, input) {
  renderUI({
    sel <- if (!is.null(input$selected_sites))
      which(site_df$id %in% input$selected_sites) else integer(0)
    tags$ul(
      lapply(sel, function(i) {
        tags$li(
          site_df$title[i],
          id = paste0("selected_site_", site_df$id[i]),
          onclick = sprintf(
            "var sel = Shiny.shinyapp.$inputValues.selected_sites || [];
             var idx = sel.indexOf(%d);
             if(idx !== -1) { sel.splice(idx,1); }
             Shiny.setInputValue('selected_sites', sel, {priority: 'event'});",
            site_df$id[i]
          ),
          class = "site-list-item"
        )
      })
    )
  })
}

# -----------------------------------------------------------------------------
# Function: register_download_handler
# Description:
#   Registers a download handler for exporting data as a CSV file. The handler
#   dynamically generates the filename and fetches the data for download.
#
# Parameters:
#   - output (Shiny output): Shiny output object.
#   - species_id, species_name, model_id, site_id, site_name, year, threshold:
#     Parameters used to fetch and filter the data for download.
#
# Returns:
#   - None. Registers the download handler directly.
# -----------------------------------------------------------------------------
register_download_handler <- function(
    output, species_id, species_name, model_id, site_id, site_name, year, threshold) {

  sanitize = function(filename) {
    # Sanitize the filename to remove any unwanted characters
    gsub("[^a-zA-Z0-9_-]", "_", filename)
  }

  output$download_data <- downloadHandler(
    filename = function() {
      cat("Generating filename for download...\n")
      filename <- sprintf("%s_%s_%s", site_name, year, species_name)
      sanitized_filename <- sanitize(filename)
      paste0(sanitized_filename, ".csv")
    },
    content = function(file) {
      cat("Fetching download data...\n")
      download_data <- get_download_data(
        species_id = species_id,
        model_id = model_id,
        site_id = site_id,
        year = year,
        threshold = threshold
      )

      if (is.null(download_data)) {
        cat("No data returned by get_download_data.\n")
        stop("No data available for download.")
      }

      cat("Writing data to CSV file...\n")
      write.csv(download_data, file, row.names = TRUE)
      cat("Download data written to file:", file, "\n")
    }
  )
}

# -----------------------------------------------------------------------------
# Main Server Function
# Description:
#   Defines the server logic for the Shiny app, including reactive values,
#   observers, and rendering of UI components.
# -----------------------------------------------------------------------------
server <- function(input, output, session) {
  # Reactive values for URL parameters
  url_threshold <- reactiveVal(NULL)
  url_site_ids <- reactiveVal(NULL)
  url_site_name <- reactiveVal(NULL)
  url_species_id <- reactiveVal(NULL)
  url_species_name <- reactiveVal(NULL)
  url_model_id <- reactiveVal(NULL)
  url_lat <- reactiveVal(NULL)
  url_lon <- reactiveVal(NULL)
  url_year <- reactiveVal(NULL)

  # Parse URL query parameters
  observe({
    parse_url_parameters(
      session,
      url_threshold, url_site_ids, url_site_name, url_species_id, url_species_name,
      url_model_id, url_lat, url_lon, url_year
    )
  })

  # Register the download handler
  observe({
    if (!is.null(url_species_id()) && !is.null(url_model_id()) &&
          !is.null(url_site_ids()) && !is.null(url_year())) {
      register_download_handler(
        output, url_species_id(), url_species_name(), url_model_id(),
        url_site_ids()[1],  # Use the first site from the list
        url_site_name(),
        url_year(), url_threshold()
      )
    } else {
      cat("Parameters are not set. Download handler not registered.\n")
    }
  })

  # Load and process data when app starts
  heatmap_data <- reactive({
    if (is.null(url_species_id()) || is.null(url_model_id()) ||
          is.null(url_site_ids()) || is.null(url_year()) ||
          is.null(url_lat()) || is.null(url_lon())) {
      return(NULL)
    }

    tryCatch({
      load_and_process_data(
        species_id = url_species_id(),
        model_id = url_model_id(),
        site_id = url_site_ids()[1],
        year = url_year(),
        threshold = url_threshold()
      )
    }, error = function(e) {
      print(paste("Error in load_and_process_data:", e$message))
      return(NULL)
    })
  })

  # Heatmap rendering
  observe({
    sun_toggle <- !is.null(input$sun_toggle) && input$sun_toggle %% 2 == 1
    twilight_toggle <- !is.null(input$twilight_toggle) && input$twilight_toggle %% 2 == 1

    render_heatmap(
      input, output, session, heatmap_data, url_year, url_threshold,
      sun_toggle, twilight_toggle, url_lat(), url_lon()
    )
  })

  # Acoustic activity text
  output$acoustic_activity_text <- renderUI({
    threshold <- url_threshold()
    count_above_threshold <- get_count_above_threshold(
      species_id = url_species_id(),
      model_id = url_model_id(),
      site_id = url_site_ids()[1],
      year = url_year(),
      threshold = threshold
    )

    cat(count_above_threshold, "minutes with acoustic activity above threshold", threshold, "\n")
    tags$span(paste("No. of minutes with acoustic activity:", count_above_threshold))
  })

  # Moon timeline plot
  observe({
    moon_toggle <- !is.null(input$moonphase_toggle) && input$moonphase_toggle %% 2 == 1
    render_moon_timeline(input, output, session, url_year, moon_toggle)
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)