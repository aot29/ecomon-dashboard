# -----------------------------------------------------------------------------
# url.R
# This file contains utility functions for parsing URL parameters and updating
# the Shiny app's reactive values and inputs based on the parsed parameters.
#
# Functions:
# 1. `parse_url_parameters`: Parses query parameters from the URL and updates
#    reactive values and UI inputs accordingly.
#
# Purpose:
# - This functionality allows the app to dynamically adjust its state based on
#   URL parameters, enabling deep linking and sharing of specific app states.
# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# Function: parse_url_parameters
# Description:
#   Parses query parameters from the URL and updates reactive values and UI
#   inputs in the Shiny app. This function enables the app to dynamically
#   adjust its state based on the URL, supporting features like deep linking.
#
# Parameters:
#   - session (Shiny session): The Shiny session object.
#   - url_threshold (reactiveVal): Reactive value for the confidence threshold.
#   - url_site_ids (reactiveVal): Reactive value for the site IDs.
#   - url_site_name (reactiveVal): Reactive value for the site name.
#   - url_species_id (reactiveVal): Reactive value for the species ID.
#   - url_species_name (reactiveVal): Reactive value for the species name.
#   - url_model_id (reactiveVal): Reactive value for the model ID.
#   - url_lat (reactiveVal): Reactive value for the latitude.
#   - url_lon (reactiveVal): Reactive value for the longitude.
#   - url_year (reactiveVal): Reactive value for the year.
#
# Returns:
#   - None. Updates reactive values and UI inputs directly.
# -----------------------------------------------------------------------------
parse_url_parameters <- function(
  session,
  url_threshold, url_site_ids, url_site_name, url_species_id, url_species_name, url_model_id, url_lat, url_lon, url_year
) {
  # Access the URL search string
  url_search <- session$clientData$url_search
  if (!is.null(url_search) && url_search != "") {
    # Parse query parameters
    query <- parseQueryString(url_search)

    # Model
    if (!is.null(query$model)) {
      model_id <- as.numeric(query$model)
      url_model_id(model_id)

      if (!is.null(query$modelName)) {
        updateSelectInput(session, "canvas_classifier", selected = query$modelName)
      }
    }

    # Site parameters
    if (!is.null(query$siteId)) {
      site_id <- as.numeric(query$siteId)
      url_site_ids(c(site_id))
    }
    if (!is.null(query$siteName)) {
      site_name <- URLdecode(query$siteName)
      updateTextInput(session, "canvas_site", value = site_name)
      url_site_name(site_name)
    }
    if (!is.null(query$lat) && !is.null(query$lon)) {
      lat <- as.numeric(query$lat)
      lon <- as.numeric(query$lon)
      url_lat(lat)
      url_lon(lon)
    }

    # Year
    if (!is.null(query$year)) {
      updateTextInput(session, "canvas_year", value = query$year)
      url_year(as.numeric(query$year))
    }

    # Species
    if (!is.null(query$species)) {
      species_id <- as.numeric(query$species)
      url_species_id(species_id)
    }
    if (!is.null(query$speciesName)) {
      species_name <- URLdecode(query$speciesName)
      updateTextInput(session, "canvas_species", value = species_name)
      url_species_name(species_name)
    }

    # Threshold
    if (!is.null(query$threshold)) {
      threshold <- as.numeric(query$threshold)
      updateTextInput(session, "canvas_threshold", value = threshold)
      url_threshold(threshold)
    }
  }
}
parse_url_parameters <- function(
  session,
  url_threshold, url_site_ids, url_site_name, url_species_id, url_species_name, url_model_id, url_lat, url_lon, url_year
) {
  # Access the URL search string
  url_search <- session$clientData$url_search
  if (!is.null(url_search) && url_search != "") {
    # Parse query parameters
    query <- parseQueryString(url_search)

    # Model
    if (!is.null(query$model)) {
      model_id <- as.numeric(query$model)
      url_model_id(model_id)

      if (!is.null(query$modelName)) {
        updateSelectInput(session, "canvas_classifier", selected = query$modelName)
      }
    }

    # Site parameters
    if (!is.null(query$siteId)) {
      site_id <- as.numeric(query$siteId)
      url_site_ids(c(site_id))
    }
    if (!is.null(query$siteName)) {
      site_name <- URLdecode(query$siteName)
      updateTextInput(session, "canvas_site", value = site_name)
      url_site_name(site_name)
    }
    if (!is.null(query$lat) && !is.null(query$lon)) {
      lat <- as.numeric(query$lat)
      lon <- as.numeric(query$lon)
      url_lat(lat)
      url_lon(lon)
    }

    # Year
    if (!is.null(query$year)) {
      updateTextInput(session, "canvas_year", value = query$year)
      url_year(as.numeric(query$year))
    }

    # Species
    if (!is.null(query$species)) {
      species_id <- as.numeric(query$species)
      url_species_id(species_id)
    }
    if (!is.null(query$speciesName)) {
      species_name <- URLdecode(query$speciesName)
      updateTextInput(session, "canvas_species", value = species_name)
      url_species_name(species_name)
    }

    # Threshold
    if (!is.null(query$threshold)) {
      threshold <- as.numeric(query$threshold)
      updateTextInput(session, "canvas_threshold", value = threshold)
      url_threshold(threshold)
    }
  }
}