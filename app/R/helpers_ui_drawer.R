# helpers_ui_drawer.R
# ==============================================================================
# UI Drawer Helpers
# ==============================================================================
# This file contains functions to generate the HTML for the right-side sliding 
# stats drawer. 
# ==============================================================================

library(shiny)

# Helper to build a single metric card
metric_card <- function(label, value, accent_class = "") {
  content <- if (inherits(value, "shiny.tag") || inherits(value, "html") || inherits(value, "shiny.tag.list")) {
    value
  } else {
    HTML(value)
  }
  div(
    class = paste("metric-card", accent_class),
    div(class = "metric-label", label),
    div(class = "metric-value", content)
  )
}

# ------------------------------------------------------------------------------
# build_region_stats_cards()
# ------------------------------------------------------------------------------
# Generates the HTML for the region stats cards in the right drawer.
# ------------------------------------------------------------------------------
build_region_stats_cards <- function(region, spatial_level, show_projections, projection_style) {
  if (is.null(region)) return(NULL)

  area_txt <- "N/A"
  if (!is.null(region$area_km2) && !is.na(region$area_km2)) {
    area_txt <- sprintf("%s km\u00b2", format(round(as.numeric(region$area_km2)), big.mark = ",", trim = TRUE))
  }

  parent_txt <- "\u2014"
  if (!is.null(region$parent_zone) && !is.na(region$parent_zone) && region$parent_zone != "null") {
    parent_txt <- region$parent_zone
  }

  # Translate Study Zones for UI clarity if they are bundled inside Bidding Zone maps
  display_level <- region$level
  if (spatial_level == "P2ON" && region$level == "SZON") {
    display_level <- "P2ON (Study Zone)"
  } else if (spatial_level == "P2OF" && region$level == "SZOF") {
    display_level <- "P2OF (Study Zone)"
  }

  cards <- list(
    metric_card("Region Name",    region$name),
    metric_card("Zone ID",        sprintf("<code>%s</code>", region$zone_id), "accent-danger"),
    metric_card("Parent Zone",    sprintf("<code>%s</code>", parent_txt)),
    metric_card("Spatial Tier",   display_level),
    metric_card("Area",           area_txt, "accent-success")
  )

  if (isTRUE(show_projections == "1")) {
    current_val <- if (is.null(projection_style)) "band" else projection_style
    
    proj_style_toggle <- div(
      class = paste0("proj-style-toggle", if (current_val == "spaghetti") " toggle-right" else ""),
      id = "projection-style-toggle-container",
      div(class = "proj-style-pill"),
      tags$button(
        type = "button",
        class = paste0("display-toggle-option", if (current_val == "band") " active" else ""),
        `data-value` = "band",
        "Band"
      ),
      tags$button(
        type = "button",
        class = paste0("display-toggle-option", if (current_val == "spaghetti") " active" else ""),
        `data-value` = "spaghetti",
        "Spaghetti"
      )
    )
    
    toggle_card <- metric_card("Ensemble View", proj_style_toggle, "accent-info")
    
    conditional_toggle <- conditionalPanel(
      condition = "input.drawer_tabs == 'trends'",
      toggle_card
    )
    
    cards <- c(cards, list(conditional_toggle))
  }

  do.call(tagList, cards)
}
