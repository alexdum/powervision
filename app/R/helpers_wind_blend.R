# helpers_wind_blend.R
# ==============================================================================
# Wind Power Blending Engine
# ==============================================================================
# Implements dynamic capacity factor (CF) blending for wind power.
# Copernicus PECD provides unblended capacity factors for individual turbine
# technologies. This engine applies time-varying or fixed weights based on
# regional resource groups to calculate an aggregated "Wind Power" CF.
# It also handles upward spatial aggregation from granular levels (P2ON, P2OF)
# to the national level (NUT0).
# ==============================================================================

#' Interpolate technology weights for a given resource group and year.
#' 
#' @param resource_group Character. The assigned resource group (e.g. "High")
#' @param target_year Numeric. The year to interpolate for (e.g. 2027)
#' @param wind_type Character. "onshore" or "offshore"
#' @return A data.frame with TechCode and Weight.
interpolate_tech_weights <- function(resource_group, target_year, wind_type) {
  # Select the correct matrices based on wind type
  if (wind_type == "onshore") {
    mix_ratios <- onshore_mix_ratios
    existing_tech <- 30 # wind_onshore_30
  } else {
    mix_ratios <- offshore_mix_ratios
    existing_tech <- 20 # wind_offshore_20
  }
  
  # Filter to the requested resource group
  group_mix <- mix_ratios |> dplyr::filter(ResourceGroup == resource_group)
  
  if (nrow(group_mix) == 0) {
    # Fallback to 100% existing fleet if group not found
    return(data.frame(TechCode = existing_tech, Weight = 1.0))
  }
  
  # For years <= 2025, we use the 2025 technology mix directly
  if (target_year <= 2025) {
    weights_df <- data.frame(
      TechCode = group_mix$TechCode,
      Weight = group_mix$Y2025
    )
    if (sum(weights_df$Weight) > 0) {
      weights_df$Weight <- weights_df$Weight / sum(weights_df$Weight)
    }
    return(weights_df)
  }
  
  # Anchor years provided in the CSV (from 2025 onwards)
  anchor_years <- c(2025, 2030, 2040, 2050)
  
  weights_df <- data.frame(TechCode = group_mix$TechCode)
  weights_df$Weight <- numeric(nrow(weights_df))
  
  # Iterate over each technology to interpolate its weight from 2025 to 2050
  for (i in seq_len(nrow(group_mix))) {
    tech_weights_at_anchors <- c(
      group_mix$Y2025[i],
      group_mix$Y2030[i],
      group_mix$Y2040[i],
      group_mix$Y2050[i]
    )
    
    # Linearly interpolate between 2025 and 2050
    interp <- approx(x = anchor_years, y = tech_weights_at_anchors, xout = target_year, rule = 2)
    weights_df$Weight[i] <- interp$y
  }
  
  # Normalize to ensure sum is exactly 1.0
  if (sum(weights_df$Weight) > 0) {
    weights_df$Weight <- weights_df$Weight / sum(weights_df$Weight)
  }
  
  return(weights_df)
}

#' Gets the resource group for a region
get_resource_group <- function(region_id, wind_type) {
  if (wind_type == "onshore") {
    groups <- onshore_resource_groups
  } else {
    groups <- offshore_resource_groups
  }
  
  res <- groups |> dplyr::filter(Region == region_id)
  if (nrow(res) > 0) {
    return(res$ResourceGroup[1])
  } else {
    return("Unknown")
  }
}

#' Format the wind technology mix as a tooltip string (e.g. "66% SP277 HH100, 24% ...")
#' 
#' @param region_id String. The region zone_id
#' @param wind_type String. "onshore" or "offshore"
#' @param target_year Numeric. The anchor year for the mix
#' @return HTML string formatted for tooltips
get_wind_mix_tooltip <- function(region_id, wind_type, target_year) {
  res_grp <- get_resource_group(region_id, wind_type)
  weights_df <- interpolate_tech_weights(res_grp, target_year, wind_type)
  
  # Filter to technologies that actually contribute
  weights_df <- weights_df |> dplyr::filter(Weight > 0.005) # at least 0.5%
  
  if (nrow(weights_df) == 0) return("")
  
  # Join to get TechName
  if (wind_type == "onshore") {
    tech_names <- onshore_mix_ratios |> dplyr::select(TechCode, TechName) |> dplyr::distinct()
  } else {
    tech_names <- offshore_mix_ratios |> dplyr::select(TechCode, TechName) |> dplyr::distinct()
  }
  
  weights_df <- weights_df |> dplyr::left_join(tech_names, by = "TechCode")
  
  # If TechName is NA (e.g. existing fleet), provide a generic name
  weights_df$TechName[is.na(weights_df$TechName)] <- paste("Existing Fleet (Tech", weights_df$TechCode[is.na(weights_df$TechName)], ")")
  
  # Sort descending by weight
  weights_df <- weights_df[order(-weights_df$Weight), ]
  
  # Format string: "66% SP277 HH100, 24% SP335 HH100"
  mix_str <- paste0(round(weights_df$Weight * 100), "% ", weights_df$TechName, collapse = ", ")
  
  html <- paste0(
    "<div style='margin-top: 6px; padding-top: 4px; border-top: 1px solid #334155;'>",
    "  <div style='font-size: 0.75rem; color: #94a3b8; font-weight: 600;'>Resource Group: <span style='color: #cbd5e1; font-weight: 400;'>", res_grp, "</span></div>",
    "  <div style='font-size: 0.7rem; color: #94a3b8; margin-top: 2px;'>", mix_str, "</div>",
    "</div>"
  )
  
  return(html)
}

#' Resolve the actual year for technology mix based on the tech_mix_mode.
#' 
#' @param tech_mix_mode String (e.g. "dynamic", "fixed_2020", "fixed_2030")
#' @param data_year Numeric. The year of the climate data being queried.
#' @return Numeric. The anchor year to use for the tech mix.
resolve_tech_year <- function(tech_mix_mode, data_year) {
  if (tech_mix_mode == "dynamic") {
    return(data_year)
  } else if (tech_mix_mode == "fixed_2020") {
    return(2020)
  } else if (tech_mix_mode == "fixed_2025") {
    return(2025)
  } else if (tech_mix_mode == "fixed_2030") {
    return(2030)
  } else if (tech_mix_mode == "fixed_2040") {
    return(2040)
  } else if (tech_mix_mode == "fixed_2050") {
    return(2050)
  }
  return(data_year) # Fallback
}

#' Get all distinct resource groups and their weights for a given target_year
get_all_group_weights <- function(target_year, wind_type) {
  if (wind_type == "onshore") {
    unique_groups <- unique(onshore_resource_groups$ResourceGroup)
  } else {
    unique_groups <- unique(offshore_resource_groups$ResourceGroup)
  }
  
  group_weights <- list()
  for (grp in unique_groups) {
    group_weights[[grp]] <- interpolate_tech_weights(grp, target_year, wind_type)
  }
  return(group_weights)
}

#' Core blending function for all regions (for map choropleth).
#' Automatically handles NUT0 aggregation if sp_level == "nuts_0".
blend_wind_power_all_regions <- function(
  tech_mix_mode, wind_type, ds_annual, ds_seasonal, ds_monthly,
  temporal_mode, sp_level, year = NULL, year_start = NULL, year_end = NULL,
  target_year = NULL, scenario_val = NULL, target_region = NULL
) {
  
  valid_levels <- if (wind_type == "onshore") c("nuts_0", "p2on", "szon") else c("nuts_0", "p2of", "szof")
  if (!(sp_level %in% valid_levels)) return(NULL)
  
  # For NUT0, we must query the granular data (P2ON or P2OF) and aggregate up
  query_sp_level <- sp_level
  is_nut0_agg <- (sp_level == "nuts_0")
  if (is_nut0_agg) {
    query_sp_level <- ifelse(wind_type == "onshore", "p2on", "p2of")
  }
  
  # Determine the technology year for the weights
  # If it's a range (baseline), we use the midpoint for dynamic, or just stick to the mode
  target_data_year <- ifelse(!is.null(year), year, 
                             ifelse(!is.null(year_start) && !is.null(year_end), floor((year_start + year_end) / 2), 2020))
  
  tech_year <- resolve_tech_year(tech_mix_mode, target_data_year)
  
  # Get group weights for the target tech year
  group_weights <- get_all_group_weights(tech_year, wind_type)
  
  # Gather all distinct tech codes we need to query
  all_techs <- unique(unlist(lapply(group_weights, function(w) w$TechCode[w$Weight > 0])))
  
  if (length(all_techs) == 0) return(NULL)
  
  # For NUT0, we must load all granular regions to perform area-weighted aggregation.
  # Otherwise we can push target_region down to Arrow to speed up queries.
  query_target_region <- if (is_nut0_agg) NULL else target_region
  
  # Query the data for all needed tech variables
  blended_results <- NULL
  
  for (tech_code in all_techs) {
    var_name <- paste0("wind_", wind_type, "_", tech_code)
    
    tech_data <- query_arrow_dataset(
      ds_annual = ds_annual,
      ds_seasonal = ds_seasonal,
      ds_monthly = ds_monthly,
      temporal_mode = temporal_mode,
      var_name = var_name,
      sp_level = query_sp_level,
      year = year,
      year_start = year_start,
      year_end = year_end,
      scenario_val = scenario_val,
      target_region = query_target_region,
      select_cols = c("Region", "Value", "Year", "scenario", "model")
    )
    
    if (is.null(tech_data)) next
    
    # We may have multiple years (baseline) or models (projections)
    # We first group by Region (and Year/Model if present) to apply weights
    tech_data$TechCode <- tech_code
    
    if (is.null(blended_results)) {
      blended_results <- tech_data
    } else {
      blended_results <- dplyr::bind_rows(blended_results, tech_data)
    }
  }
  
  if (is.null(blended_results)) return(NULL)
  
  # Join with resource groups mapping
  groups_map <- if (wind_type == "onshore") onshore_resource_groups else offshore_resource_groups
  blended_results <- blended_results |>
    dplyr::left_join(groups_map, by = "Region") |>
    dplyr::mutate(ResourceGroup = ifelse(is.na(ResourceGroup), "Unknown", ResourceGroup))
  
  # Flatten group_weights list into a lookup dataframe
  weights_flat <- do.call(rbind, lapply(names(group_weights), function(grp) {
    df <- group_weights[[grp]]
    if (nrow(df) > 0) df$ResourceGroup <- grp
    df
  }))
  
  # Join weights
  blended_results <- blended_results |>
    dplyr::left_join(weights_flat, by = c("ResourceGroup", "TechCode")) |>
    dplyr::mutate(Weight = ifelse(is.na(Weight), 0, Weight))
  
  # Compute weighted value
  blended_results$WeightedValue <- blended_results$Value * blended_results$Weight
  
  # Aggregate by Region (and other dimensions)
  group_cols <- c("Region", "ResourceGroup")
  if ("Year" %in% names(blended_results)) group_cols <- c(group_cols, "Year")
  if ("scenario" %in% names(blended_results)) group_cols <- c(group_cols, "scenario")
  if ("model" %in% names(blended_results)) group_cols <- c(group_cols, "model")
  
  final_blended <- blended_results |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarize(
      Value = if (all(is.na(WeightedValue))) NA_real_ else sum(WeightedValue, na.rm = TRUE) / sum(Weight[!is.na(WeightedValue)]),
      .groups = "drop"
    )
  
  # --- NUT0 Aggregation ---
  if (is_nut0_agg) {
    # Extract area weights from geojson
    sf_layer_name <- ifelse(wind_type == "onshore", "P2ON", "P2OF")
    sf_data <- spatial_boundary_cache[[sf_layer_name]]
    
    if (!is.null(sf_data)) {
      # Extract NUT0 code from region ID (first 2 chars)
      areas_df <- sf_data |> 
        sf::st_drop_geometry() |> 
        dplyr::select(zone_id, area_km2) |>
        dplyr::rename(Region = zone_id)
      
      final_blended <- final_blended |>
        dplyr::left_join(areas_df, by = "Region") |>
        dplyr::mutate(
          NUT0 = substr(Region, 1, 2),
          area_km2 = ifelse(is.na(area_km2), 1, area_km2) # Fallback to unweighted if missing
        )
      
      # Now group by NUT0 instead of Region
      agg_cols <- setdiff(group_cols, c("Region", "ResourceGroup"))
      agg_cols <- c("NUT0", agg_cols)
      
      final_blended <- final_blended |>
        dplyr::group_by(dplyr::across(dplyr::all_of(agg_cols))) |>
        dplyr::summarize(
          Value = sum(Value * area_km2, na.rm = TRUE) / sum(area_km2, na.rm = TRUE),
          .groups = "drop"
        ) |>
        dplyr::rename(Region = NUT0) |>
        dplyr::mutate(ResourceGroup = "Aggregated") # NUTS0 loses granular group
    }
  }
  
  return(as.data.frame(final_blended))
}

#' Time-series blending for a single region (handles dynamic year-by-year weights).
blend_wind_power_timeseries <- function(region_id, tech_mix_mode, wind_type, ds_annual, ds_seasonal, ds_monthly,
                                        temporal_mode, sp_level,
                                        scenario_val = NULL) {
  
  valid_levels <- if (wind_type == "onshore") c("nuts_0", "p2on", "szon") else c("nuts_0", "p2of", "szof")
  if (!(sp_level %in% valid_levels)) return(NULL)

  is_nut0_agg <- (sp_level == "nuts_0")
  query_sp_level <- if(is_nut0_agg) ifelse(wind_type == "onshore", "p2on", "p2of") else sp_level
  
  # For NUT0, we must fetch ALL regions in that country, blend them dynamically, and aggregate
  if (is_nut0_agg) {
    sf_layer_name <- ifelse(wind_type == "onshore", "P2ON", "P2OF")
    sf_data <- spatial_boundary_cache[[sf_layer_name]]
    if (is.null(sf_data)) return(NULL)
    
    sub_regions_df <- sf_data |> sf::st_drop_geometry() |> dplyr::select(zone_id, area_km2)
    # Filter to regions whose ID starts with the NUT0 code
    sub_regions_df <- sub_regions_df[substr(sub_regions_df$zone_id, 1, 2) == region_id, ]
    target_regions <- sub_regions_df$zone_id
  } else {
    target_regions <- region_id
  }
  
  if (length(target_regions) == 0) return(NULL)
  
  # Pre-fetch all possible tech codes for the wind_type to avoid repeated tiny queries
  if (wind_type == "onshore") {
    all_techs <- unique(onshore_mix_ratios$TechCode)
    all_techs <- unique(c(all_techs, 30))
  } else {
    all_techs <- unique(offshore_mix_ratios$TechCode)
    all_techs <- unique(c(all_techs, 20))
  }
  
  # Fetch data for all required regions and techs
  raw_data_list <- list()
  for (tech_code in all_techs) {
    var_name <- paste0("wind_", wind_type, "_", tech_code)
    
    # query_arrow_dataset doesn't natively accept a vector of regions, so we loop if needed
    # Actually, the query_arrow_dataset takes target_region=NULL to get all, then we filter in memory
    tech_data <- query_arrow_dataset(
      ds_annual = ds_annual,
      ds_seasonal = ds_seasonal,
      ds_monthly = ds_monthly,
      temporal_mode = temporal_mode,
      var_name = var_name,
      sp_level = query_sp_level,
      scenario_val = scenario_val,
      select_cols = c("Region", "Value", "Year", "Month", "scenario", "model")
    )
    if (!is.null(tech_data)) {
      tech_data <- tech_data[tech_data$Region %in% target_regions, ]
      if (nrow(tech_data) > 0) {
        tech_data$TechCode <- tech_code
        raw_data_list[[length(raw_data_list) + 1]] <- tech_data
      }
    }
  }
  
  if (length(raw_data_list) == 0) return(NULL)
  raw_data <- dplyr::bind_rows(raw_data_list)
  
  # Now we have all the raw data. We need to apply weights year-by-year.
  unique_years <- unique(raw_data$Year)
  groups_map <- if (wind_type == "onshore") onshore_resource_groups else offshore_resource_groups
  
  blended_by_year <- list()
  
  for (yr in unique_years) {
    tech_year <- resolve_tech_year(tech_mix_mode, yr)
    
    # Filter data for this year
    yr_data <- raw_data[raw_data$Year == yr, ]
    
    # Get weights for all groups for this tech_year
    group_weights <- get_all_group_weights(tech_year, wind_type)
    weights_flat <- do.call(rbind, lapply(names(group_weights), function(grp) {
      df <- group_weights[[grp]]
      if (nrow(df) > 0) df$ResourceGroup <- grp
      df
    }))
    
    yr_data <- yr_data |>
      dplyr::left_join(groups_map, by = "Region") |>
      dplyr::mutate(ResourceGroup = ifelse(is.na(ResourceGroup), "Unknown", ResourceGroup)) |>
      dplyr::left_join(weights_flat, by = c("ResourceGroup", "TechCode")) |>
      dplyr::mutate(Weight = ifelse(is.na(Weight), 0, Weight)) |>
      dplyr::mutate(WeightedValue = Value * Weight)
    
    # Aggregate to region level, preserving Month if present (for seasonality boxplots)
    grp_cols <- c("Region", "Year")
    if ("Month" %in% names(yr_data)) grp_cols <- c(grp_cols, "Month")
    if ("scenario" %in% names(yr_data)) grp_cols <- c(grp_cols, "scenario")
    if ("model" %in% names(yr_data)) grp_cols <- c(grp_cols, "model")
    
    reg_agg <- yr_data |>
      dplyr::group_by(dplyr::across(dplyr::all_of(grp_cols))) |>
      dplyr::summarize(
        Value = if (all(is.na(WeightedValue))) NA_real_ else sum(WeightedValue, na.rm = TRUE) / sum(Weight[!is.na(WeightedValue)]),
        .groups = "drop"
      )
    
    blended_by_year[[length(blended_by_year) + 1]] <- reg_agg
  }
  
  final_blended <- dplyr::bind_rows(blended_by_year)
  
  # Perform NUT0 aggregation if needed
  if (is_nut0_agg) {
    final_blended <- final_blended |>
      dplyr::left_join(sub_regions_df, by = c("Region" = "zone_id")) |>
      dplyr::mutate(area_km2 = ifelse(is.na(area_km2), 1, area_km2))
    
    agg_cols <- c("Year")
    if ("Month" %in% names(final_blended)) agg_cols <- c(agg_cols, "Month")
    if ("scenario" %in% names(final_blended)) agg_cols <- c(agg_cols, "scenario")
    if ("model" %in% names(final_blended)) agg_cols <- c(agg_cols, "model")
    
    final_blended <- final_blended |>
      dplyr::group_by(dplyr::across(dplyr::all_of(agg_cols))) |>
      dplyr::summarize(
        Value = sum(Value * area_km2, na.rm = TRUE) / sum(area_km2, na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::mutate(Region = region_id)
  }
  
  return(as.data.frame(final_blended))
}
