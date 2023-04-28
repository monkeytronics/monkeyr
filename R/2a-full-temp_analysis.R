#' get_reg_data
#'
#' @description Make Data-Frame of Regression Test:
#'   for use with indoor - outdoor regression testing
#'
#' @param wrangled_devices from `wrangle_observations( )`
#' @param wrangled_obs from `wrangled_obs( )`
#' @param target_reading variable upon which to segment data
#'
#' @returns data frame
#'
#' @examples
#' reg_data  <- get_reg_data(wrangled_devices, wrangled_obs, target_reading = "temp")
#' reg_kable <- get_reg_kable(reg_data, target_var)
#'
#' @export
get_reg_data <- function(wrangled_devices, wrangled_obs, target_reading = "temp") {
  tryCatch (
    {
      ## Wrangle Obs for Temperature Regression Tests
      regression <- wrangled_obs %>%
        filter(!is.na(outdoor_temp)) %>%
        filter(reading == target_reading) %>%
        filter(as.integer(hour) %in% c(23, 0:7))
        # filter(as.integer(hour) %in% c(20:23, 0:7))

      ## Devices Present in Analysis
      deviceList <-
        regression %>%
        select(device_id) %>%
        unique() %>%
        unlist() %>%
        unname()

      ## Initialise Empty Object
      regression_local <- c()

      ## For Each device in regression, do {temp ~ outdoor_temp} & push into temp obj
      for (dev in deviceList) {
        subset <- regression %>%
          filter(device_id == dev)
        model <- lm(val ~ outdoor_temp, data = subset, na.action=na.exclude)
        regression_local <- rbind(regression_local, pixiedust::dust(model) %>% as_tibble())
      }

      ## Restore regression, Format & Add Significance
      regression <-
        regression_local %>%
        filter(term != "(Intercept)") %>%
        tibble::add_column(device_id = deviceList, .before = "term") %>%
        mutate_at(c("estimate", "p.value"), as.numeric) %>%

        mutate(estimate = round(estimate, digits = 4),
               significance = case_when(p.value < 1 & p.value >= 0.1 ~ " ",
                                        p.value < 0.1 & p.value >= 0.05 ~ ".",
                                        p.value < 0.05 & p.value >= 0.01 ~ "\\*",
                                        p.value < 0.01 & p.value >= 0.001 ~ "**",
                                        p.value < 0.001 ~ "***"),
               p.value = pixiedust::pvalString(p.value, format = "exact", digits = 3)) %>%

        select(-c(std.error, statistic)) %>%

        ## Add back in Device Cols which lm() removed
        left_join(wrangled_devices, by = "device_id") %>%
        dplyr::filter(!is.na(estimate))

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "get_reg_data")
    }
  )
}



#' get_reg_kable
#'
#' @description Make Kable from Inside-Outside Regression Data that shows detailed
#' device's model relationship coefficient.
#'
#' @param reg_data from `get_reg_data`
#' @param target_var variable upon which to segment kable
#'
#' @returns kable
#'
#' @examples
#' reg_data  <- get_reg_data(wrangled_devices, wrangled_obs, target_reading = "temp")
#' reg_kable <- get_reg_kable(reg_data, target_var)
#'
#' @export
get_reg_kable <- function(reg_data, target_var) {
  tryCatch (
    {
      ## Create Local var Regression for current variable - Leave original unchanged!
      reg_data_local <-
        reg_data %>%
        dplyr::rename(target_var := {target_var}) %>%
        ## Want user to be aware of data that they are not using properly.
        tidyr::replace_na(list(target_var = "unknown"))

      # ## Rename column (base R method)
      # names(regression)[names(regression) == target_var <- "target_var"

      target_var_Title <- gsub('_',' ',target_var)
      target_var_Title <- stringi::stri_trans_totitle(target_var_Title)


      ## Kable to Print
      reg_kable <-
        reg_data_local %>%

        ## Keep necessary cols
        select(city, target_var, device_id, estimate) %>%
        # relocate(city, .after = hhi) %>%

        ## Arrange so worst estimates at top of table
        arrange(`city`, `estimate`) %>%

        ## prettify with kableExtra
        kable(col.names = c("City", target_var_Title, "Device Id", "Coefficient")) %>%
        kableExtra::kable_material(lightable_options = c("striped", "hover", "condensed", "responsive"),
                       html_font = "sans-serif") %>%

        ## Remove repeated hhi provider name
        collapse_rows(columns = 1, valign = "top") %>%
        scroll_box(height = "350px")

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "get_reg_kable")
    }
  )
}



#' get_reg_boxplot
#'
#' @description Make Box Plot from Inside-Outside Regression Data showing results
#' segmented by the target variable.
#'
#' @param reg_data from `get_reg_data`
#' @param target_var variable upon which to segment kable
#'
#' @returns Box Plot
#'
#' @examples
#' reg_data  <- get_reg_data(wrangled_devices, wrangled_obs, target_reading = "temp")
#' reg_kable <- get_reg_kable(reg_data, target_var)
#'
#' @export
get_reg_boxplot <- function(reg_data, target_var) {
  tryCatch (
    {
      ## Create Local var Regression for current variable - Leave original unchanged!
      reg_data_local <-
        reg_data %>%
        dplyr::rename(target_var := {target_var}) %>%
        ## Want user to be aware of data that they are not using properly.
        tidyr::replace_na(list(target_var = "unknown"))


      ## Create New target_data group = "All"
      reg_all <-
        reg_data_local %>%
        mutate(target_var := "All")

      ## Join "All" back in
      reg_joined <-
        full_join(reg_data_local, reg_all)

      ## the n sample size in title of plots
      n <- nrow(reg_joined) / 2

      ## https://stackoverflow.com/questions/13370164/how-to-display-the-median-value-in-a-boxplot-in-ggplot
      dataMedian <- summarise(group_by(reg_joined, target_var), MD = round(median(estimate), digits = 2))


      reg_boxplot <-
        reg_joined %>%
        ggplot2::ggplot( ggplot2::aes(x = estimate, y = target_var)) +
        geom_vline(xintercept = 1.0, show.legend = TRUE, colour = "red") +
        # geom_text(label = "Excellent", x = 0.15, y = 0.5) +
        geom_vline(xintercept = 0.7, show.legend = TRUE, colour = "orange") +
        # geom_text(label = "Average", x = 0.15, y = 0.7) +
        geom_vline(xintercept = 0.0, show.legend = TRUE, colour = "green") +
        # geom_text(label = "Very Poor", x = 0.85, y = 0.5) +
        geom_boxplot(fill = "blue", alpha = 0.15, outlier.shape = NA) +
        scale_x_continuous(limits = c(quantile(reg_joined$estimate,0.01), quantile(reg_joined$estimate,0.99))) +

        geom_text(data = dataMedian, ggplot2::aes(MD, target_var, label = MD),
                  position = position_dodge(width = 0.8), size = 3, hjust = -1.0) +

        # geom_jitter(width = 0.1, shape = 21, size = 1, stroke = 1) +
        # geom_jitter(width = 0.1, shape = 21, size = 1, stroke = 1, ggplot2::aes(colour = p.value)) +
        # scale_color_gradient(name = "P Value")

        theme_light() +
        labs(x = "Model Relationship Coefficient", y = target_var) +

        hrbrthemes::theme_ipsum_ps() #+

        #theme(plot.margin = margin(0.0, 0.0, 0.0, 0.0, "cm"))#,

              # strip.text.x = element_text(angle = 0, hjust = 0.3),
              #
              # legend.title = element_text(size = 9),
              # legend.position = "right",
              #
              # axis.title.x = element_blank(),
              # axis.text.x = element_blank(),
              # axis.text.y = element_blank(),
              #
              # panel.grid.major = element_blank(),
              # panel.grid.minor = element_blank())

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "get_reg_boxplot")
    }
  )
}



#' get_temp_exposure
#'
#' @description Generate Temperature Exposure data-set used to build kables that
#' show percentage of time spent at night below WHO thresholds.
#'
#' @param wrangled_obs from `wrangled_obs`
#' @param wrangled_devices from `wrangled_devices`
#' @param target_var variable upon which to segment kable
#'
#' @returns Data.Frame
#'
#' @examples
#' temp_exposure      <- get_temp_exposure(wrangled_obs, wrangled_devices, target_var)
#' exposure_kable_var <- get_exposure_kable_var(temp_exposure, target_var)
#' exposure_kable_dev <- get_exposure_kable_dev(temp_exposure, target_var)
#'
#' @export
get_temp_exposure <- function(wrangled_obs, wrangled_devices, target_var) {
  tryCatch (
    {

      ## Based on Home Report : WHO Threshold Function
      temp_exposure <-
        monkeyr::who_threshold_data(observations = wrangled_obs) %>%

        ## Group all > 21
        mutate(temp_band = ifelse(temp_band == "< 29°C", "> 21°C", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "> 29°C", "> 21°C", temp_band)) %>%

        ## Sum fraction > 21
        group_by(device_id, temp_band) %>%
        summarise(fraction = sum(fraction)) %>%
        ungroup() %>%

        ## Spread - Replaced by Pivot Wider
        tidyr::pivot_wider(names_from = temp_band, values_from = fraction) %>%

        ## Add back in Device Info
        left_join(wrangled_devices, by = "device_id")

        temp_exposure <-
          temp_exposure %>%
          dplyr::rename(target_var := {target_var}) %>%
          ## Want user to be aware of data that they are not using properly.
          tidyr::replace_na(list(target_var = "unknown")) %>%
          select(c(target_var, device_id, "< 21°C", "< 18°C", "< 16°C", "< 12°C")) %>%
          mutate(`< 16°C` = `< 16°C` + `< 12°C`) %>%
          mutate(`< 18°C` = `< 18°C` + `< 16°C`) %>%
          mutate(`< 21°C` = `< 21°C` + `< 18°C`)

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "get_temp_exposure")
    }
  )
}



#' get_exposure_kable_var
#'
#' @description Using temp_exposure data-set, generate a neat Kable showing
#' temperature exposure groups by a target var.
#'
#' @param temp_exposure from `get_temp_exposure`
#' @param target_var variable upon which to segment kable
#'
#' @returns Kable
#'
#' @examples
#' temp_exposure      <- get_temp_exposure(wrangled_obs, wrangled_devices, target_var)
#' exposure_kable_var <- get_exposure_kable_var(temp_exposure, target_var)
#' exposure_kable_dev <- get_exposure_kable_dev(temp_exposure, target_var)
#'
#' @export
get_exposure_kable_var <- function(temp_exposure, target_var) {
  tryCatch (
    {
      exposure_kable_var <-
        temp_exposure %>%
        select(-device_id) %>%
        group_by(target_var) %>%
        summarise_all(.funs = c(mean="mean")) %>%
        # arrange(-`< 21°C_mean`, -`< 18°C_mean`, -`< 16°C_mean`, -`< 12°C_mean`) %>%
        arrange(-`< 12°C_mean`, -`< 16°C_mean`, -`< 18°C_mean`, -`< 21°C_mean`) %>%
        janitor::adorn_pct_formatting() %>%
        kable(col.names = c(stringi::stri_trans_totitle(target_var), "< 21°C", "< 18°C", "< 16°C", "< 12°C")) %>%
        kable_material(lightable_options = c("striped", "hover", "condensed"), html_font = "sans-serif") #%>%
        # collapse_rows(columns = 1:2, valign = "middle")

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "get_exposure_kable_var")
    }
  )
}



#' get_exposure_kable_dev
#'
#' @description Using temp_exposure data-set, generate a neat Kable showing
#' temperature exposure for all devices individually.
#'
#' @param temp_exposure from `get_temp_exposure`
#' @param target_var variable upon which to segment kable
#'
#' @returns Kable
#'
#' @examples
#' temp_exposure      <- get_temp_exposure(wrangled_obs, wrangled_devices, target_var)
#' exposure_kable_var <- get_exposure_kable_var(temp_exposure, target_var)
#' exposure_kable_dev <- get_exposure_kable_dev(temp_exposure, target_var)
#'
#' @export
get_exposure_kable_dev <- function(temp_exposure, target_var) {
  tryCatch (
    {
      target_var_Title <- gsub('_',' ',target_var)
      target_var_Title <- stringi::stri_trans_totitle(target_var_Title)

      exposure_kable_dev <-
        temp_exposure %>%
        relocate(device_id) %>%
        group_by(target_var, device_id)  %>%
        # arrange(desc(`< 21°C`)) %>%
        # arrange(-`< 21°C`, -`< 18°C`, -`< 16°C`, -`< 12°C`) %>%
        arrange(-`< 12°C`, -`< 16°C`, -`< 18°C`, -`< 21°C`) %>%
        janitor::adorn_pct_formatting() %>%
        kable(col.names = c("Device Id", target_var_Title, "< 21°C", "< 18°C", "< 16°C", "< 12°C")) %>%
        kable_material(lightable_options = c("striped", "hover", "condensed"), html_font = "sans-serif")

      if (nrow(temp_exposure) > 5) {
        exposure_kable_dev <- exposure_kable_dev %>%
          scroll_box(height = "400px")
      }

      ## output
      exposure_kable_dev

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "get_exposure_kable_dev")
    }
  )
}



#' get_exposure_chart_data
#'
#' @description Starting from observations wrangled for use testing against WHO
#' thresholds, this function massages the data into the requisiste format for the
#' get_exposure_plot function.
#'
#' @param wrangled_obs from `wrangled_obs`
#' @param wrangled_devices from `wrangled_devices`
#' @param target_var variable upon which to segment kable
#'
#' @returns Data.Frame
#'
#' @examples
#' exposure_chart_data <- get_exposure_chart_data(wrangled_obs, wrangled_devices, target_var)
#' exposure_plot <- get_exposure_plot(exposure_chart_data, target_var)
#'
#'# target_var = "room_type" (for test)
#'
#' @export
get_exposure_chart_data <- function(wrangled_obs, wrangled_devices, target_var) {
  tryCatch (
    {
      ## Based on Home Report : WHO Threshold Function
      temp_exposure <-
        monkeyr::who_threshold_data(observations = wrangled_obs) %>%

        ## Mutate temp_band to number (>21=0,<21=1,<18=2...)
        mutate(temp_band = ifelse(temp_band == "< 29°C", "0", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "> 29°C", "0", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 21°C", "1", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 18°C", "2", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 16°C", "3", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 12°C", "4", temp_band)) %>%

        ## Sum fraction > 21
        group_by(device_id, temp_band) %>%
        summarise(fraction = sum(fraction)) %>%
        ungroup() %>%

        ## Add back in Device Info
        left_join(wrangled_devices, by = "device_id") %>%

        ## Rename target_var
        dplyr::rename(target_var := {target_var}) %>%
        relocate(target_var, .after = fraction) %>%
        ## Want user to be aware of data that they are not using properly.
        tidyr::replace_na(list(target_var = "unknown", fraction = 0))

      ## Create "all" group
      temp_exposure_all <-
        temp_exposure %>%
        mutate(target_var := "All")

      ## Join "All" back in
      exposure_chart_data <-
        full_join(temp_exposure, temp_exposure_all)


      ## UNIQUE CODE BELOW HERE
      ## ----------------------

      ## Group & Get Mean Vals
      exposure_chart_data <-
        exposure_chart_data %>%
        group_by(target_var, temp_band) %>%
        summarise_at(vars(fraction), mean)

      ## Now Create Rectangles for plot & Add labels
      exposure_chart_data <-
        exposure_chart_data %>%
        group_by(target_var) %>%
        mutate(ymax = cumsum(fraction),
               ymin = c(0, head(cumsum(fraction), n = -1))) %>%
        mutate(label_pos = (ymax+ymin) / 2,
               label = ifelse(fraction < 0.01, "", paste0(round(fraction*100, 0), "%")))

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "get_exposure_chart_data")
    }
  )
}




#' get_exposure_plot
#'
#' @description Takes well formatted data and outputs exposure plot against
#' WHO thresholds - is currently used with :
#' * temperature exposure analysis
#' * lowest temperature experienced
#'
#' @param exposure_chart_data from `get_exposure_chart_data` for example
#' @param target_var variable upon which to segment kable
#' @param chart_type "bar" or "donut" - defaults to "bar"
#'
#' @returns Data.Frame
#'
#' @examples
#' exposure_chart_data <- get_exposure_chart_data(wrangled_obs, wrangled_devices, target_var)
#' exposure_plot <- get_exposure_plot(exposure_chart_data, target_var)
#'
#' lowtemp_chart_data <- get_lowtemp_chart_data(wrangled_obs, wrangled_devices, target_var)
#' lowtemp_chart      <- get_exposure_plot(lowtemp_chart_data, target_var)
#'
#' @export
get_exposure_plot <- function(exposure_chart_data, target_var, chart_type = "bar") {
  tryCatch (
    {

      # facet_columns <- min(4, length(unique(exposure_chart_data[["target_var"]])))
      facet_rows    <- ceiling(length(unique(exposure_chart_data[["target_var"]])) / 4)
      aspect_ratio  <- 2.5

      if (chart_type == "donut") {
        facet_rows <- ceiling(length(unique(exposure_chart_data[["target_var"]])) / 2)
        aspect_ratio  <- 1.0
      }

      plot <-
        exposure_chart_data %>%
        ggplot(ggplot2::aes(ymax = ymax, ymin = ymin,
                   xmax = 2, xmin = 1,
                   fill = as.factor(temp_band))) +
        geom_rect() +
        geom_text(x = 2.75,
                  ggplot2::aes(y = label_pos,
                      label = ifelse(label == "0%","",label)),
                  size = 2.5) +

        nord::scale_fill_nord("lumina", # moose_pond
                              name = "night-time\ntemperature\n",
                              label = c("> 20°C", "< 20°C", "< 18°C", "< 16°C", "< 12°C"),
                              drop = FALSE) +
        nord::scale_color_nord("lumina", # moose_pond
                               drop = FALSE) +

        guides(color = "none",
               fill = guide_legend(title.position = "top"))

      if(chart_type =="donut"){
        plot <- plot + coord_polar(theta="y")   ## Pie Chart!
      }

      plot2 <- plot +
        xlim(c(0, 3)) +
        ylim(c(1, 0)) + # reversing so lower temps are at bottom.

        facet_wrap( vars( target_var ), nrow = facet_rows, ) + ## ncol = facet_columns ) +
        #facet_grid(cols = vars(room_type), rows = vars(hhi, rental_type)) +

        labs(title = paste0(""), y = "", x = "") +
        theme_void()+

        hrbrthemes::theme_ipsum_ps() +

        theme(plot.margin = margin(0.0, 0.0, 0.0, 0.0, "cm"),

              strip.text.x = element_text(angle = 0, hjust = 0.3, size = 9),

              legend.title = element_text(size = 9),
              legend.position = "right",

              axis.title.x = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_blank(),

              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),

              aspect.ratio = aspect_ratio)


    # Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "get_exposure_plot")
    }
  )
}


 #' get_lowtemp_chart_data
#'
#' @description Starting from observations wrangled for use testing against WHO
#' thresholds, this function massages the data into the requisiste format for the
#' get_exposure_plot function.
#'
#' @param wrangled_obs from `wrangled_obs`
#' @param wrangled_devices from `wrangled_devices`
#' @param target_var variable upon which to segment kable
#'
#' @returns Data.Frame
#'
#' @examples
#' lowtemp_chart_data <- get_lowtemp_chart_data(wrangled_obs, wrangled_devices, target_var)
#' lowtemp_chart      <- get_exposure_plot(lowtemp_chart_data, target_var)
#'
#' @export
get_lowtemp_chart_data <- function(wrangled_obs, wrangled_devices, target_var) {
  tryCatch (
    {
      ## Based on Home Report : WHO Threshold Function
      temp_exposure <-
        monkeyr::who_threshold_data(observations = wrangled_obs) %>%

        ## Mutate temp_band to number (>21=0,<21=1,<18=2...)
        mutate(temp_band = ifelse(temp_band == "< 29°C", "0", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "> 29°C", "0", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 21°C", "1", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 18°C", "2", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 16°C", "3", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 12°C", "4", temp_band)) %>%

        ## Sum fraction > 21
        group_by(device_id, temp_band) %>%
        summarise(fraction = sum(fraction)) %>%
        ungroup() %>%

        ## Add back in Device Info
        left_join(wrangled_devices, by = "device_id") %>%

        ## Rename target_var
        dplyr::rename(target_var := {target_var}) %>%
        ## Want user to be aware of data that they are not using properly.
        tidyr::replace_na(list(target_var = "unknown", fraction = 0))

      ## Create "all" group
      temp_exposure_all <-
        temp_exposure %>%
        mutate(target_var := "All")

      ## Join "All" back in
      lowest_temp_chart_data <-
        full_join(temp_exposure, temp_exposure_all)


      ## UNIQUE CODE BELOW HERE
      ## ----------------------

      ## Flag Worst temp_band
      lowest_temp_chart_data <-
        lowest_temp_chart_data %>%
        relocate(target_var, .after = device_id) %>%

        ## assign number to temp bands in use - not in use NA
        group_by(target_var, device_id) %>%
        mutate(worst_band = ifelse(fraction > 0.0, temp_band, NA)) %>%

        ## Highest in use denotes the worst case
        mutate(worst_band = ifelse(worst_band == max(worst_band, na.rm=TRUE), 1, 0)) %>%

        ## Restore NA values
        tidyr::replace_na(list(worst_band = 0)) %>%
        relocate(worst_band, .after = temp_band) %>%

        ## fraction by band grouped by target var
        ungroup() %>%
        group_by(target_var, temp_band) %>%
        summarise(fraction = sum(worst_band)/n())


      ## Now Create Rectangles for plot & Add labels
      lowest_temp_chart_data <-
        lowest_temp_chart_data %>%
        group_by(target_var) %>%
        mutate(ymax = cumsum(fraction),
               ymin = c(0, head(cumsum(fraction), n = -1))) %>%
        mutate(label_pos = (ymax+ymin) / 2,
               label = ifelse(fraction < 0.005, "", paste0(round(fraction*100, 0), "%")))

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "get_lowtemp_chart_data")
    }
  )
}

#' Get Intervention Sample
#'
#' A function to filter observations based on several parameters
#'
#' @param wrangled_obs_interv from `wrangle_interv`
#' @param inteverntion_data a data frame with all the interventions to be evaluated
#' @param interventions_only a logical argument about whether observations should be restricted to homes with interventions
#' @param hours_greater_than an integer indicating night time start
#' @param hours_less_than an integer indicating night time end
#' @param temps_greater_than an integer indicating the lowest outdoor temp
#' @param temps_less_than an integer indicating the highest outdoor temp
#'
#' @return a data frame of filtered observations
#'
#' @export
get_intervention_sample <- function(
    wrangled_obs_interv
    ,intervention_data
    ,interventions_only
    ,hours_greater_than
    ,hours_less_than
    ,temps_greater_than
    ,temps_less_than) {

  temp <- wrangled_obs_interv %>%
    filter(
      (as.numeric(hour) >= hours_greater_than | as.numeric(hour) <= hours_less_than )
      & (outdoor_temp >= temps_greater_than & outdoor_temp <= temps_less_than)
      )

  if(interventions_only) {
    temp <- temp %>%
      filter( device_id %in% intervention_data$device_id)
  }

  return(temp)
}

#' get_clean_model_data
#'
#' @description We need clean data for the model. This function will join the joined obs
#' and interv data with the device and weather data. After those files are all joined, the
#' function will then clean the data and keep only the data-points we need. A list of
#' two items will be returned. The Output Variable, and the cleaned data
#'
#' @param wrangled_obs_interv from `wrangle_interv`
#' @param model_vars a vector of string input variables/predictors by name
#' @param output_var a string indicating the column name of the output/predicted variable
#'
#' @returns List
#' @export
get_clean_model_data <- function(wrangled_obs_interv, model_vars, output_var){

  temp <- wrangled_obs_interv %>%
    ## select what we need
    select( any_of( c( output_var, model_vars ) ) ) %>%
    mutate(room = if_else(grepl("Bedroom", room), "Bedroom", room)) ## reduce variation over bedroom 1, 2,3, etc.

  outlist <- list(output_var, temp)

  return(outlist)

}


#' get_model
#'
#' @description This function will get the model object so we can get items like the coefficients and
#' residuals.
#'
#' @param clean_data from `get_clean_model_data
#'
#' @returns model_object
#' @export
get_model <- function(clean_data_list){

  output_var <- clean_data_list[[1]]
  clean_data <- clean_data_list[[2]]

  fit_the_model <- function(clean_data, output){
    fm <- glue::glue("{output} ~ .") #not standard eval
    fit_lin <- lm(fm, data = clean_data)
    return(fit_lin)
  }

  model_object <- fit_the_model(clean_data, output_var)

  return(model_object)
}


#' get_model_coef
#'
#' @description This function will get the model object and return the coefficients of the model
#'
#' @param model_object from `get_model`
#'
#' @returns Data.Frame
#'
#'@export
get_model_coef <- function(model){
  model_summary <- summary(model)

  coefs <- model_summary$coefficients

  return(coefs)
}

#' get_model_rsquared
#'
#' @description This function will get the model object and return the r-squared and adjusted
#' r-squared as a list.
#'
#' @param model_object from `get_model`
#'
#' @returns list
#'
#'@export
get_model_rsquared <- function(model){
  model <- model
  rsqr <- summary(model)$r.squared
  adj_rsqr <- summary(model)$adj.r.squred

  out_list <- list(rsqr, adj_rsqr)

  return(out_list)
}

#' get_model_residuals
#'
#' @description This function will get the model object and return the output variable indoor temp
#' and the residuals in a data frame
#'
#' @param model_object from `get_model`
#' @param clean_data  from `get_clean_model_data`
#'
#' @returns Data.Frame
#'
#'@export
get_model_residuals <- function(model,clean_data){
  model <- model
  residuals <- as.list(summary(model)$residuals)


  val <- clean_data$val

  out_df <- data.frame(unlist(val), unlist(residuals))
  names(out_df) = c("val","Residual")
  return(out_df)
}


#' test_for_hetero
#'
#' @description This function will run a test for heteroscedasticity within the data.
#'
#' @param model_object from `get_model`
#'
#' @returns test results
#'
#'@export
bp_results <- function(model_object){

  results <- bptest(model_object)
  return(results)
}

#' get_hetero_model
#'
#' @description This function will remake the model, with weights that will help fix the heteroscedasticity.
#'
#' @param clean_model_data from `get_clean_model_data`
#' @param model_object from `get_model`
#' @param output_var from `get_clean_model_data`
#'
#' @returns model_object
#' @export
get_hetero_model <- function(clean_model_data, model_object, output){

  wt <- 1 / lm(abs(model_object$residuals) ~ model_object$fitted.values)$fitted.values^2
  fm <- glue::glue("{output} ~ .") #not standard eval
  fit_lin <- lm(fm, data = clean_model_data, weights = wt)
  return(fit_lin)
}

#' get_temp_lowest_ever
#'
#' @description Generate Lowest Ever Temperature data-set used to build kables
#' showing percentage of devices which dipped below thresholds.
#'
#' @param wrangled_obs from `wrangled_obs`
#' @param wrangled_devices from `wrangled_devices`
#' @param target_var variable upon which to segment kable
#'
#' @returns Data.Frame
#'
#' @examples
#' lowest_ever           <- get_temp_lowest_ever(wrangled_obs, wrangled_devices, target_var)
#' lowest_ever_kable_var <- get_lowest_ever_kable_var(lowest_ever, target_var)
#'
#' @export
get_temp_lowest_ever <- function(wrangled_obs, wrangled_devices, target_var) {
  tryCatch (
    {

      ## Based on Home Report : WHO Threshold Function
      lowest_ever <-
        monkeyr::who_threshold_data(observations = wrangled_obs) %>%

        ## Mutate temp_band to number (>21=0,<21=1,<18=2...)
        mutate(temp_band = ifelse(temp_band == "< 29°C", "0", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "> 29°C", "0", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 21°C", "1", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 18°C", "2", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 16°C", "3", temp_band)) %>%
        mutate(temp_band = ifelse(temp_band == "< 12°C", "4", temp_band)) %>%

        ## Sum fraction > 21
        group_by(device_id, temp_band) %>%
        summarise(fraction = sum(fraction)) %>%
        ungroup() %>%

        ## Spread - Replaced by Pivot Wider
        # tidyr::pivot_wider(names_from = temp_band, values_from = fraction) %>%

        ## Add back in Device Info
        left_join(wrangled_devices, by = "device_id")

      lowest_ever <-
        lowest_ever %>%
        dplyr::rename(target_var := {target_var}) %>%
        ## Want user to be aware of data that they are not using properly.
        tidyr::replace_na(list(target_var = "unknown")) %>%

        ## assign number to temp bands in use - not in use NA
        group_by(target_var, device_id) %>%
        mutate(worst_band = ifelse(fraction > 0.0, temp_band, NA)) %>%

        ## Highest in use denotes the worst case
        mutate(worst_band = ifelse(worst_band == max(worst_band, na.rm=TRUE), 1, 0)) %>%

        ## Restore NA values
        tidyr::replace_na(list(worst_band = 0)) %>%
        relocate(worst_band, .after = temp_band) %>%

        ## fraction by band grouped by target var
        ungroup() %>%
        group_by(target_var, temp_band) %>%
        summarise(fraction = sum(worst_band)/n())

      ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "get_temp_exposure")
    }
  )
}



#' get_lowest_ever_kable_var
#'
#' @description Using lowest_ever data-set, generate a neat Kable showing
#' temperature exposure groups by a target var.
#'
#' @param lowest_ever from `get_temp_lowest_ever`
#' @param target_var variable upon which to segment kable
#'
#' @returns Kable
#'
#' @examples
#' lowest_ever           <- get_temp_lowest_ever(wrangled_obs, wrangled_devices, target_var)
#' lowest_ever_kable_var <- get_lowest_ever_kable_var(lowest_ever, target_var)
#'
#' @export
get_lowest_ever_kable_var <- function(lowest_ever, target_var) {
  tryCatch (
    {
      exposure_kable_var <-
        lowest_ever %>%

        ## Spread - Replaced by Pivot Wider
        tidyr::pivot_wider(names_from = temp_band, values_from = fraction) %>%

        ## Format Table
        janitor::adorn_pct_formatting() %>%
        kable(col.names = c(stringi::stri_trans_totitle(target_var), "Over 21°C", "< 21°C", "< 18°C", "< 16°C", "< 12°C")) %>%
        kable_material(lightable_options = c("striped", "hover", "condensed"), html_font = "sans-serif") #%>%

      ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "get_exposure_kable_var")
    }
  )
}
