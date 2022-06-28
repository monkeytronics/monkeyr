#' get_reg_data
#'
#' @description Make Data-Frame of Regression Test:
#'   for use with indoor - outdoor regression testing
#'
#' @param wranged_devices from `wrangle_observations( )`
#' @param wrangled_obs from `wrangled_obs( )`
#' @param target_reading variable upon which to segment data
#'
#' @returns data frame
#'
#' @examples
#' reg_data  <- get_reg_data(wranged_devices, wrangled_obs, target_reading = "temp")
#' reg_kable <- get_reg_kable(reg_data, target_var)
#'
#' @export
get_reg_data <- function(wranged_devices, wrangled_obs, target_reading = "temp") {

  ## Wrangle Obs for Temperature Regression Tests
  regression <- wrangled_obs %>%
    filter(!is.na(outdoor_temp)) %>%
    filter(reading == target_reading) %>%
    filter(as.integer(hour) %in% c(20:23, 0:7))

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
    left_join(wrangled_devices, by = "device_id")

}


#' get_reg_kable
#'
#' @description Make Kable from Inside-Outside Regression Data that shows detailed
#' device estimated correlation factor.
#'
#' @param reg_data from `get_reg_data`
#' @param target_var variable upon which to segment kable
#'
#' @returns kable
#'
#' @examples
#' reg_data  <- get_reg_data(wranged_devices, wrangled_obs, target_reading = "temp")
#' reg_kable <- get_reg_kable(reg_data, target_var)
#'
#' @export
get_reg_kable <- function(reg_data, target_var) {

  ## Create Local var Regression for current variable - Leave original unchanged!
  reg_data_local <-
    reg_data %>%
    dplyr::rename(target_var := {target_var}) %>%
    ## Want user to be aware of data that they are not using properly.
    tidyr::replace_na(list(target_var = "unknown"))

  # ## Rename column (base R method)
  # names(regression)[names(regression) == target_var <- "target_var"


  ## Kable to Print
  reg_kable <-
    reg_data_local %>%
    ## Keep necessary cols

    select(city, hhi, target_var, device_id, estimate) %>%
    relocate(city, .after = hhi) %>%

    kable(col.names = c("HHI provider", "City", target_var, "Device Id", "Correlation")) %>%
    kableExtra::kable_material(bootstrap_options = c("striped", "hover", "condensed", "responsive"),
                   html_font = "sans-serif") %>%

    collapse_rows(columns = 1:3, valign = "middle") %>%
    scroll_box(width = "100%", height = "350px")
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
#' reg_data  <- get_reg_data(wranged_devices, wrangled_obs, target_reading = "temp")
#' reg_kable <- get_reg_kable(reg_data, target_var)
#'
#' @export
get_reg_boxplot <- function(reg_data, target_var) {

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
    labs(x = "Estimated Correlation", y = target_var) +

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
  ## Based on Home Report : WHO Threshold Function
  temp_exposure <-
    monkeyr::who_threshold_data(observations = wrangled_obs) %>%

    ## Group all > 21
    mutate(temp_band = ifelse(temp_band == "< 29°C", "> 21°C", temp_band)) %>%
    mutate(temp_band = ifelse(temp_band == "> 29°C", "> 21°C", temp_band)) %>%

    ## Sum fraction > 21
    group_by(device_id, temp_band) %>%
    summarise(fraction = round(sum(fraction), 3)) %>%
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

  exposure_kable_var <-
    temp_exposure %>%
    select(-device_id) %>%
    group_by(target_var) %>%
    summarise_all(.funs = c(mean="mean")) %>%
    janitor::adorn_pct_formatting() %>%
    kable(col.names = c(stringi::stri_trans_totitle(target_var), "< 21°C", "< 18°C", "< 16°C", "< 12°C")) %>%
    kable_material(bootstrap_options = c("striped", "hover", "condensed"), html_font = "sans-serif") %>%
    collapse_rows(columns = 1:2, valign = "middle")
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

  exposure_kable_dev <-
    temp_exposure %>%
    relocate(device_id) %>%
    group_by(target_var, device_id)  %>%
    arrange(desc(`< 21°C`)) %>%
    janitor::adorn_pct_formatting() %>%
    kable(col.names = c("Device Id", stringi::stri_trans_totitle(target_var), "< 21°C", "< 18°C", "< 16°C", "< 12°C")) %>%
    kable_material(bootstrap_options = c("striped", "hover", "condensed"), html_font = "sans-serif")
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
#' @export
get_exposure_chart_data <- function(wrangled_obs, wrangled_devices, target_var) {

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
    summarise(fraction = round(sum(fraction), 3)) %>%
    ungroup() %>%

    ## Add back in Device Info
    left_join(wrangled_devices, by = "device_id") %>%

    ## Rename target_var
    dplyr::rename(target_var := {target_var}) %>%
    ## Want user to be aware of data that they are not using properly.
    tidyr::replace_na(list(target_var = "unknown"))



  ## Create "all" group
  temp_exposure_all <-
    temp_exposure %>%
    mutate(target_var := "All")

  ## Join "All" back in
  exposure_chart_data <-
    full_join(temp_exposure, temp_exposure_all)


  ## UNIQUE CODE BELOW HERE
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
           label = ifelse(fraction < 0.01, "", paste0(round(fraction*100, 0), "%"))) #%>%
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
get_exposure_plot <- function(exposure_chart_data, target_var) {

  plot <-
    exposure_chart_data %>%
    ggplot(ggplot2::aes(ymax = ymax, ymin = ymin,
               xmax = 2, xmin = 0,
               fill = as.factor(temp_band))) +
    geom_rect() +
    geom_text(x = 2.5,
              ggplot2::aes(y = label_pos,
                  label = label),
              size = 3) +

    nord::scale_fill_nord("lumina", # moose_pond
                          name = "of device experienced\n a night-time temperature\n",
                          label = c("> 20°C", "< 20°C", "< 18°C", "< 16°C", "< 12°C"),
                          drop = FALSE) +
    nord::scale_color_nord("lumina", # moose_pond
                           drop = FALSE) +

    guides(color = "none",
           fill = guide_legend(title.position = "top")) +

    # coord_polar(theta="y") +
    xlim(c(0, 3)) +
    ylim(c(1, 0)) + # reversing so lower temps are at bottom.

    facet_wrap(vars( target_var ), ncol=4) +

    # facet_grid(cols = vars(room_type), rows = vars(hhi, rental_type)) +

    labs(title = paste0(""), y = "", x = "") +

    hrbrthemes::theme_ipsum_ps() +

    theme(plot.margin = margin(0.0, 0.0, 0.0, 0.0, "cm"),

          strip.text.x = element_text(angle = 0, hjust = 0.3),

          legend.title = element_text(size = 9),
          legend.position = "right",

          axis.title.x = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),

          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())
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
    summarise(fraction = round(sum(fraction), 3)) %>%
    ungroup() %>%

    ## Add back in Device Info
    left_join(wrangled_devices, by = "device_id") %>%

    ## Rename target_var
    dplyr::rename(target_var := {target_var}) %>%
    ## Want user to be aware of data that they are not using properly.
    tidyr::replace_na(list(target_var = "unknown"))

  ## Create "all" group
  temp_exposure_all <-
    temp_exposure %>%
    mutate(target_var := "All")

  ## Join "All" back in
  lowest_temp_chart_data <-
    full_join(temp_exposure, temp_exposure_all)


  ## UNIQUE CODE BELOW HERE
  ## Flag Worst temp_band
  lowest_temp_chart_data <-
    lowest_temp_chart_data %>%
    group_by(target_var, device_id) %>%
    relocate(target_var, .after = device_id) %>%
    ## flag temp_bands that were found
    mutate(worst_band = ifelse(fraction > 0.0, temp_band, 0)) %>%
    mutate(worst_band = ifelse(worst_band == max(worst_band), 1, 0)) %>%
    relocate(worst_band, .after = temp_band) %>%

    ## contibution to fraction...
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
           label = ifelse(fraction < 0.01, "", paste0(round(fraction*100, 0), "%")))

}
