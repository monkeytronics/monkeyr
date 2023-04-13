#' pm_data
#' @description Use WHO guidelines functions on PM
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle.observations` and further filtered by `remove_excluded_devices`
#' @export
pm_data <- function(observations) {
  ## observations <- wrangled_obs
  tryCatch (
    {

      ## Custom Function to Quickly Get Mean Excluding NA
      calcmode <- function(a) {
        vector <- a[!is.na(a)]
        vector <- unique(vector)
        vector[which.max(tabulate(match(a, vector)))]
      }

      bands <- c("> 50 ppm", "> 24 ppm", "> 8 ppm", "< 8 ppm")

      pm_data <- observations %>%
        ## only interested in pm2_5 from 11pm - 7am
        # dplyr::filter(as.numeric(hour) %in% c(23, 0:6)) %>%
        dplyr::filter(reading == "pm2_5" & !is.na(ts)) %>%
        dplyr::rename(pm2_5 = val) %>%

        ## assign values to bands
        dplyr::select(device_id, pm2_5, name) %>%
        dplyr::mutate(
          pm2_5_band   = dplyr::if_else(pm2_5 >= 50,              bands[1], ""),
          pm2_5_band   = dplyr::if_else(pm2_5 < 50 & pm2_5 >= 24, bands[2], pm2_5_band),
          pm2_5_band   = dplyr::if_else(pm2_5 < 24 & pm2_5 >= 8,  bands[3], pm2_5_band),
          pm2_5_band   = dplyr::if_else(pm2_5 < 8,                bands[4], pm2_5_band),
        ) %>%

        ## n observations for device
        dplyr::group_by(device_id, name) %>%
        dplyr::mutate(n_obs = dplyr::n()) %>%
        ungroup() %>%

        ## n observations for each band
        dplyr::group_by(device_id, name, pm2_5_band, n_obs) %>%
        dplyr::summarise(n_obs_band = dplyr::n()) %>%

        ## Add in zeros f rows
        ungroup() %>% # complete doesn't work unless you ungroup!
        tidyr::complete(
          device_id,
          pm2_5_band = bands,
          fill = list(n_obs_band = 0, n_obs = 0)
        ) %>%

        ## Restore name values that got set to NA
        group_by(device_id) %>%
        mutate(name = calcmode(name)) %>%
        ungroup() %>%

        ## replace zero n_obs values
        dplyr::group_by(device_id, name) %>%
        mutate(n_obs = max(n_obs)) %>%

        ## Get percentatge
        dplyr::mutate(fraction = n_obs_band / n_obs) %>%
        slice(match(bands, pm2_5_band))     ## reorder based on bands!

      ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "who_threshold_data")
    }
  )

}



#' get_pm_totals
#' @description Totals for Value Boxes
#' @param pm_data data.frame, the output from `?pm_data()`
#' @export
get_pm_totals <- function(pm_data) {
  tryCatch (
    {
      vent_totals <- pm_data %>%
        dplyr::group_by(pm2_5_band) %>%
        dplyr::summarise_at(c("fraction"), mean)

      above_50 <- vent_totals %>%
        filter(pm2_5_band == "> 50 ppm") %>%
        pull(fraction)

      above_24 <- vent_totals %>%
        filter(pm2_5_band == "> 24 ppm") %>%
        pull(fraction)

      above_8 <- vent_totals %>%
        filter(pm2_5_band == "> 8 ppm") %>%
        pull(fraction)


      list(
        above_8  = above_8  + above_24 + above_50,
        above_24 = above_24 + above_50,
        above_50 = above_50
      )

      ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "get_who_totals")
    }
  )
}


#' pm_donut
#' @description Donut Chart (facetted per device)
#' @param pm_data data.frame, the output from `?pm_data()`
#' @param from_timestamp,to_timestamp timestamps for the begin and end of the report
#' @export
pm_donut <- function(pm_data, from_timestamp, to_timestamp) {
  ## from_timestamp   <- params$fromTimeStamp
  ## to_timestamp     <- params$toTimeStamp
  ## pm_data <- monkeyr::pm_data(observations = wrangled_obs)
  tryCatch (
    {
      checkmate::assert_number(from_timestamp)
      checkmate::assert_number(to_timestamp)

      ## Get palette for 4 levels
      pm_palette <- rev(monkeyr::get_palette("PM2_5", 4))
      bands <- c("> 50 ppm", "> 24 ppm", "> 8 ppm", "< 8 ppm")

      ## prep data set - ensure names unique else concat- device_id
      pm_data <- pm_data %>%
        ## group by device_id & make names unique
        dplyr::group_by(device_id) %>%
        dplyr::mutate(name = ifelse(
          sum(duplicated(name)) >=  length(bands),
          paste0(device_id, " - ", name),
          name
        )) %>%
        dplyr::filter(name == "P000001")

      pm_data$ymax <- cumsum(pm_data$fraction)

      # Compute the cumulative percentages (top of each rectangle)
      pm_data$ymax <- cumsum(pm_data$fraction)

      # Compute the bottom of each rectangle
      pm_data$ymin <- c(0, head(pm_data$ymax, n=-1))

      # Compute label position
      pm_data$labelPosition <- (pm_data$ymax + pm_data$ymin) / 2

      # Compute a good label
      pm_data$label <- paste0(pm_data$pm2_5_band, "\n : ", pm_data$count, "%")


      # Make the plot
      ggplot2::ggplot(pm_data, ggplot2::aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=pm2_5_band)) +
        ggplot2::geom_rect() +
        ggplot2::geom_text( x=2, ggplot2::aes(y=labelPosition, label=label, color=pm2_5_band), size=6) + # x here controls label position (inner / outer)
        ggplot2::scale_fill_brewer(palette=3) +
        ggplot2::scale_color_brewer(palette=3) +
        ggplot2::coord_polar(theta="y") +
        ggplot2::xlim(c(-1, 4)) +
        ggplot2::theme_void() +
        ggplot2::theme(legend.position = "bottom")



    ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "pm_guage")
    }
  )
}


#' pm_chart
#' @description chart based on PM
#' @param pm_data data.frame, the output from `?pm_data()`
#' @param from_timestamp,to_timestamp timestamps for the begin and end of the report
#' @export
pm_chart <- function(pm_data, from_timestamp, to_timestamp) {
  ## from_timestamp   <- params$fromTimeStamp
  ## to_timestamp     <- params$toTimeStamp
  ## pm_data <- monkeyr::pm_data(observations = wrangled_obs)
  tryCatch (
    {
      checkmate::assert_number(from_timestamp)
      checkmate::assert_number(to_timestamp)

      ## Get palette for 4 levels
      pm_palette <- rev(monkeyr::get_palette("PM2_5", 4))
      bands <- c("> 50 ppm", "> 24 ppm", "> 8 ppm", "< 8 ppm")

      ## prep data set - ensure names unique else concat- device_id
      pm_data <- pm_data %>%
        ## group by device_id & make names unique
        dplyr::group_by(device_id) %>%
        dplyr::mutate(name = ifelse(
          sum(duplicated(name)) >=  length(bands),
          paste0(device_id, " - ", name),
          name
        ))


      ## Make chart
      report_period <- report_period(from_timestamp , to_timestamp)
      chart <- pm_data %>%

        ggplot2::ggplot(ggplot2::aes(
          x    = name,
          y    = fraction,
          fill = factor(pm2_5_band, levels=bands) # reverse order cos it was wrong!
        )) +
        ggplot2::geom_bar(
          position = "stack",
          stat = "identity",
          width = 0.6,
          colour = "#e9ecef"
        ) +
        ggplot2::scale_y_continuous(limits = c(0, 1.0), labels = scales::percent_format()) +
        hrbrthemes::theme_ipsum() +
        ggplot2::theme(
          axis.text.x = ggplot2::element_text(
            size = 9,
            angle = 60,
            hjust = 1
          ),
          axis.text.y = ggplot2::element_text(size = 10),
          plot.title  = ggplot2::element_text(size = 9),
          # legend.position = "bottom",
          legend.position = "right",
          legend.title = ggplot2::element_text(size = 9),
          legend.text = ggplot2::element_text(size = 8)
        ) +
        ggplot2::scale_fill_manual(
          name = "",
          values = pm_palette
        ) +
        ggplot2::labs(title = paste0("PM 2.5 Concentration ", report_period)) +
        ggplot2::xlab("") +
        ggplot2::ylab("of total hours")

      # Fire up plotly (basic)
      plotly::ggplotly(chart)

      ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "pm_chart")
    }
  )

}




#' pm_kable
#' @description Render an html table of Ventilation
#' @export
pm_kable <- function() {
  tryCatch (
    {
      kableExtra::kable(
        tibble::tibble(
          indoor_pm = c(
            "below 8 ppm",
            "above 8 ppm",
            "above 24 ppm",
            "above 50 ppm"
          ),
          effect = c(
            "Concentrations typical of occupied indoor spaces - within healthy range",

            "Prolonged exposure to particaulte matter concentrations above this level <br>
             isn't good for your respiratory or cardiovascular health.",

            "Short peaks above 24ppm throughout the day are common.  If there are <br>
             consistent and sustained elevations in PM levels over 24ppm throughout <br>
             the day, try to determine the cause and take steps to reduce it.<br>
             ",

            "Peaks of high PM levels can also occur. If you have followed the above <br>
             advice and continue to have sustained PM levels over 50ppm, consider <br>
             expert advice."

          )
        ),
        col.names = c("PM Concentration", "Health Effects"),
        # caption = "Table 2.1.1. Impact of PM Concentration on health",
        escape = FALSE
      ) %>%
        kableExtra::kable_material(
          lightable_options = c("striped", "hover", "bordered", "responsive"),
          html_font = "sans-serif"
        ) %>%
        # kableExtra::footnote(
        #   general = "Source WHO: Housing and Health Guidelines (ISBN: 978 92 4 155037 6)",
        #   general_title = "*",
        #   footnote_as_chunk = TRUE
        # ) %>%
        kableExtra::column_spec(1:2, width = c("25em","75em"))

      ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "who_guidance_kable")
    }
  )
}

