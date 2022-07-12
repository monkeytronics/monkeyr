#' who_threshold_data
#' @description Arrange the measurement according to WHO guidalines
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle.observations` and further filtered by `remove_excluded_devices`
#' @export
ventilation_data <- function(observations) {
  ## observations <- wrangled_obs
  tryCatch (
    {

      ## Custom Fucntion to Quickly Get Mean Excluding NA
      calcmode <- function(a) {
        vector <- a[!is.na(a)]
        vector <- unique(vector)
        vector[which.max(tabulate(match(a, vector)))]
      }

      bands <- c("> 2000 ppm", "> 1250 ppm", "> 800 ppm", "< 800 ppm")

      ventilation_data <- observations %>%
        ## only interested in co2 from 11pm - 7am
        # dplyr::filter(as.numeric(hour) %in% c(23, 0:6)) %>%
        dplyr::filter(reading == "co2" & !is.na(ts)) %>%
        dplyr::rename(co2 = val) %>%

        ## assign values to bands
        dplyr::select(device_id, co2, name) %>%
        dplyr::mutate(
          co2_band   = dplyr::if_else(co2 >= 2000,              bands[1], ""),
          co2_band   = dplyr::if_else(co2 < 2000 & co2 >= 1250, bands[2], co2_band),
          co2_band   = dplyr::if_else(co2 < 1250 & co2 >= 800,  bands[3], co2_band),
          co2_band   = dplyr::if_else(co2 < 800,                bands[4],  co2_band),
        ) %>%

        ## n observations for device
        dplyr::group_by(device_id, name) %>%
        dplyr::mutate(n_obs = dplyr::n()) %>%
        ungroup() %>%

        ## n observations for each band
        dplyr::group_by(device_id, name, co2_band, n_obs) %>%
        dplyr::summarise(n_obs_band = dplyr::n()) %>%

        ## Add in zeros f rows
        ungroup() %>% # complete doesn't work unless you ungroup!
        tidyr::complete(
          device_id,
          co2_band = bands,
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
        slice(match(bands, co2_band))     ## reorder based on bands!

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "who_threshold_data")
    }
  )

}

#'
#' #' get_heat_palette
#' #' @description Get Heat Palette Colour Hex Values
#' #' @param width number, the number of hex values needed
#' #' @export
#' get_heat_palette <- function(width) {
#'   tryCatch (
#'     {
#'       heatcols <- 6
#'       switch(width,
#'              {heatcols <- c("#F97162")},
#'              {heatcols <- c("#F97162", "#FBE1B1")},
#'              {heatcols <- c("#F97162", "#FBE1B1", "#B7DFCB")},
#'              {heatcols <- c("#F97162", "#FBE1B1", "#B7DFCB", "#5ABAD1")},
#'              {heatcols <- c("#F97162", "#FBE1B1", "#B7DFCB", "#5ABAD1", "#3984B6")},
#'              {heatcols <- c("#F97162", "#FBE1B1", "#B7DFCB", "#5ABAD1", "#3984B6", "#275a7c")},
#'       )
#'
#'     ## Error Handler
#'     },
#'       error = function(cond) {
#'         monkeyr::monkey_knit_error(err = cond, resource = "get_heat_palette")
#'     }
#'   )
#' }
#'
#'
#'
#' #' degrees_below_18_score
#' #' @description Calculate the degrees below 18C score
#' #' @param observations data.frame, the wrangled observations data frame as output by
#' #' `wrangle.observations` and further filtered by `remove_excluded_devices`
#' #' @export
#' degrees_below_18_score <- function(observations) {
#'   tryCatch (
#'     {
#'       ## average degrees below 18
#'       browser()
#'       score <- observations %>%
#'         ## only intereted in 11pm - 7am
#'         dplyr::filter(as.numeric(hour) %in% c(23, 0:6)) %>%
#'         dplyr::mutate(cap_co2 = ifelse(co2 > 18, 18, co2)) %>%
#'         dplyr::summarise(sum(18 - cap_co2)) / nrow(.)
#'
#'       ## shitness score. 12deg (score = 6) -> 100.
#'       score <- 100 - 100 * (score / (18 - 12))
#'       floor(max(score, 0))
#'
#'     ## Error Handler
#'     },
#'       error = function(cond) {
#'         monkeyr::monkey_knit_error(err = cond, resource = "degrees_below_18_score")
#'     }
#'   )
#'
#'
#' }
#'
#'
#' get_ventilation_totals
#' @description Totals for Value Boxes
#' @param who_threshold_data data.frame, the output from `?ventilation_data()`
#' @export
get_ventilation_totals <- function(ventilation_data) {
  tryCatch (
    {
      vent_totals <- ventilation_data %>%
        dplyr::group_by(co2_band) %>%
        dplyr::summarise_at(c("fraction"), mean)

      above_2000 <- vent_totals %>%
        filter(co2_band == "> 2000 ppm") %>%
        pull(fraction)

      above_1250 <- vent_totals %>%
        filter(co2_band == "> 1250 ppm") %>%
        pull(fraction)

      above_800 <- vent_totals %>%
        filter(co2_band == "> 800 ppm") %>%
        pull(fraction)

      list(
        above_800  = above_800  + above_1250 + above_2000,
        above_1250 = above_1250 + above_2000,
        above_2000 = above_2000
      )

    ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "get_who_totals")
    }
  )
}


#' ventilation_chart
#' @description Ventilation chart based on CO2
#' @param ventilation_data data.frame, the output from `?ventilation_data()`
#' @param from_timestamp,to_timestamp timestamps for the begin and end of the report
#' @export
ventilation_chart <- function(ventilation_data, from_timestamp, to_timestamp) {
  ## from_timestamp   <- params$fromTimeStamp
  ## to_timestamp     <- params$toTimeStamp
  ## ventilation_data <- monkeyr::ventilation_data(observations = wrangled_obs)
  tryCatch (
    {
      checkmate::assert_number(from_timestamp)
      checkmate::assert_number(to_timestamp)

      ## Get palette for 6 levels
      heatcols <- rev(get_heat_palette(4))
      bands <- c("> 2000 ppm", "> 1250 ppm", "> 800 ppm", "< 800 ppm")

      ## prep data set - ensure names unique else concat- device_id
      ventilation_data <- ventilation_data %>%
        ## group by device_id & make names unique
        dplyr::group_by(device_id) %>%
        dplyr::mutate(name = ifelse(
          sum(duplicated(name)) >=  length(bands),
          paste0(device_id, " - ", name),
          name
        ))


      ## Make chart
      report_period <- report_period(from_timestamp , to_timestamp)
      chart <- ventilation_data %>%

        ggplot2::ggplot(ggplot2::aes(
          x    = name,
          y    = fraction,
          fill = factor(co2_band, levels=bands) # reverse order cos it was wrong!
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
          values = heatcols
        ) +
        ggplot2::labs(title = paste0("CO2 Concentration ", report_period)) +
        ggplot2::xlab("") +
        ggplot2::ylab("of total hours")

        # Fire up plotly (basic)
        plotly::ggplotly(chart)

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "who_threshold_chart")
    }
  )

}



#' ventilation_kable
#' @description Render an html table of Ventilation
#' @export
ventilation_kable <- function() {
  tryCatch (
    {
      kableExtra::kable(
        tibble::tibble(
          indoor_co2 = c(
            "below 800 ppm",
            "above 800 ppm",
            "above 1250 ppm",
            "above 2000 ppm"
          ),
          effect = c(
            "Concentrations typical of occupied indoor spaces with good air exchange",

            "Open all your windows as much as it is practicle to do so while <br>
             maintaining comfortable indoor temperatures. Rooms which iexperience <br>
             consistenly high levels may require purging or maintenance on the windows",

            "Short peaks above 1250ppm throughout the day are common.  If there are <br>
             consistent and sustained elevations in CO2 levels over 1250ppm throughout <br>
             the day, consider face coverings and reduced occupancy.<br>
             ",

            "Peaks of high CO2 levels can also occur. If you have followed the above <br>
             advice and continue to have sustained CO2 levels over 2000ppm, consider <br>
            expert advice."

          )
        ),
        col.names = c("CO2 Concentration", "Health Effects"),
        # caption = "Table 2.1.1. Impact of CO2 Concentration on health",
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

