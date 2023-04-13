#' hcho_data
#' @description Use WHO guidelines functions on Formaldehyde
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle.observations` and further filtered by `remove_excluded_devices`
#' @export
hcho_data <- function(observations) {
  ## observations <- wrangled_obs
  tryCatch (
    {

      ## Custom Function to Quickly Get Mean Excluding NA
      calcmode <- function(a) {
        vector <- a[!is.na(a)]
        vector <- unique(vector)
        vector[which.max(tabulate(match(a, vector)))]
      }

      bands <- c("> 1000 ppb", "> 800 ppb", "> 500 ppb", "< 500 ppb")

      hcho_data <- observations %>%
        ## only interested in hcho from 11pm - 7am
        # dplyr::filter(as.numeric(hour) %in% c(23, 0:6)) %>%
        dplyr::filter(reading == "hcho" & !is.na(ts)) %>%
        dplyr::rename(hcho = val) %>%

        ## assign values to bands
        dplyr::select(device_id, hcho, name) %>%
        dplyr::mutate(
          hcho_band   = dplyr::if_else(hcho >= 1000,              bands[1], ""),
          hcho_band   = dplyr::if_else(hcho < 1000 & hcho >= 800, bands[2], hcho_band),
          hcho_band   = dplyr::if_else(hcho < 800 & hcho >= 500,  bands[3], hcho_band),
          hcho_band   = dplyr::if_else(hcho < 500,                bands[4], hcho_band),
        ) %>%

        ## n observations for device
        dplyr::group_by(device_id, name) %>%
        dplyr::mutate(n_obs = dplyr::n()) %>%
        ungroup() %>%

        ## n observations for each band
        dplyr::group_by(device_id, name, hcho_band, n_obs) %>%
        dplyr::summarise(n_obs_band = dplyr::n()) %>%

        ## Add in zeros f rows
        ungroup() %>% # complete doesn't work unless you ungroup!
        tidyr::complete(
          device_id,
          hcho_band = bands,
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
        slice(match(bands, hcho_band))     ## reorder based on bands!

      ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "who_threshold_data")
    }
  )

}



#' get_hcho_totals
#' @description Totals for Value Boxes
#' @param hcho_data data.frame, the output from `?hcho_data()`
#' @export
get_hcho_totals <- function(hcho_data) {
  tryCatch (
    {
      vent_totals <- hcho_data %>%
        dplyr::group_by(hcho_band) %>%
        dplyr::summarise_at(c("fraction"), mean)

      above_1000 <- vent_totals %>%
        filter(hcho_band == "> 1000 ppb") %>%
        pull(fraction)

      above_800 <- vent_totals %>%
        filter(hcho_band == "> 800 ppb") %>%
        pull(fraction)

      above_500 <- vent_totals %>%
        filter(hcho_band == "> 500 ppb") %>%
        pull(fraction)


      list(
        above_500  = above_500  + above_800 + above_1000,
        above_800  = above_800 + above_1000,
        above_1000 = above_1000
      )

      ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "get_who_totals")
    }
  )
}


#' hcho_guage
#' @description Guage Charts facetted per device
#' @param hcho_data data.frame, the output from `?hcho_data()`
#' @param from_timestamp,to_timestamp timestamps for the begin and end of the report
#' @export
hcho_guage <- function(hcho_data, from_timestamp, to_timestamp) {
  ## from_timestamp   <- params$fromTimeStamp
  ## to_timestamp     <- params$toTimeStamp
  ## hcho_data <- monkeyr::hcho_data(observations = wrangled_obs)
  tryCatch (
    {
      checkmate::assert_number(from_timestamp)
      checkmate::assert_number(to_timestamp)

      ## Get palette for 4 levels
      hcho_palette <- dev(monkeyr::get_palette("HCHO", 4))
      bands <- c("> 1000 ppb", "> 800 ppb", "> 500 ppb", "< 500 ppb")

      ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "hcho_guage")
    }
  )
}


#' hcho_chart
#' @description chart based on Formaldehyde
#' @param hcho_data data.frame, the output from `?hcho_data()`
#' @param from_timestamp,to_timestamp timestamps for the begin and end of the report
#' @export
hcho_chart <- function(hcho_data, from_timestamp, to_timestamp) {
  ## from_timestamp   <- params$fromTimeStamp
  ## to_timestamp     <- params$toTimeStamp
  ## hcho_data <- monkeyr::hcho_data(observations = wrangled_obs)
  tryCatch (
    {
      checkmate::assert_number(from_timestamp)
      checkmate::assert_number(to_timestamp)

      ## Get palette for 4 levels
      # hcho_palette <- c('#456c35', '#778f51', '#acc38b', '#c9c8b4')
      hcho_palette <- rev(monkeyr::get_palette("HCHO", 4))
      bands <- c("> 1000 ppb", "> 800 ppb", "> 500 ppb", "< 500 ppb")

      ## prep data set - ensure names unique else concat- device_id
      hcho_data <- hcho_data %>%
        ## group by device_id & make names unique
        dplyr::group_by(device_id) %>%
        dplyr::mutate(name = ifelse(
          sum(duplicated(name)) >=  length(bands),
          paste0(device_id, " - ", name),
          name
        ))


      ## Make chart
      report_period <- report_period(from_timestamp , to_timestamp)
      chart <- hcho_data %>%

        ggplot2::ggplot(ggplot2::aes(
          x    = name,
          y    = fraction,
          fill = factor(hcho_band, levels=bands) # reverse order cos it was wrong!
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
          values = hcho_palette
        ) +
        ggplot2::labs(title = paste0("Formaldehyde Concentration ", report_period)) +
        ggplot2::xlab("") +
        ggplot2::ylab("of total hours")

      # Fire up plotly (basic)
      plotly::ggplotly(chart)

      ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "hcho_chart")
    }
  )

}




#' hcho_kable
#' @description Render an html table of Ventilation
#' @export
hcho_kable <- function() {
  tryCatch (
    {
      kableExtra::kable(
        tibble::tibble(
          indoor_hcho = c(
            "below 500 ppb",
            "above 500 ppb",
            "above 800 ppb",
            "above 1000 ppb"
          ),
          effect = c(
            "Concentrations typical of occupied indoor spaces - within healthy range",

            "Prolonged exposure to particaulte matter concentrations above this level <br>
             isn't good for your respiratory or cardiovascular health.",

            "Short peaks above 800 ppb throughout the day are common.  If there are <br>
             consistent and sustained elevations in Formaldehyde levels over 800 ppb, try<br>
             to determine the cause and take steps to reduce it.<br>
             ",

            "Peaks of high Formadehyde levels can also occur. If you have followed <br>
             the above advice and continue to have sustained Formaldehyde levels over 1000 ppb, <br>
             consider expert advice."

          )
        ),
        col.names = c("Formaldehyde Concentration", "Health Effects"),
        # caption = "Table 2.1.1. Impact of Formaldehyde Concentration on health",
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

