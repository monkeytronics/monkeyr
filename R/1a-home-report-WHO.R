#' who_threshold_data
#' @description Arrange the measurement according to WHO guidalines
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle.observations` and further filtered by `remove_excluded_devices`
#' @export
who_threshold_data <- function(observations) {
  ## observations <- wrangled_obs
  tryCatch (
    {

      ## Custom Fucntion to Quickly Get Mean Excluding NA
      calcmode <- function(a) {
        vector <- a[!is.na(a)]
        vector <- unique(vector)
        vector[which.max(tabulate(match(a, vector)))]
      }

      o <- observations %>%
        ## only interested in temp from 11pm - 7am
        dplyr::filter(as.numeric(hour) %in% c(23, 0:6)) %>%
        dplyr::filter(reading == "temp" & !is.na(ts)) %>%
        dplyr::rename(temp = val) %>%

        ## assign values to bands
        dplyr::select(device_id, temp, name) %>%
        dplyr::mutate(
          temp_band   = dplyr::if_else(temp < 29 & temp >= 21, "< 29°C", ""),
          temp_band   = dplyr::if_else(temp < 21 & temp >= 18, "< 21°C", temp_band),
          temp_band   = dplyr::if_else(temp < 18 & temp >= 16, "< 18°C", temp_band),
          temp_band   = dplyr::if_else(temp < 16 & temp >= 12, "< 16°C", temp_band),
          temp_band   = dplyr::if_else(temp < 12,              "< 12°C", temp_band),
          temp_band   = dplyr::if_else(temp >= 29,             "> 29°C", temp_band)
        ) %>%

        ## n observations for device
        dplyr::group_by(device_id, name) %>%
        dplyr::mutate(n_obs = dplyr::n()) %>%
        ungroup() %>%

        ## n observations for each band
        dplyr::group_by(device_id, name, temp_band, n_obs) %>%
        dplyr::summarise(n_obs_band = dplyr::n()) %>%

        ## Add in zeros f rows
        ungroup() %>% # complete doesn't work unless you ungroup!
        tidyr::complete(
          device_id,
          temp_band = c("< 29°C", "< 21°C" ,"< 18°C" ,"< 16°C" ,"< 12°C", "> 29°C"),
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
        dplyr::mutate(fraction = n_obs_band / n_obs)

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "who_threshold_data")
    }
  )

}


#' get_heat_palette
#' @description Get Heat Palette Colour Hex Values
#' @param width number, the number of hex values needed
#' @export
get_heat_palette <- function(width) {
  tryCatch (
    {
      heatcols <- 6
      switch(width,
             {heatcols <- c("#F97162")},
             {heatcols <- c("#F97162", "#FBE1B1")},
             {heatcols <- c("#F97162", "#FBE1B1", "#B7DFCB")},
             # {heatcols <- c("#F97162", "#FBE1B1", "#B7DFCB", "#5ABAD1")},
             {heatcols <- c("#e2d5f9", "#c5abf4", "#a781ee", "#6d2de3")},
             {heatcols <- c("#F97162", "#FBE1B1", "#B7DFCB", "#5ABAD1", "#3984B6")},
             {heatcols <- c("#F97162", "#FBE1B1", "#B7DFCB", "#5ABAD1", "#3984B6", "#275a7c")},
      )

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "get_heat_palette")
    }
  )
}



#' degrees_below_18_score
#' @description Calculate the degrees below 18C score
#' @param observations data.frame, the wrangled observations data frame as output by
#' `wrangle.observations` and further filtered by `remove_excluded_devices`
#' @export
degrees_below_18_score <- function(observations) {
  tryCatch (
    {
      ## average degrees below 18
      browser()
      score <- observations %>%
        ## only intereted in 11pm - 7am
        dplyr::filter(as.numeric(hour) %in% c(23, 0:6)) %>%
        dplyr::mutate(cap_temp = ifelse(temp > 18, 18, temp)) %>%
        dplyr::summarise(sum(18 - cap_temp)) / nrow(.)

      ## shitness score. 12deg (score = 6) -> 100.
      score <- 100 - 100 * (score / (18 - 12))
      floor(max(score, 0))

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "degrees_below_18_score")
    }
  )


}


#' get_who_totals
#' @description Totals for Value Boxes
#' @param who_threshold_data data.frame, the output from `?who_threshold_data()`
#' @export
get_who_totals <- function(who_threshold_data) {
  tryCatch (
    {
      who_totals <- who_threshold_data %>%
        dplyr::group_by(temp_band) %>%
        dplyr::summarise_at(c("fraction"), mean)

      below_12 <- who_totals %>%
        filter(temp_band == "< 12°C") %>%
        pull(fraction)

      below_16 <- who_totals %>%
        filter(temp_band == "< 16°C") %>%
        pull(fraction)

      below_18 <- who_totals %>%
        filter(temp_band == "< 18°C") %>%
        pull(fraction)

      list(
        below_18 = below_18 + below_16 + below_12,
        below_16 = below_16 + below_12,
        below_12 = below_12
      )

    ## Error Handler
    },
    error = function(cond) {
      monkeyr::monkey_knit_error(err = cond, resource = "get_who_totals")
    }
  )
}


#' who_threshold_chart
#' @description WHO threshold chart
#' @param who_threshold_data data.frame, the output from `?who_threshold_data()`
#' @param from_timestamp,to_timestamp timestamps for the begin and end of the report
#' @export
who_threshold_chart <- function(who_threshold_data, from_timestamp, to_timestamp) {
  ## from_timestamp <- params$fromTimeStamp
  ## to_timestamp   <- params$toTimeStamp
  ## who_threshold_data <- monkeyr::who_threshold_data(observations = wrangled_obs)
  tryCatch (
    {
      checkmate::assert_number(from_timestamp)
      checkmate::assert_number(to_timestamp)

      ## Get palette for 6 levels
      heatcols <- get_heat_palette(6)
      bands    <- c(temp_band = c("< 29°C", "< 21°C" ,"< 18°C" ,"< 16°C" ,"< 12°C", "> 29°C"))

      ## prep data set - ensure names unique else concat- device_id
      who_threshold_data <- who_threshold_data %>%
        ## group by device_id & make names unique
        dplyr::group_by(device_id) %>%
        dplyr::mutate(name = ifelse(
          sum(duplicated(name)) > length(bands),
          paste0(device_id, " - ", name),
          name
        ))


      ## Make chart
      report_period <- report_period(from_timestamp , to_timestamp)
      who_chart <- who_threshold_data %>%

        ggplot2::ggplot(ggplot2::aes(
          x    = name,
          y    = fraction,
          fill = forcats::fct_rev(temp_band) # reverse order cos it was wrong!
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
        ggplot2::labs(title = paste0("Night time temperature ", report_period)) +
        ggplot2::xlab("") +
        ggplot2::ylab("of total night-time hours")

        # Fire up plotly (basic)
        # plotly::ggplotly(who_chart)

        # Fire up plotly and disable acceptable traces.
        plotly::style(
          plotly::ggplotly(who_chart),
          visible="legendonly",
          traces = c(1,2)
        )

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "who_threshold_chart")
    }
  )

}



#' who_guidance_kable
#' @description Render an html table of WHO guidelines
#' @export
who_guidance_kable <- function() {
  tryCatch (
    {
      kableExtra::kable(
        tibble::tibble(
          indoor_temp = c(
            "above 29°C",
            "21 to 29°C",
            "below 21°C",
            "below 18°C",
            "below 16°C",
            "below 12°C"
          ),
          effect = c(
            "Evidence from the modelling of outdoor  temperature indicates <br>
             that health effects are likely above 29 °C",

            "Healthy range for indoor temperature",

            "An indoor temperature of at least  21 °C is recommended  for <br>
             vulnerable groups including older people,  children and those <br>
             with chronic illnesses, particularly cardiorespiratory disease",

            "18 °C is the WHO recommended  minimum  indoor  temperature to <br>
             protect the health of general populations during cold seasons",

            "Temperatures under 16 °C are shown to have cardiovascular effects",

            "Temperatures under 12 °C have been shown to cause immediate <br>
             reduction in children’s lung function. "
          )
        ),
        col.names = c("Indoor Temp", "Health Effects"),
        # caption = "Table 2.1.1. Impact of Indoor temperature on health",
        escape = FALSE
      ) %>%
        kableExtra::kable_material(
          lightable_options = c("striped", "hover", "bordered", "responsive"),
          html_font = "sans-serif"
        ) %>%
        kableExtra::footnote(
          general = "Source WHO: Housing and Health Guidelines (ISBN: 978 92 4 155037 6)",
          general_title = "*",
          footnote_as_chunk = TRUE
        ) %>%
        kableExtra::column_spec(1:2, width = c("25em","75em"))

    ## Error Handler
    },
      error = function(cond) {
        monkeyr::monkey_knit_error(err = cond, resource = "who_guidance_kable")
    }
  )
}

