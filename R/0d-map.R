#' get_map_params
#' @description Get map params for target country as list
#' @param countryCode string : the country code ("NZ", "AU", ...)
#' @returns list : country map parameters
#' @examples
#' map_params  <- get_map_params("NZ")
#'
#' @export
get_map_params <- function(countryCode_local) {
  ### countryCode_local <- "IE" For Test

  map_params <- NULL

  ## geojson = "../maps/nz/nz_region.geojson",       # from rmd
  ## geojson = "./inst/maps/nz/nz_region.geojson",   # from R  (i.e. from this file)

  ## main is the whole country map - choose less detailed type
  ## inset is close up - use detail.
  ## want to make the inset as small as possible.
  ## maps & col fill must match.

  ## Available Palettes
  ## Blues BuGn BuPu GnBu Greens Greys Oranges OrRd PuBu PuBuGn PuRd Purples RdPu Reds YlGn YlGnBu YlOrBr YlOrRd

  ### New Zealand Data
  rbind(map_params,
        data.frame(
          countryCode = "NZ",
          countryName = "New Zealand",
          main = list(
            geojson = "../maps/nz/nz_region.geojson",
            admin_distr_col = "REGC2016_N",
            colour_col = "AREA_SQ_KM",
            palette = "Blues",
            colour_depth = 0.3,
            admin_distr_modal = "tbd",
            city_size = 50000),
          inset = list(
            geojson = "../maps/nz/nz_ta.geojson",
            admin_distr_col = "TA2016_NAM",
            colour_col = "AREA_SQ_KM",
            palette = "Blues",
            colour_depth = 0.3,
            admin_distr_modal = "tbd",
            city_size = 20000),
          bbox_src = "main",
          inset_map_src = "inset",
          insert = list(
            x       = 0.10,
            y       = 0.40,
            height  = 0.43,
            width   = 0.38),
          margins = list(
            left    = 0.20,
            right   = 0.20,
            bottom  = 0.00,
            top     = 0.00
          )
        )
  )->map_params

  ### Australia Data
  rbind(map_params,
        data.frame(
          countryCode = "AU",
          countryName = "Austalia",
          main = list(
            geojson = "../maps/au/aus_lga.geojson",   # detailed
            admin_distr_col = "Name",
            colour_col = "colour_col", # if colour_col, need to add
            palette = "Oranges",
            colour_depth = 0.3,
            admin_distr_modal = "tbd",
            city_size = 100000),
          inset = list(
            geojson = "../maps/au/aus_state.geojson", # state level
            admin_distr_col = "STATE_NAME",
            colour_col = "rmapshaperid",
            palette = "Oranges",
            colour_depth = 0.3,
            admin_distr_modal = "tbd",
            city_size = 10000),
          bbox_src = "inset",
          inset_map_src = "inset",
          insert = list(
            x       = 0.28,
            y       = 0.05,
            height  = 0.40,
            width   = 0.40),
          margins = list(
            left    = 0.00,
            right   = 0.10,
            bottom  = 0.60,
            top     = 0.00
          )
        )
  )->map_params

  ### UK Data
  rbind(map_params,
        data.frame(
          countryCode = "GB", # ISO code for UK! Go figure.
          countryName = "United Kingdom",
          main = list(
            geojson = "../maps/uk/uk_ta.json",   # Nations
            admin_distr_col = "NAME_1",             # Eng, Sct, Wal, NI.
            colour_col = "ID_1",
            palette = "Reds",
            colour_depth = 0.3,
            admin_distr_modal = "tbd",
            city_size = 300000),
          inset = list(
            geojson = "../maps/uk/uk_admin.json", # Counties
            admin_distr_col = "NAME_2",           # Name_1 is {England, Scotland, Wales, Northern Ireland}
            colour_col = "ID_2",
            palette = "Reds",
            colour_depth = 0.3,
            admin_distr_modal = "tbd",
            city_size = 20000),
          bbox_src = "inset",
          inset_map_src = "inset",
          insert = list(
            x       = 0.20,
            y       = 0.05,
            height  = 0.30,
            width   = 0.30),
          margins = list(
            left    = 0.00,
            right   = 0.20,
            bottom  = 0.00,
            top     = 0.00
          )
        )
  )->map_params

  ### Ireland Data
  rbind(map_params,
        data.frame(
          countryCode = "IE",
          countryName = "Ireland",
          main = list(
            geojson = "../maps/ie/ia_ta.geojson",       # Provinces
            admin_distr_col = "NAME",                   # Ulster, Monster, ...
            colour_col = "colour_col",                  # None
            palette = "Greens",
            colour_depth = 0.3,
            admin_distr_modal = "tbd",
            city_size = 50000),
          inset = list(
            geojson = "../maps/ie/ia-counties.geojson", # Counties
            admin_distr_col = "name",                   # country name
            colour_col = "cartodb_id",
            palette = "Greens",
            colour_depth = 0.3,
            admin_distr_modal = "tbd",
            city_size = 10000),
          bbox_src = "inset",
          inset_map_src = "inset",
          insert = list(
            x       = 0.05,
            y       = 0.33,
            height  = 0.50,
            width   = 0.50),
          margins = list(
            left    = 0.70,
            right   = 0.00,
            bottom  = 0.00,
            top     = 0.00
          )
        )
  )->map_params

  ### Get Country Data as single row dataframe
  map_params <- map_params %>%
    dplyr::filter(countryCode == countryCode_local)

  map_params <- list(map_params[1,])[[1]]
  map_params$countryCode

  return (map_params)
}


#' read_geojson
#' @description Get geojson object from file
#' @param geojson_file string : path from `map_params$xxxx.geojson`
#' @returns sf : geojson map object as sf
#'
#' @examples
#' geojson_main  <- read_geojson(
#'   geojson_file = map_params$main.geojson
#' )
#'
#' geojson_inset  <- read_geojson(
#'   geojson_file = map_params$inset.geojson
#' )
#'
#' @export
read_geojson <- function(geojson_file) {
  ### geojson_file <- "./inst/maps/ie/ia-counties.geojson"   for test
  ### geojson_file <- "./inst/maps/au/aus_state.geojson"  for test


  ### Read Geojson from file
  geojson <- sf::st_read(geojson_file)

  #### Convert to SF objects and generate centroids
  geojson <- sf::st_as_sf(geojson)
  geojson <- sf::st_make_valid(geojson)
  geojson <- cbind(geojson, sf::st_coordinates(sf::st_centroid(geojson)))
}



#' geojson_verify_colour_col
#' @description Generate missing Colour Fill Col for geojson object
#' @param geojson_file sf
#' @returns sf : geojson map object as sf
#'
#' @examples
#' geojson_main  <- geojson_verify_colour_col(
#'   geojson = geojson_main,
#'   map_params = map_params,
#'   map_layer = "main"
#' )
#'
#' @export
geojson_verify_colour_col <- function(geojson, map_params, map_layer) {

  ### Default
  gen_colour_col <- NULL

  ### Check if needed
  if (map_layer == "main" && map_params$main.colour_col == "colour_col") {
    gen_colour_col <- TRUE
  }

  if (map_layer == "inset" && map_params$inset.colour_col == "colour_col") {
    gen_colour_col <- TRUE
  }

  ### Do it
  if (!is.null(gen_colour_col)) {
    geojson <- geojson %>%
      mutate(colour_col = row_number())
  }

  return (geojson)

}



#' update_devices_for_map
#' @description :
#' Add admin_distr to map object for this map
#'
#' @param devices_for_map data.frame : from `monkeyr::get_devices_for_map()`
#' @param admin_distr_col string : from `map_params$xxxx.admin_distr_col`
#' @param map_geojson sf : geojson map object
#' @param map_layer string : either "main" or "inset"
#'
#' @returns data.frame
#'
#' @examples
#' devices_for_map  <- update_devices_for_map(
#'   devices_for_map = devices_for_map,
#'   map_geojson     = geojson_main,
#'   admin_distr_col = map_params$main.admin_distr_col,
#'   map_layer       = "main"
#' )
#'
#' @export
update_devices_for_map <- function(devices_for_map, map_geojson, admin_distr_col, map_layer) {

  ### check admin district present in map
  if (admin_distr_col %in% names(map_geojson)) {

    ### Calc admin_distr for devices
    devices_for_map <- devices_for_map %>%
      dplyr::filter(has_coords == 1) %>%
      rowwise() %>%

      ### Match Region
      mutate(admin_col = map_geojson[[admin_distr_col]] [
        which.min(
          sqrt((long - map_geojson$X)^2 + (lat - map_geojson$Y)^2)
        )]
      )

    ### Specify admin_distr_col as main or inset
    if (map_layer == "main") {
      devices_for_map <- devices_for_map %>%
        dplyr::mutate(main.admin_distr_col = admin_col)
    } else {
      devices_for_map <- devices_for_map %>%
        dplyr::mutate(inset.admin_distr_col = admin_col)
    }

    return (devices_for_map)
  }

  return (NULL)
}


#' update_map_params
#' @description Add mode values to map_params
#'
#' @param devices_for_map data.frame : from `monkeyr::get_devices_for_map()`
#' @param map_params list :
#'
#' @returns list : map_params
#'
#' @examples
#' update_map_params  <- update_map_params(
#'   devices_for_map = devices_for_map,
#'   map_params = map_params
#' )
#'
#' @export
update_map_params <- function(devices_for_map, map_params) {
  ### add mode value of admin_distr for each map (main & inset)

  ### function to get modal value of character vector
  getmode <- function(v) {
    uniqv <- unique(v)
    return (uniqv[which.max(tabulate(match(v, uniqv)))])
  }


  ### main.admin_distr_col
  if ("main.admin_distr_col" %in% names(devices_for_map)) {
    modal_val <-  getmode(devices_for_map$main.admin_distr_col)
    map_params$main.admin_distr_modal <- modal_val
  }


  ### inset.admin_distr_col
  if ("inset.admin_distr_col" %in% names(devices_for_map)) {
    modal_val <-  getmode(devices_for_map$inset.admin_distr_col)
    map_params$inset.admin_distr_modal <- modal_val
  }

  return (map_params)
}



#' get_map
#' @description generate map with inset
#'
#' @param devices_for_map data.frame : from `monkeyr::get_devices_for_map()`
#' @param map_params      list :
#' @param geojson_main    MAP :
#' @param geojson_inset   MAP :
#'
#' @returns plot
#'
#' @examples
#' map  <- update_map_params(
#'   devices_for_map = devices_for_map,
#'   map_params = map_params
#' )
#'
#' @export
get_map <- function(devices_for_map, map_params, geojson_main, geojson_inset) {

  tryCatch (
    {
      # ### Remove devices with no location           - already filtered?
      # devices_for_map <- devices_for_map %>%
      #   dplyr::filter(has_coords == 1)


    ### Gradient Limits - Matching Sets - Don't mix up
      ## For maps based on geojson_main
      main_colour_min        <- min(geojson_main[[map_params$main.colour_col]])
      main_colour_max        <- max(geojson_main[[map_params$main.colour_col]]) / map_params$main.colour_depth # makes lighter

      ## For maps based on geojson_inset
      inset_colour_min       <- min(geojson_inset[[map_params$inset.colour_col]])
      inset_colour_max       <- max(geojson_inset[[map_params$inset.colour_col]]) / map_params$inset.colour_depth # makes lighter



### Main Bounding Box
#####################

      bbox_new       <- st_bbox(geojson_main) # current bounding box

      xrange         <- bbox_new$xmax - bbox_new$xmin # range of x values
      yrange         <- bbox_new$ymax - bbox_new$ymin # range of y values

      ## Margin as fraction of full range
      margin_left    <- map_params$margins.left   * xrange
      margin_right   <- map_params$margins.right  * xrange
      margin_top     <- map_params$margins.top    * yrange
      margin_bottom  <- map_params$margins.bottom * yrange

      bbox_new[1]    <- bbox_new[1] - margin_left      # xmin - left margin
      bbox_new[3]    <- bbox_new[3] + margin_right     # xmax + right margin
      bbox_new[2]    <- bbox_new[2] - margin_bottom    # ymin - bottom margin
      bbox_new[4]    <- bbox_new[4] + margin_top       # ymax + top margin
      bbox_new       <- bbox_new %>%  # take the bounding box ...
        st_as_sfc()



### Inset Bounding Box
######################
    ## Ensure clean edges by making other regions white?

    ## 1 - Administrative District Level associated with "Main" Map Data Set
      tas_main <- geojson_main %>%
        filter(.data[[map_params$main.admin_distr_col]] == map_params$main.admin_distr_modal)

    ## 2 - Administrative District Level associated with "Inset" Map Data Set
      tas_inset <- geojson_inset %>%
        filter(.data[[map_params$inset.admin_distr_col]] == map_params$inset.admin_distr_modal)

    ## 3 - Custom based on data points (excluding outliers) : not working.
      ### Get vals
      custom_bbox_vals <- c(
        min(devices_for_map$long),
        min(devices_for_map$lat),
        max(devices_for_map$long),
        max(devices_for_map$lat)
      )
      ### Get Names
      custom_bbox_names <- c(
        "xmin", "ymin", "xmax", "ymax"
      )
      ### Create Named Number & cast as bbox
      tas_custom <- st_bbox(
        setNames(custom_bbox_vals, custom_bbox_names)
      )


    ### Choose Bounding Box
      tas <- tas_inset
      inset_title <- ""
      if (map_params$bbox_src == "main") {
        tas <- tas_main
        inset_title <- map_params$main.admin_distr_modal
      } else if (map_params$bbox_src == "inset"){
        tas <- tas_inset
        inset_title <- map_params$inset.admin_distr_modal
      } else {
        tas <- tas_custom
        inset_title <- map_params$main.admin_distr_modal
      }

      inset_bbox <- st_bbox(tas) %>% st_as_sfc()


    ### Choose Inset Map Data Source
      if (map_params$inset_map_src == "main") {
        country_cropped <- st_crop(geojson_main,  inset_bbox)
      } else {
        country_cropped <- st_crop(geojson_inset,  inset_bbox)
      }


    ### Cities to Include
      cities_main <- world.cities %>%
        filter(country.etc == map_params$countryName) %>%
        filter(pop > map_params$main.city_size)

      cities_inset <- world.cities %>%
        filter(country.etc == map_params$countryName) %>%
        filter(pop > map_params$inset.city_size) %>%
        filter(long > st_bbox(tas)$xmin) %>%
        filter(long < st_bbox(tas)$xmax) %>%
        filter(lat  > st_bbox(tas)$ymin) %>%
        filter(lat  < st_bbox(tas)$ymax)




### Custom theme
################
      default_font_color       <- "#4e4d47"
      default_background_color <- "#ffffff"
      default_font_family      <- "sans"
      default_caption          <- paste0("Source: ")

      theme_map <- function(...) {
        theme_minimal() +
          theme(
            text = element_text(
              family = default_font_family,
              color = default_font_color,
              size = 9),

            ### remove all axes
            axis.line         = element_blank(),
            axis.text.x       = element_blank(),
            axis.text.y       = element_blank(),
            axis.ticks        = element_blank(),

            ### add a subtle grid
            # panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
            panel.grid.major  = element_blank(),
            panel.grid.minor  = element_blank(),

            ### background colors
            plot.background   = element_rect(
              fill = default_background_color,
              color = NA),
            panel.background  = element_rect(
              fill = default_background_color,
              color = NA),
            legend.background = element_rect(
              fill = default_background_color,
              color = NA),

            ### borders and margins
            plot.margin   = unit(c(.5, .5, .2, .5), "cm"),
            panel.border  = element_blank(),
            panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),

            ### titles
            legend.title  = element_text(size = 11),
            legend.text   = element_text(
              size = 8,
              hjust = 0,
              color = default_font_color),
            plot.title    = element_text(
              size = 11,
              hjust = 0.5,
              color = default_font_color),
            plot.subtitle = element_text(
              size = 10,
              hjust = 0.5,
              color = default_font_color,
              margin = margin(
                b = -0.1,
                t = -0.1,
                l = 2,
                unit = "cm"),
              debug = F),

            ### captions
            plot.caption = element_text(
              size = 7,
              hjust = .5,
              margin = margin(
                t = 0.2,
                b = 0,
                unit = "cm"),
              color = "#939184"),
            ...
          )
      }



### Main Map
############
      main_map <- geojson_main %>%
        ggplot() +
        geom_sf(aes(fill=.data[[map_params$main.colour_col]]),
                lwd = 0,
                colour = "white") +
        geom_sf(data = bbox_new, fill = NA, color = "black", size = 0.3) +
        geom_sf(
          data = geojson_inset,
          fill = "transparent",
          color = "white",
          size = 0.2
        ) +
        #geom_text_repel(aes(X, Y, label = .data[[map_params$main.admin_distr_col]]), max.overlaps=10,size = 1.8,force=2,force_pull=2) +
        ggspatial::annotation_scale(location = "br", width_hint = 0.2) + ## GGSPATIAL FUNCTION
        theme_void() +
        coord_sf(expand = FALSE) +

        scale_fill_gradientn(
          limits = c(main_colour_min, main_colour_max),
          colours=brewer.pal(7, map_params$main.palette),

          name = "Area Square KM",
          # here we use guide_colourbar because it is still a continuous scale
          guide = guide_colorbar(
            direction = "horizontal",
            barheight = unit(2, units = "mm"),
            barwidth = unit(50, units = "mm"),
            draw.ulim = F,
            title.position = 'top',
            # some shifting around
            title.hjust = 0.7,
            label.hjust = 0.7
          )) +
        geom_point(data = devices_for_map, aes(x = long, y = lat,colour = connected),size=0.4) +
        scale_color_manual(values = c("TRUE" = "#069C36", "FALSE" = "#C03653")) +

        ### cities
        geom_point(data = cities_main, aes(x = long, y = lat),size=0.5) +
        #geom_text_repel(aes(X, Y, label = .data[[map_params$main.admin_distr_col]]), size = 1.3,max.overlaps =20) +
        geom_text_repel(
          data = cities_main,
          aes(x=.data[['long']], y=.data[['lat']], label = .data[['name']]),
          max.overlaps=10,
          max.time = 0.3,
          max.iter=1000,
          size = 2.0,
          force=2,
          force_pull=2) +

        labs(title = paste0('Connection Status across ', map_params$countryName),
             x = "", y = "") +
        geom_rect(
          xmin = st_bbox(tas)[1],
          ymin = st_bbox(tas)[2],
          xmax = st_bbox(tas)[3],
          ymax = st_bbox(tas)[4],
          fill = NA,
          colour = "black",
          size = 0.6
        ) +
        theme_map() +
        theme(legend.position = "none") +
        guides(colour = "none")



### Insert Map
##############
      country_cropped <- st_crop(geojson_inset,  inset_bbox)
      inset_map <- country_cropped %>%
        mutate(fill_num = row_number()) %>%
        ggplot() +
        geom_sf(aes(fill=.data[[map_params$inset.colour_col]]),
                lwd = 0,
                colour = "white") +

        geom_sf(data = country_cropped,fill = "transparent",color = "white",size = 0.2) +
        scale_fill_gradientn(
          limits = c(inset_colour_min, inset_colour_max),
          colours= brewer.pal(7, map_params$inset.palette),


          name = "Area Square KM",
          # here we use guide_colourbar because it is still a continuous scale
          guide = guide_colorbar(
            direction = "horizontal",
            barheight = unit(2, units = "mm"),
            barwidth = unit(50, units = "mm"),
            draw.ulim = F,
            title.position = 'top',
            # some shifting around
            title.hjust = 0.7,
            label.hjust = 0.7
          )) +

        geom_sf(data = inset_bbox, fill = NA, color = "black", size = 0.4) +

        ### label names in inset
        geom_text_repel(aes(X, Y, label = .data[[map_params$inset.admin_distr_col]]), size = 1.0, max.overlaps = 10) +
        geom_point(data = devices_for_map, aes(x = long, y = lat,colour = factor(connected)),size=1.0)+
        scale_color_manual(values = c("TRUE" = "#069C36", "FALSE" = "#C03653")) +


        ### cities
        geom_point(data = cities_inset, aes(x = long, y = lat),size=0.5) +
        geom_text_repel(
          data = cities_inset,
          aes(x=.data[['long']], y=.data[['lat']], label = .data[['name']]),
          max.overlaps=10,
          max.time = 0.3,
          max.iter=1000,
          size = 2.0,
          force=2,
          force_pull=2) +

        theme_void()+
        coord_sf(
          xlim=st_bbox(tas)[c(1,3)],
          ylim = st_bbox(tas)[c(2,4)],
          expand = FALSE) +
        theme(legend.position = "bottom") +
        theme(text = element_text(size = 9,family = 'sans')) +
        ggtitle(inset_title) +
        theme(plot.title = element_text(hjust = 0.5, vjust = 3)) +
        guides(
          fill = "none",
          col = guide_legend(
            "Connected to Network",
            title.position = 'top',
            # shift around legend text
            title.hjust = 0.7,
            label.hjust = 0.5
          ),
        )
      # message(paste0("\nGET_MAP 6"))




### Combine Maps
################
      main_inset_map <- cowplot::ggdraw() +
        cowplot::draw_plot(main_map) +
        cowplot::draw_plot(
          inset_map,
          x      = map_params$insert.x,       # 0.1
          y      = map_params$insert.y,       # 0.1
          width  = map_params$insert.width,   # 0.2
          height = map_params$insert.height   # 0.2
        )

      return (main_inset_map)


    ## Error Handler
    },
    error = function(cond) {
      # monkeyr::monkey_knit_error(err = cond, resource = "get_map", debug = TRUE) ## Completely Bail!
      monkeyr::monkey_knit_msg(msg = cond, resource = "get_map", debug = TRUE)     ## Handle Error
    }
  )
}



