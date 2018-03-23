require(readr)
require(dplyr)
require(tidyr)
require(geosphere)
require(ggmap)
require(stringr)


#' Load hurricane track and windspeed data
#' 
#' This function reads in hurricane data from a fixed width file and does not tidy the dataset.
#' 
#' @importFrom readr read.fwf fwf_widths
#'
#' @param filename a character vector of filename of hurricane data file to read
#' 
#' @example 
#'   load_data("data/ebtrk_atlc_1988_2015.txt")
#' 
#' @export
load_data = function(filename) {
  
  ext_tracks_widths = c(7, 10, 2, 2, 3, 5, 5, 6, 4, 5, 4, 4, 5, 3, 4, 3, 3, 3,
                        4, 3, 3, 3, 4, 3, 3, 3, 2, 6, 1)
  ext_tracks_colnames = c("storm_id", "storm_name", "month", "day",
                          "hour", "year", "latitude", "longitude",
                          "max_wind", "min_pressure", "rad_max_wind",
                          "eye_diameter", "pressure_1", "pressure_2",
                          paste("radius_34", c("ne", "se", "sw", "nw"), sep = "_"),
                          paste("radius_50", c("ne", "se", "sw", "nw"), sep = "_"),
                          paste("radius_64", c("ne", "se", "sw", "nw"), sep = "_"),
                          "storm_type", "distance_to_land", "final")
  
  ext_tracks = readr::read_fwf(filename, 
                               fwf_widths(ext_tracks_widths, ext_tracks_colnames),
                               na = "-99")
  
  return(ext_tracks)
  
}

#' Tidy hurricane track and windspeed data
#' 
#' {clean_data} rearranges hurricane data and cleans values and formatting
#' 
#' @importFrom stringr str_c str_to_title
#' @importFrom dplyr mutate_ select_
#' @importFrom tidyr gather spread
#' 
#' @param data full hurricane dataset to tidy
#' 
#' @export
clean_data = function(data) {
  data %>% 
    dplyr::mutate_(storm_id = ~stringr::str_c(stringr::str_to_title(storm_name), year, sep = '-'),
                   date = ~stringr::str_c(year, '-', month, '-', day, ' ', hour, ':', '00', ':', '00'),
                   longitude = ~-longitude
    ) %>% 
    
    dplyr::select_(.dots = c('storm_id', 'date', 'longitude', 'latitude', 
                             'radius_34_ne', 'radius_34_se', 'radius_34_sw', 'radius_34_nw',
                             'radius_50_ne', 'radius_50_se', 'radius_50_sw', 'radius_50_nw',
                             'radius_64_ne', 'radius_64_se', 'radius_64_sw', 'radius_64_nw')
    ) %>%
    tidyr::gather(variable, value, -storm_id, -date,-latitude, -longitude, -storm_id, -date) %>% mutate_(wind_speed = ~str_extract(variable, "(34|50|64)"),
                                                                                                         variable = ~str_extract(variable, "(ne|nw|se|sw)")) %>% tidyr::spread(variable, value) %>% select_(.dots = c('storm_id', 'date', 'latitude', 'longitude', 'wind_speed', 'ne', 'nw', 'se', 'sw'))
}

#' Filter the dataset to specific hurricane and date
#' 
#' {clean_data} is used to select a specific hurricane and datetime to visualize
#' 
#' @importFrom dplyr filter_
#' 
#' @param data hurricane fulldataset
#' @param hurricane filters hurricane ID
#' @param observation filters datetime
#' 
#' @export
#'
filter_data = function(data, hurricane, observation) {
  data = filter_(data, ~storm_id == hurricane & date == observation)
  
}

#' Creates layer for proto
#' 
#' @importFrom ggplot2 layer
#' 
#' @param mapping Aesthetic mappings.
#' @param data The hurricane data to include in the layer.
#' @param stat The statistical transformation to use on the data for this layer, as a string.
#' @param position Adjust position.
#' @param na.rm If FALSE, the default, missing values are removed with a warning. If TRUE, missing values are silently removed.
#' @param show.legend logical. Indicates whether the legend should be included
#' @param inherit.aes FALSE overrides the default aesthetics
#' 
#' @export
geom_hurricane = function(mapping = NULL, data = NULL, stat = 'identity',
                          position = 'identity', na.rm = FALSE, show.legend = NA, inherit.aes = TRUE, ...) {
  ggplot2::layer(
    geom = geom_hurricane_proto, mapping = mapping, data = data, stat = stat, position = position, 
    show.legend = show.legend, inherit.aes = inherit.aes, params = list(na.rm = na.rm, ...)
  )
}


#' Create geom_hurricane and functions to correctly format wind radii
#' 
#' @importFrom ggplot2 ggproto
#' @importFrom dplyr bind_rows rename_ mutate_
#' @importFrom grid polygonGrob gpar 
#' 
#' @param required_aes required aesthetics for the geom_hurricane function
#' @param default_aes default values for aesthetics
#' @param draw_key the function to draw the legend with the associated geom
#' @param draw_group build polygons
#' 
#' @examples
#'   geom_hurricane(data = storm_observation, aes(x = longitude, y = latitude, r_ne = ne, r_se = se, r_nw = nw, r_sw = sw, fill = wind_speed, color = wind_speed)
#' 
#' @export

m_adj_constant<- 1852
  
geom_hurricane_proto <- ggplot2::ggproto("geom_hurricane_proto", Geom,
                                         required_aes = c("x", "y",
                                                          "r_ne", "r_se", "r_nw", "r_sw"
                                         ),
                                         default_aes = aes(fill = 1, colour = 1, alpha = 1, scale_radii = 1),
                                         draw_key = draw_key_polygon,
                                         draw_group = function(data, panel_scales, coord) {
                                           
                                           coords <- coord$transform(data, panel_scales)
                                           
                                           # convert to meters and scale
                                           data <- data %>% mutate_(r_ne = ~r_ne*m_adj_constant*scale_radii,
                                                                    r_se = ~r_se*m_adj_constant*scale_radii,
                                                                    r_sw = ~r_sw*m_adj_constant*scale_radii,
                                                                    r_nw = ~r_nw*m_adj_constant*scale_radii
                                           )
                                           
                                           # calculate points and values
                                           for (i in 1:nrow(data)) {
                                             
                                             # Create the Northwest Quandrant
                                             df_nw <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 270:360,
                                                                                            d = data[i,]$r_nw),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
                                             # NE
                                             df_ne <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 1:90,
                                                                                            d = data[i,]$r_ne),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
                                             # SE
                                             df_se <- base::data.frame(colour = data[i,]$colour,
                                                                       fill = data[i,]$fill,
                                                                       geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                            b = 90:180,
                                                                                            d = data[i,]$r_se),
                                                                       group = data[i,]$group,
                                                                       PANEL = data[i,]$PANEL,
                                                                       alpha = data[i,]$alpha
                                             )
                                             
                                             # SW
                                             df_sw <- data.frame(colour = data[i,]$colour,
                                                                 fill = data[i,]$fill,
                                                                 geosphere::destPoint(p = c(data[i,]$x, data[i,]$y),
                                                                                      b = 180:270,
                                                                                      d = data[i,]$r_sw),
                                                                 group = data[i,]$group,
                                                                 PANEL = data[i,]$PANEL,
                                                                 alpha = data[i,]$alpha
                                             )
                                             
                                             # combine quadrants
                                             df_points <- dplyr::bind_rows(list(df_nw, df_ne, df_se, df_sw))
                                             
                                           }
                                           
                                           df_points <- df_points %>% dplyr::rename_('x' = 'lon',
                                                                                     'y' = 'lat'
                                           )

                                           df_points$colour <- base::as.character(df_points$colour)
                                           df_points$fill <- base::as.character(df_points$fill)
                                           
                                           coords_df <- coord$transform(df_points, panel_scales)
                                           
                                           ## build polygon
                                           grid::polygonGrob(
                                             x= coords_df$x,
                                             y = coords_df$y,
                                             gp = grid::gpar(col = coords_df$colour, fill = coords_df$fill, alpha = coords_df$alpha)
                                           )
                                         }
)

# import full hurricane dataset and limit to Hurricane Ike while it was over the Caribbean
single_hurricane = load_data('data/ebtrk_atlc_1988_2015.txt') %>% 
  clean_data() %>% 
  filter_data(hurricane = 'Ike-2008', observation = '2008-09-08 12:00:00')

# build map for single observation
map_ike= get_map("Cuba", zoom = 5, maptype = "toner-background") %>%
  ggmap(extent = "device") +
  geom_hurricane(data = single_hurricane,
                 aes(x = longitude, y = latitude, r_ne = ne, r_se = se, r_nw = nw, r_sw = sw,
                     fill = wind_speed, color = wind_speed)) + 
  scale_color_manual(name = "Wind Speed", values = c("red", "orange", "yellow")) + 
  scale_fill_manual(name = "Wind Speed", values = c("red", "orange", "yellow"))


