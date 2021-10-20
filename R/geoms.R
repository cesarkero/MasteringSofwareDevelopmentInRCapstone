#-------------------------------------------------------------------------------
# 1. EARTHQUAKE PLOT
#-------------------------------------------------------------------------------
#' eqpoints
#' @title eqpoints
#' @description This functions is the geom config form a geom_timeline function, where a timeline
#' of a selected earthquakes (by period and country) are shown
#'
#' @param Geom geom for ggplot
#' @param required_aes required_aes
#' @param default_aes default_aes
#' @param drawn_key drawn_key (default)
#' @param draw_panel draw_panel (default)
#'
#' @return default function for ggplot representation (time series of earthquakes)
#'
#' @examples
#' \dontrun{
#' df <- eq_clean_data(NOAArawData='./data_raw/earthquakes-2021-10-06_09-24-38_+0200.tsv')
#' }
#' @export
#'

# ojo , falta por hacer dinamico el cambio de tamaño en los puntos...
eqpoints <- ggplot2::ggproto("eqpoints", ggplot2::Geom,
                    required_aes = c("x", "y"),
                    default_aes = ggplot2::aes(y = 0.2,
                                      shape = 19,
                                      size = 10,
                                      colour = "gray",
                                      alpha = 0.25,
                                      stroke = 0.3,
                                      fill = 'NA'),
                    draw_panel = function(data, panel_scales, coord) {
                        ## Transform the data first
                        coords <- coord$transform(data, panel_scales)

                        ## Construct a grid grob 01
                        grid::pointsGrob(
                            x = coords$x,
                            y = coords$y,
                            pch = coords$shape,
                            gp = grid::gpar(col=alpha(coords$colour, coords$alpha),
                                            fill = coords$colour. ),
                        )
                    }
)

#' geom_timeline
#' @title geom_timeline
#' @description This function allows the user a faster representation of data
#' based on a predefined layout of a ggplot2
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#'
#' @return ggplot representation (time series of earthquakes)
#'
#' @examples
#' \dontrun{
#' dfs <- df %>%
#' dplyr::filter(Year>2015 & Year<2021 & (COUNTRY =='CHINA' | COUNTRY =='GREECE'))
#'
#' p1 <- dfs %>%
#' ggplot() +
#' geom_timeline(aes(x = Date,
#'                   y = COUNTRY,
#'                   size = Mag,
#'                   colour = Deaths)) +
#'                   theme_minimal()+
#'                   theme(legend.position="bottom")
#'                   p1
#' }
#' @export
#'
geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = "identity",
                          position = "identity",
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = eqpoints,
        mapping = mapping,
        data = data,
        stat = stat,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}

#-------------------------------------------------------------------------------
# 2. LINES AND LOCATIONS
#-------------------------------------------------------------------------------
#' @name eqloc
#' @title eqloc
#' @description
#'
#' @param Geom geom for ggplot
#' @param required_aes required_aes
#' @param default_aes default_aes
#' @param draw_panel draw_panel (default)
#'
#' @return default function for ggplot representation (lines with locations)
#'
#' @export
#'
eqloc <- ggplot2::ggproto("eqloc", ggplot2::Geom,
                 required_aes = c("x"),

                 default_aes = ggplot2::aes(y = 0.5,
                                            locations = NA,
                                            label = NA,
                                            number = NULL,
                                            max_aes = NULL),

                 draw_panel = function(data, panel_scales, coord) {

                     # Transform the data
                     coords <- coord$transform(data, panel_scales)

                     # Locate text in plot
                     lines <- grid::segmentsGrob(x0 = grid::unit(coords$x, "npc"),
                                                 y0 = grid::unit(coords$y, "npc"),
                                                 x1 = grid::unit(coords$x, "npc"),
                                                 y1 = grid::unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
                                                 default.units = "npc",
                                                 arrow = NULL,
                                                 name = NULL,
                                                 gp = grid::gpar(col=alpha('grey', 0.9)),
                                                 vp = NULL)

                     # Add text to line
                     text <- grid::textGrob(label = coords$locations,
                                            x = unit(coords$x, "npc"),
                                            y = unit(coords$y + 0.06/length(unique(coords$y)), "npc"),
                                            rot = 60,
                                            just = "left",
                                            gp = grid::gpar(fontsize = 8))

                     # Plot over plot
                     grid::gTree(children = grid::gList(lines, text))
                 }
)

#' geom_eqloc
#' @title geom_eqloc
#' @description This functions adds lines with the locations to the graph created with geom_timeline() function
#'
#' @param mapping mapping
#' @param data data
#' @param stat stat
#' @param position position
#' @param na.rm na.rm
#' @param show.legend show.legend
#' @param inherit.aes inherit.aes
#'
#' @return ggplot with lines and locations of the main earth quackes represented with geom_timeline()
#'
#' @examples
#' \dontrun{
#' p2 <- p1 + geom_eqloc(aes(x = Date, y = COUNTRY, locations = LOCATION_NAME, number = 3))
#' p2
#' }
#' @export
#'
geom_eqloc <- function(mapping = NULL,
                       data = NULL,
                       na.rm = TRUE,
                       show.legend = NA,
                       stat = "identity",
                       position = "identity",
                       inherit.aes = TRUE, ...) {
    ggplot2::layer(
        geom = eqloc,
        mapping = mapping,
        data = data,
        stat = stat,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(na.rm = na.rm, ...)
    )
}



#-------------------------------------------------------------------------------
# 3. MAP REPRESENTATION
#-------------------------------------------------------------------------------
#' eq_map
#' @title function to create a map with the selected eq points
#' @description This functions creates a map based lon longitud and latitud and
#' sizes the points based in Mag
#'
#' @param df_map df_map
#' @param annot_col annot_col
#'
#' @return leaflet map of a dataframe where eq are represented
#'
#' @examples
#' \dontrun{
#' p3 <- dfs %>% eq_map(annot_col = "Date")
#' p3
#' }
#'
#' @export
#'
eq_map <- function(df_map = NULL, annot_col) {

    # Using Leaflet to plot a map.
    leaflet::leaflet(df_map) %>%
        leaflet::addTiles() %>%
        # Adding circles in each Earthquake point.
        leaflet::addCircles(lng = ~Longitude,
                            lat = ~Latitude,
                            weight = 1,
                            radius = ~Mag * 25000,
                            # Plotting a simple information inside of the popup.
                            popup = ~eval(parse(text = annot_col)) ) -> map_to_plot

    # Returning the map with circles.
    return(map_to_plot)
}





#' eq_create_label
#' @title eq_create_label
#' @description This functions add a new popup to the map created with eq_map
#' using html notation to show clean and organize data in points
#'
#' @param df just use this function in the dplyr pipeline to add the poup
#'
#' @return new popup of points
#'
#' @examples
#' \dontrun{
#' dfs %>%
#' dplyr::mutate(popup = eq_create_label(.)) %>%
#' eq_map(annot_col = "popup")
#' }
#' @export
#'
eq_create_label <- function(df) {

    #Creating the "popup_text" without using NA Labels
    df<- df %>% select_(.dots=c('LOCATION_NAME','Mag','Deaths')) %>%
        dplyr::mutate(t1 = paste0("<b>Location:</b> ", LOCATION_NAME,"<br />"),
               t2 = paste0("<b>Magnitude:</b> ", Mag,"<br />"),
               t3 = paste0("<b>Deaths:</b> ", Deaths,"<br />")) %>%
        unite('popup',c('t1','t2','t3'), sep='')

    popup <- dplyr::collect(dplyr::select(df,.dots=c('popup')))[[1]]

    return(popup)
}
