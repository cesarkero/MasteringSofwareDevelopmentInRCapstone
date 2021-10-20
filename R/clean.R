#' @title eq_clean_data
#'
#' @description This function cleans data previously downloaded from
#' 'https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data'.
#' The raw downloaded data have been stored in in data-raw
#' The cleaned data contains: (1) filter out of NA years, (2) Date as a composition of Year-Mo-Dy
#'
#' @param NOAArawData raw data input
#'
#' @return df object with the cleaned data
#' @examples
#' \dontrun{
#' df <- eq_clean_data(NOAArawData='./data_raw/earthquakes-2021-10-06_09-24-38_+0200.tsv')
#' }
#' @export
#'
eq_clean_data <- function(NOAArawData='./data_raw/earthquakes-2021-10-06_09-24-38_+0200.tsv'){
    # Url to download --> 'https://www.ngdc.noaa.gov/hazel/view/hazards/earthquake/event-data'

    # Read data
    # NOAArawData <- './data_raw/earthquakes-2021-10-06_09-24-38_+0200.tsv'
    data <- NOAArawData
    d <- readr::read_delim(data)

    # Create DATE in Date class format
    d1 <- d %>%
        dplyr::filter(!is.na(Year)) %>%
        dplyr::mutate(Mo = ifelse(is.na(Mo),1,Mo), Dy = ifelse(is.na(Dy),1,Dy)) %>%
        dplyr::mutate(Date = ymd(paste0('0000', '-', Mo, '-', Dy)) + years(Year))

    # Convert LATITUDE AND LONGITUDE to numeric class
    return(as.data.frame(d1))
}

#-------------------------------------------------------------------------------
#' @title eq_location_clean
#'
#' @description In addition, write a function eq_location_clean() that cleans the LOCATION_NAME
#' column by stripping out the country name (including the colon) and converts names to title case (as opposed to all caps)
#'
#' @param df data.frame from eq_clean_data()
#'
#' @return df object with the cleaned data
#'
#' @examples
#' \dontrun{
#' dfclean <- eq_location_clean(df)
#' }
#'
#' @export
eq_location_clean <- function(df){
    # Remove country: and extra spaces and convert to title

    df <- df %>%
        dplyr::mutate(LOCATION_NAME = stringr::str_trim(gsub("^.*:", '', df$`Location Name`))) %>%
        dplyr::mutate(LOCATION_NAME = stringr::str_to_title(LOCATION_NAME)) %>%
        dplyr::mutate(COUNTRY = stringr::str_trim(gsub(":.*$", '', df$`Location Name`)))

    return(df)
}
