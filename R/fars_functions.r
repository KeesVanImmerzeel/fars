#@import readr dplyr tidyr maps graphics

# ----------------------------------------------------------------------------
#' @title Read FARS data
#'
#' @description
#' Read Fatality Analysis Reporting System (FARS) data in a data data frame tbl.
#'   This function is not exported.
#' @param filename (character)
#' @return data frame tbl
#' @details This function throws an error message if the file does not exist.
# @importFrom readr read_csv
# @importFrom dplyr tbl_df
#' @examples
#' \dontrun{fars_read(system.file("extdata","accident_2013.csv.bz2",package="fars"))}
#' @seealso \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
fars_read <- function(filename) {
   if (!file.exists(filename))
      stop("file '", filename, "' does not exist")
   data <- suppressMessages({
      readr::read_csv(filename, progress = FALSE)
   })
   dplyr::tbl_df(data)
}

# ----------------------------------------------------------------------------
#' @title Create FARS filename from specified year: 2013, 2014 or 2015
#'
#' @description
#' For a specified year, create the filename to read the (compressed) Fatality Analysis
#'   Reporting System (FARS) data.
#'   This function is not exported.
#' @param year (integer)
#' @return filename (character)
#' @details Also prints the created filename.
#' @examples
#' \dontrun{make_filename(2013)}
#' @seealso \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
make_filename <- function(year = 2013 ) {
   year <- as.character(year)
   year <- match.arg(year, c("2013", "2014", "2015"))
   year <- as.integer(year)
   system.file("extdata",sprintf("accident_%d.csv.bz2", year),package="fars")
}

# ----------------------------------------------------------------------------
#' @title Read multiple years of FARS data
#'
#' @description
#' For specified years, read the MONTH and year values of the Fatality Analysis Reporting System (FARS)
#'   data in a list of tibbles.
#'   Only data for the years 2013, 2014 or 2015 is available.
#'   This function is not exported.
#' @param years (integer vector)
#' @return list of tibbles
#' @details Throws an error if data for specified year does not exist.
# @importFrom dplyr mutate
# @importFrom dplyr select
#' @examples
#' \dontrun{fars_read_years(c(2013,2014))}
#' @seealso \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
fars_read_years <- function(years = c(2013, 2014)) {
   lapply(years, function(year) {
      file <- make_filename(year)
      tryCatch({
         dat <- fars_read(file)
         dplyr::mutate(dat, year = year) %>%
            dplyr::select(MONTH, year)
      }, error = function(e) {
         warning("invalid year: ", year)
         return(NULL)
      })
   })
}

# ----------------------------------------------------------------------------
#' @title Number of fatal accidents per month in FARS data
#'
#' @description
#' From specified years, return the number of fatal accidents per month
#'   as included in the (FARS) data.
#'   Only data for the years 2013, 2014 or 2015 is available.
#' @param years (integer vector)
#' @return Data frame with columns month, and the number of fatal accidents for the specified years.
#' @details Throws an error if data for specified year does not exist.
# @importFrom dplyr bind_rows
# @importFrom dplyr group_by
# @importFrom dplyr summarize
# @importFrom tidyr spread
#' @examples
#' \dontrun{fars_read_years(c("2013","2014"))}
#' @seealso \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#' @export
fars_summarize_years <- function(years = c(2013, 2014)) {
   dat_list <- fars_read_years(years)
   dplyr::bind_rows(dat_list) %>%
      dplyr::group_by(year, MONTH) %>%
      dplyr::summarize(n = n()) %>%
      tidyr::spread(year, n)
}

# ----------------------------------------------------------------------------
#' @title Create map of fatal accidents
#'
#' @description
#' From specified year and state number, create a map of the accidents
#'   as included in the (FARS) data.
#'   Only data for the years 2013, 2014 or 2015 is available.
#' @param state.num state number (numeric, 1-56)
#' @param year (numeric)
#' @return NULL
#' @details Throw an error if the state number is invalid or there are no accidents to plot
# @importFrom dplyr filter
# @importFrom maps map
# @importFrom graphics points
#' @examples
#'  \dontrun{fars_map_state(1,2013)}
#' @seealso \url{https://www.nhtsa.gov/Data/Fatality-Analysis-Reporting-System-(FARS)}
#' @export
fars_map_state <- function(state.num=1, year=2013) {
      filename <- make_filename(year)
      data <- fars_read(filename)
      state.num <- as.integer(state.num)

      if(!(state.num %in% unique(data$STATE)))
            stop("invalid STATE number: ", state.num)
      data.sub <- dplyr::filter(data, STATE == state.num)
      if(nrow(data.sub) == 0L) {
            message("no accidents to plot")
            return(invisible(NULL))
      }
      is.na(data.sub$LONGITUD) <- data.sub$LONGITUD > 900
      is.na(data.sub$LATITUDE) <- data.sub$LATITUDE > 90
      with(data.sub, {
            maps::map("state", ylim = range(LATITUDE, na.rm = TRUE),
                      xlim = range(LONGITUD, na.rm = TRUE))
            graphics::points(LONGITUD, LATITUDE, pch = 46)
      })
}
