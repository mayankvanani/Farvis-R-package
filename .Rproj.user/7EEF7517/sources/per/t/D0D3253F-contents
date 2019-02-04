#' Import Dataset - US NHTSA Fars Data (Fatality Analysis Reporting System)
#'
#' This function check the data is available in the specified directory and
#' then imports it to global environment by read it in .CSV format.
#'
#' @importFrom readr read_csv
#' @importFrom dplyr tbl_df
#'
#' @param filename A character string specifying just the filename if it resides in current
#'                 working directory, else,  specifying the absolute path to the file.
#'
#' @return This function returns DataFrame tbl class for the dataset.This function stops
#'         executing if the file does not exits in the specified directory and throws error
#'         message "file <filename> does not exists".
#'
#' @examples
#' fars_read("fars_data")
#' fars_read("data/fars_data")
#' @export

fars_read <- function(filename) {
  if(!file.exists(filename))
    stop("file '", filename, "' does not exist")
  data <- suppressMessages({
    readr::read_csv(filename, progress = FALSE)
  })
  dplyr::tbl_df(data)
}

#' Prints "accident_<year>.csv.bz2"
#'
#' This function takes the year as input an prints the formatted string "accident_year.csv.bz2"
#'
#' @param year Takes number as input which will be used in formatted string literal. Would throw an error
#'             if alphabets is provided as input argument
#'
#' @return returns formatted string as output.
#'
#' @examples
#' make_file(2019)
#' make_file('2019')
#' make_file("2019")
#' @export
make_filename <- function(year) {
  year <- as.integer(year)
  system.file("extdata",
              sprintf("accident_%d.csv.bz2", year),
              package = "farviz",
              mustWork = TRUE)
}
#' Selects the data based on 'month' an saves as separate file extension - .csv.bz2 file
#'
#' This function uses base R function 'lapply' and also uses function - 'mutate', 'select'
#' from "dplyr" package. Takes years as arguments, create a formatted string object used for
#' naming the saved file and finally, selects the 'MONTH' and 'year' column and assigns it to
#' "dat" object. Returns error if the "year" is not found.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @import magrittr
#'
#' @param years  Takes number as input which will be used in formatted string literal.
#'               Would throw an error if alphabets is provided as input argument.
#'
#' @return A compressed file containing dataset for the the specified month.
#'
#' @note
#' If the specified year is not there in the dataset, It throws a warning: "invalid year: <year>" and
#'  returns NULL. It would return NULL in case of alphbet being passed as an argument.
#'
#' @examples
#' fars_read_years(2015)
#' fars_read_years('2015')
#' fars_read_years("2015")
#'
#' @export
fars_read_years <- function(years) {
  lapply(years, function(year) {
    file <- make_filename(year)
    tryCatch({
      dat <- fars_read(file)
      dplyr::mutate_(dat,  year = "YEAR") %>%
        dplyr::select_("MONTH", "year")
    }, error = function(e) {
      warning("invalid year: ", year)
      return(NULL)
    })
  })
}

#' Summarises and spread the data for a specified year
#'
#' This function uses packages: "dplyr" and "tidyr". It reads the data for specified
#' year and summarises it, giving various metrics also spreads the data for better visualisation.
#'
#' @importFrom dplyr bind_rows
#' @importFrom dplyr group_by_
#' @importFrom dplyr summarize_
#' @importFrom tidyr spread_
#' @importFrom magrittr "%>%"
#'
#' @inheritParams fars_read_years
#'
#' @return This function returns a spreaded data for the specified year for visualisation.
#'
#' @examples
#' fars_summarize_years(2015)
#' fars_summarize_years('2015')
#' fars_summarize_years("2015")
#'
#' @export
fars_summarize_years <- function(years) {
  dat_list <- fars_read_years(years)
  dplyr::bind_rows(dat_list) %>%
    dplyr::group_by_("year", "MONTH") %>%
    dplyr::summarize_(n = "n()") %>%
    tidyr::spread_("year", "n")
}

#' Plots the accidents on a map for specified state and year
#'
#' This function takes 'state number' and 'year' as input and verifies if its a valid state
#' number and then check if the number of accidents is NONE or not. If not then reads the
#' latitude and longitude and plots it on graphical maps
#'
#' @param state.num Input is a numerical value which references a state
#' @param year Input should be numerical value denoting year.
#'
#' @importFrom maps map
#' @importFrom dplyr filter_
#' @importFrom graphics points
#'
#' @returns It return NULL but plots a map with accidents sites plotted based on the latitude and longitudes.
#'
#' @note
#'  It throws an error if state number is not valid and prints"invalid state number" state.num"
#'  It returns a message "no accidents found" if number of accidents for specified state and year is 0.
#'
#' @examples
#' fars_map_state(1,2015)
#' fars_map_state('1','2015')
#' fars_map_state("1","2015")
#'
#' @export
fars_map_state <- function(state.num, year) {
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
