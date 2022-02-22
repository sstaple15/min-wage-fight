#' Load data from the American Community Survey Public Use Microdata Series API
#'
#' @param variables A vector of variables from the PUMS API.
#'   Use \code{View(pums_variables)} to browse variable options.
#' @param state A state, or vector of states, for which you would like to
#'   request data.  The entire US can be requested with \code{state = "all"} - though be patient with the data download!
#' @param puma A vector of PUMAs from a single state, for which you would like
#'   to request data. To get data from PUMAs in more than one state, specify a
#'   named vector of state/PUMA pairs and set \code{state = "multiple"}.
#' @param year The data year of the 1-year ACS sample or the endyear of the
#'   5-year sample. Defaults to 2019.
#' @param survey The ACS survey; one of either \code{"acs1"} or \code{"acs5"}
#'   (the default).
#' @param variables_filter A named list of filters you'd like to return from the
#'   PUMS API.  For example, passing \code{list(AGE = 25:50, SEX = 1)} will return
#'   only males aged 25 to 50 in your output dataset.  Defaults to \code{NULL},
#'   which returns all records. If a housing-only dataset is required,
#'   use \code{list(SPORDER = 1)} to only return householder records (taking care
#'   in your analysis to use the household weight \code{WGTP}).
#' @param rep_weights Whether or not to return housing unit, person, or both
#'   housing and person-level replicate weights for calculation of standard
#'   errors; one of \code{"person"}, \code{"housing"}, or \code{"both"}.
#' @param recode If TRUE, recodes variable values using Census data dictionary
#'   and creates a new \code{*_label} column for each variable that is recoded.
#'   Available for 2017 - 2019 data. Defaults to FALSE.
#' @param show_call If TRUE, display call made to Census API. This can be very
#'   useful in debugging and determining if error messages returned are due to
#'   tidycensus or the Census API. Copy to the API call into a browser and see
#'   what is returned by the API directly. Defaults to FALSE.
#' @param key Your Census API key. Obtain one at
#'   \url{https://api.census.gov/data/key_signup.html}
#'
#' @return A tibble of microdata from the ACS PUMS API.
#' @export
#'
#' @examples
#' \dontrun{
#' get_pums(variables = "AGEP", state = "VT")
#' get_pums(variables = "AGEP", state = "multiple", puma = c("UT" = 35008, "NV" = 00403))
#' get_pums(variables = c("AGEP", "ANC1P"), state = "VT", recode = TRUE)
#' get_pums(variables = "AGEP", state = "VT", survey = "acs1", rep_weights = "person")
#' }
#'
get_pums_pr <- function(variables = NULL,
                     state = NULL,
                     puma = NULL,
                     year = 2019,
                     survey = "acs5",
                     variables_filter = NULL,
                     rep_weights = NULL,
                     recode = FALSE,
                     show_call = FALSE,
                     key = NULL) {
  
  if (is.null(state)) {
    stop("You must specify a state by name, postal code, or FIPS code. To request data for the entire United States, specify `state = 'all'`.", call. = FALSE)
  }
  
  if (survey == "acs1") {
    message(sprintf("Getting data from the %s 1-year ACS Public Use Microdata Sample",
                    year))
  } else if (survey == "acs5") {
    startyear <- year - 4
    message(sprintf("Getting data from the %s-%s 5-year ACS Public Use Microdata Sample",
                    startyear, year))
  }
  
  if (Sys.getenv('CENSUS_API_KEY') != '') {
    
    key <- Sys.getenv('CENSUS_API_KEY')
    
  } else if (is.null(key)) {
    
    stop('A Census API key is required.  Obtain one at http://api.census.gov/data/key_signup.html, and then supply the key to the `census_api_key()` function to use it throughout your tidycensus session.')
    
  }
  
  # Account for missing PUMAs in 2008-2012 through 2011-2015 ACS samples
  if (year %in% 2012:2015 && survey == "acs5" && (!is.null(puma) || "PUMA" %in% variables)) {
    stop("PUMAs are not available for end-years between 2012 and 2015 due to inconsistent PUMA boundary definitions.", call. = FALSE)
  }
  
  
  # Allow users to get all states by specifying "all"
  # To avoid large data timeout errors, iterate through the states instead.
  if (all(state == "all")) {
    
    state_codes <- unique(fips_codes$state_code)[1:51]
    
    message("Requesting data for the entire United States by state then combining the result.\nThis can take several minutes to complete.\nFor large data extract workflows, consider downloading data from the Census FTP server\nor from IPUMS (https://ipums.org) instead.")
    output <- purrr::map_dfr(state_codes, function(each_state) {
      suppressMessages(get_pums(
        state = each_state,
        variables = variables,
        puma = NULL,
        year = year,
        survey = survey,
        variables_filter = variables_filter,
        rep_weights = rep_weights,
        recode = recode,
        show_call = TRUE,
        key = key
      ))
    })
    
    return(output)
    
  }
  
  # If variables is NULL, initialize a length-0 vector to store
  # the required variables eventually
  if (is.null(variables)) {
    variables <- c()
  }
  
  # Avoid double-requesting variables
  # However, if all states are requested, we should still return the state by default
  # as this is expected behavior when requesting data by state
  
  # Old code - this should get picked up in load_data.R.  To be removed
  # if (all(state == "all")) {
  #   if (!"ST" %in% variables) {
  #     variables <- c("ST", variables)
  #   }
  # }
  
  join_vars <- c("SERIALNO", "SPORDER", "WGTP", "PWGTP", "ST")
  
  variables <- variables[!variables %in% join_vars]
  
  if (!is.null(rep_weights)) {
    if (rep_weights == "housing") {
      variables <- c(variables, housing_weight_variables)
    }
    if (rep_weights == "person") {
      variables <- c(variables, person_weight_variables)
    }
    if (rep_weights == "both") {
      variables <- c(variables, housing_weight_variables, person_weight_variables)
    }
  }
  
  ## If more than 45 vars requested, split into multiple API calls and join the result
  ## this works, but repeats pulling the weight and ST vars
  if (length(variables) > 45) {
    l <- split(variables, ceiling(seq_along(variables) / 45))
    pums_data <- map(l, function(x) {
      
      load_data_pums_pr(variables = x,
                     state = state,
                     year = year,
                     puma = puma,
                     survey = survey,
                     variables_filter = variables_filter,
                     recode = recode,
                     show_call = show_call,
                     key = key)
    })
    
    # to combine the multiple API calls, we need to join using the repeated
    # variables so they don't get duplicated in the final data frame
    # the repeated variables will depend on how we requested data
    #
    # This gets complicated further by the use of a variable filter which
    # is handled differently than the other variables, so we'll need to
    # merge that in as well
    
    if (recode) {
      if (!is.null(puma)) {
        join_vars <- c(join_vars, "ST_label", "PUMA")
      } else {
        join_vars <- c(join_vars, "ST_label")
      }
    } else {
      if (!is.null(puma)) {
        join_vars <- c(join_vars, "PUMA")
      }
    }
    
    if (!is.null(variables_filter)) {
      var_names <- names(variables_filter)
      var_names <- var_names[!var_names == "SPORDER"]
      
      if (recode) {
        check_type <- pums_variables %>%
          dplyr::filter(var_code %in% var_names,
                        survey == survey,
                        year == year,
                        data_type == "chr") %>%
          dplyr::distinct(var_code) %>%
          dplyr::pull(var_code)
        
        chr_names <- var_names[var_names %in% check_type]
        
        if (length(chr_names) > 0) {
          var_labels <- paste0(chr_names, "_label")
          
          join_vars <- c(join_vars, var_names, var_labels)
        } else {
          join_vars <- c(join_vars, var_names)
        }
        
      } else {
        join_vars <- c(join_vars, var_names)
      }
    }
    
    pums_data <- reduce(pums_data, left_join, by = join_vars)
    
    
    pums_data <- select(pums_data, -contains("WGTP"), everything(), contains("WGTP"))
  } else {
    pums_data <- load_data_pums_pr(variables = variables,
                                state = state,
                                puma = puma,
                                year = year,
                                survey = survey,
                                variables_filter = variables_filter,
                                recode = recode,
                                show_call = show_call,
                                key = key)
  }
  
  return(pums_data)
}


#' Convert a data frame returned by get_pums() to a survey object
#'
#' @description This helper function takes a data frame returned by
#'   \code{\link{get_pums}} and converts it to a tbl_svy from the srvyr
#'   \code{\link[srvyr]{as_survey}} package or a svyrep.design object from the
#'   \code{\link[survey]{svrepdesign}} package. You can then use functions from
#'   the srvyr or survey to calculate weighted estimates with replicate weights
#'   included to provide accurate standard errors.
#'
#' @param df A data frame with PUMS person or housing weight variables, most
#'   likely returned by \code{\link{get_pums}}.
#' @param type Whether to use person or housing-level weights; either
#'   \code{"housing"} or \code{"person"} (the default).
#' @param design The survey design to use when creating a survey object.
#'   Currently the only option is code{"rep_weights"}/.
#' @param class Whether to convert to a srvyr or survey object; either
#'   \code{"survey"} or \code{"srvyr"} (the default).
#'
#' @return A tbl_svy or svyrep.design object.
#' @export
#'
#' @examples
#' \dontrun{
#' pums <- get_pums(variables = "AGEP", state = "VT", rep_weights = "person")
#' pums_design <- to_survey(pums, type = "person", class = "srvyr")
#' survey::svymean(~AGEP, pums_design)
#' }
to_survey <- function(df,
                      type = c("person", "housing"),
                      class = c("srvyr", "survey"),
                      design = "rep_weights") {
  
  type <- match.arg(type)
  class <- match.arg(class)
  design <- match.arg(design)
  
  if (class == "srvyr" && !"srvyr" %in% installed.packages()) {
    stop('srvyr package must be installed to convert to a srvyr object. Please install using install.packages("srvyr") and try again.',
         call. = FALSE)
  }
  
  if (!"survey" %in% installed.packages()) {
    stop('survey package must be installed to convert to a survey object. Please install using install.packages("survey") and try again.',
         call. = FALSE)
  }
  
  # if (design == "cluster") {
  #   if (!all(c("SERIALNO", "PUMA") %in% names(df))) {
  #     stop('"SERIALNO" and "PUMA" must both be present in the input data.', call. = FALSE)
  #   }
  # }
  
  if (type == "person") {
    
    variables <- df[, !names(df) %in% c(person_weight_variables, "PWGTP")]
    weights <- df$PWGTP
    
    if (design == "rep_weights") {
      if (!all(person_weight_variables %in% names(df))) {
        stop("Not all person replicate weight variables are present in input data.", call. = FALSE)
      }
      if (!"PWGTP" %in% names(df)) {
        stop("Person weight variable is not present in input data.", call. = FALSE)
      }
      repweights <- df[, person_weight_variables]
    }
  }
  
  if (type == "housing") {
    if (anyDuplicated(df$SERIALNO) != 0) {
      warning("You have duplicate values in the SERIALNO column of your input data, are you sure you wish to proceed?",
              call. = FALSE)
    }
    
    variables <- df[, !names(df) %in% c(housing_weight_variables, "WGTP")]
    weights <- df$WGTP
    
    if (design == "rep_weights") {
      if (!all(housing_weight_variables %in% names(df))) {
        stop("Not all housing replicate weight variables are present in input data.", call. = FALSE)
      }
      if (!"WGTP" %in% names(df)) {
        stop("Housing weight variable is not present in input data.", call. = FALSE)
      }
      repweights <- df[, housing_weight_variables]
    }
  }
  
  # # remove option for cluster design for now pending more research/discussion
  # if (design == "cluster") {
  #   survey <- survey::svydesign(
  #     variables = variables,
  #     weights = weights,
  #     ids = df$SERIALNO,
  #     strata = df$PUMA
  #   )
  # }
  
  if (design == "rep_weights"){
    survey <- survey::svrepdesign(
      variables = variables,
      weights = weights,
      repweights = repweights,
      scale = 4 / 80,
      rscales = rep(1 , 80),
      mse = TRUE,
      type = "JK1"
    )
  }
  
  if (class == "srvyr") {
    return(srvyr::as_survey(survey))
  } else {
    survey
  }
}






load_data_pums_pr <- function(variables, state, puma, key, year, survey,
                              variables_filter, recode, show_call) {
  
  # for which years is data dictionary available in pums_variables?
  # we'll use this a couple times later on
  recode_years <- 2017:2019
  
  base <- sprintf("https://api.census.gov/data/%s/acs/%s/pumspr",
                  year, survey)
  
  
  if (!is.null(puma)) {
    
    if (length(state) > 1) {
      stop('When requesting PUMAs for more than one state, you must set state to "multiple" and set puma to a named vector of state/PUMA pairs.', call. = FALSE)
    }
    
    # pumas in multiple states can be requested with a named vector of
    # state / puma pairs in the puma argument of get_pums()
    if (state == "multiple") {
      
      # print FIPS code of states used just once
      purrr::walk(unique(names(puma)), validate_state_pr)
      
      geo <- purrr::map2_chr(names(puma), unname(puma), function(x, y) {
        paste0("7950000US", suppressMessages(validate_state_pr(x)), y)
      })
      
      geo <- paste0(geo, collapse = ",")
      
    } else {
      # if PUMAs requested are in one state only
      state <- validate_state_pr(state)
      geo <- purrr::map_chr(puma, function(x) {
        paste0("7950000US", state, x)
      })
      
      if (length(puma) > 1) {
        geo <- paste0(geo, collapse = ",")
      }
    }
  } else {
    # if no PUMAs specified, get all PUMAs in each state requested
    if (!is.null(state)) {
      geo <- purrr::map_chr(state, function(x) {
        paste0("0400000US", validate_state_pr(x))
      })
      
    } else {
      geo <- NULL
    }
    
    if (length(state) > 1) {
      geo <- paste0(geo, collapse = ",")
      
    }
  }
  
  # Handle variables & variable filter
  # If a variables filter is supplied, those variables should be removed from
  # variables (if applicable)
  if (!is.null(variables_filter)) {
    filter_names <- names(variables_filter)
    
    variables <- variables[!variables %in% filter_names]
    
    if (length(variables) > 0) {
      var <- paste0(variables, collapse = ",")
      
      # Handle housing-only query
      if ("SPORDER" %in% filter_names) {
        vars_to_get <- paste0("SERIALNO,WGTP,PWGTP,", var)
      } else {
        vars_to_get <- paste0("SERIALNO,SPORDER,WGTP,PWGTP,", var)
      }
    } else {
      if ("SPORDER" %in% filter_names) {
        vars_to_get <- "SERIALNO,WGTP,PWGTP"
      } else {
        vars_to_get <- "SERIALNO,SPORDER,WGTP,PWGTP"
      }
    }
    
    # If geo is NULL, state should be added back in here
    if (is.null(geo)) {
      vars_to_get <- paste0(vars_to_get, ",ST")
    }
    
    # Combine the default query with the variables filter query
    query_default <- list(get = vars_to_get,
                          ucgid = geo,
                          key = key)
    
    # Collapse vectors if supplied
    variables_filter_collapsed <- purrr::map(variables_filter,
                                             ~{paste0(.x, collapse = ",")})
    
    query <- c(
      list(get = vars_to_get),
      variables_filter_collapsed,
      list(ucgid = geo, key = key)
    )
    
  } else {
    
    var <- paste0(variables, collapse = ",")
    
    vars_to_get <- paste0("SERIALNO,SPORDER,WGTP,PWGTP,", var)
    
    # If geo is NULL, state should be added back in here
    if (is.null(geo)) {
      vars_to_get <- paste0(vars_to_get, ",ST")
    }
    
    query <- list(get = vars_to_get,
                  ucgid = geo,
                  key = key)
    
  }
  
  call <- httr::GET(base,
                    query = query,
                    httr::progress())
  
  if (show_call) {
    call_url <- gsub("&key.*", "", call$url)
    message(paste("Census API call:", call_url))
  }
  
  # Make sure call status returns 200, else, print the error message for the user.
  if (call$status_code != 200) {
    msg <- content(call, as = "text")
    
    if (grepl("The requested resource is not available", msg)) {
      stop("One or more of your requested variables is likely not available at the requested geography.  Please refine your selection.", call. = FALSE)
    } else {
      stop(sprintf("Your API call has errors.  The API message returned is %s.", msg), call. = FALSE)
    }
    
  }
  
  
  content <- httr::content(call, as = "text")
  
  if (grepl("You included a key with this request", content)) {
    stop("You have supplied an invalid or inactive API key. To obtain a valid API key, visit https://api.census.gov/data/key_signup.html. To activate your key, be sure to click the link provided to you in the email from the Census Bureau that contained your key.", call. = FALSE)
  }
  
  dat <- jsonlite::fromJSON(content)
  
  colnames(dat) <- dat[1,]
  
  dat <- dplyr::as_tibble(dat, .name_repair = "minimal")
  
  dat <- dat[-1,]
  
  # Convert the weights columns to numeric
  dat <- dat %>%
    dplyr::mutate(
      dplyr::across(
        .cols = dplyr::contains("WGTP"),
        as.numeric
      )
    )
  # dat$WGTP <- as.numeric(dat$WGTP)
  # dat$PWGTP <- as.numeric(dat$PWGTP)
  
  # Filter the pums lookup table for the selected year and survey
  pums_variables_filter <- tidycensus::pums_variables %>%
    filter(year == !!year, survey == !!survey)
  
  # Do some clean up of the API response: pad returned values with 0s when
  # necessary to match data dictionary codes and
  # convert variables to numeric according to data dictionary
  
  # Only works for years included in pums_variables data dictionary
  if (year %in% recode_years) {
    var_val_length <- pums_variables_filter %>%
      filter(!is.na(.data$val_length)) %>%
      distinct(.data$var_code, .data$val_length, .data$val_na)
    
    num_vars <- pums_variables_filter %>%
      filter(.data$data_type == "num") %>%
      distinct(.data$var_code) %>%
      pull()
    
    # For all variables in which we know what the length should be, pad with 0s
    dat_padded <- suppressWarnings(
      dat %>%
        select(.data$SERIALNO, .data$SPORDER, any_of(var_val_length$var_code)) %>%
        pivot_longer(
          cols = -c(.data$SERIALNO, .data$SPORDER),
          names_to = "var_code",
          values_to = "val"
        ) %>%
        left_join(var_val_length, by = "var_code") %>%
        mutate(
          val = ifelse(!is.na(.data$val_na) & .data$val_na == .data$val, strrep("b", .data$val_length), .data$val),
          val = ifelse(.data$var_code != "NAICSP", str_pad(.data$val, .data$val_length, pad = "0"), .data$val),
          val = ifelse(.data$var_code == "NAICSP" & .data$val == "*", "bbbbbbbb", .data$val),  # special NULL value returned by API for this var
        ) %>%
        select(-.data$val_length, -.data$val_na) %>%
        pivot_wider(
          names_from = .data$var_code,
          values_from = .data$val
        )
    )
    
    # Rejoin padded variables to API return
    dat <- dat %>%
      select(.data$SERIALNO, .data$SPORDER, !any_of(var_val_length$var_code)) %>%
      left_join(dat_padded, by = c("SERIALNO", "SPORDER")) %>%
      mutate_at(vars(any_of(num_vars)), as.double)
  }
  
  # Do you want to return value labels also?
  if (recode) {
    
    # Only works for years included in pums_variables data dictionary
    if (year %in% recode_years) {
      var_lookup <- pums_variables_filter %>%
        select(.data$var_code, val = .data$val_min, .data$val_label)
      
      # Vector of variables that are possible to recode
      vars_to_recode <- pums_variables_filter %>%
        filter(.data$recode) %>%
        distinct(.data$var_code) %>%
        pull()
      
      # Pivot to long format and join variable codes to lookup table with labels
      recoded_long <- suppressWarnings(
        dat %>%
          select(.data$SERIALNO, .data$SPORDER, any_of(vars_to_recode)) %>%
          pivot_longer(
            cols = -c(.data$SERIALNO, .data$SPORDER),
            names_to = "var_code",
            values_to = "val"
          ) %>%
          left_join(var_lookup, by = c("var_code", "val")) %>%
          select(-.data$val)
      )
      
      # Create a "pivot spec" with nicer names for the labeled columns
      # https://tidyr.tidyverse.org/articles/pivot.html#wider-1
      spec <- recoded_long %>%
        build_wider_spec(
          names_from = .data$var_code,
          values_from = .data$val_label
        ) %>%
        mutate(.name = paste0(.data$var_code, "_label", ""))
      
      recoded_wide <- recoded_long %>%
        pivot_wider_spec(spec)
      
      recode_v <- recoded_long %>% pull(var_code) %>% unique()
      
      for (var in recode_v){
        
        order_v <- var_lookup %>%
          filter(var_code==var) %>%
          pull(val_label)
        
        var_label <- str_c(var, "_label")
        
        recoded_wide[[var_label]] <-
          readr::parse_factor(recoded_wide[[var_label]], ordered=TRUE, levels=order_v)
        
      }
      
      
      # Join recoded columns to API return
      dat <- dat %>%
        left_join(recoded_wide, by = c("SERIALNO", "SPORDER"))
    } else {
      message(paste("Recoding is currently supported for",
                    min(recode_years), "-", max(recode_years),
                    "data. Returning original data only."))
    }
  }
  return(dat)
}




validate_state_pr <- function(state, .msg=interactive()) {
  
  if (is.null(state)) return(NULL)
  
  state <- tolower(str_trim(state)) # forgive white space
  
  if (grepl("^[[:digit:]]+$", state)) { # we prbly have FIPS
    
    state <- sprintf("%02d", as.numeric(state)) # forgive 1-digit FIPS codes
    
    if (state %in% fips_state_table$fips) {
      return(state)
    } else {
      # perhaps they passed in a county FIPS by accident so forgive that, too,
      # but warn the caller
      state_sub <- substr(state, 1, 2)
      if (state_sub %in% fips_state_table$fips) {
        message(sprintf("Using first two digits of %s - '%s' (%s) - for FIPS code.",
                        state, state_sub,
                        fips_state_table[fips_state_table$fips == state_sub, "name"]),
                call.=FALSE)
        return(state_sub)
      } else {
        stop(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)
        
      }
    }
    
  } else if (grepl("^[[:alpha:]]+", state)) { # we might have state abbrev or name
    
    if (nchar(state) == 2 && state %in% fips_state_table$abb) { # yay, an abbrev!
      
      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        fips_state_table[fips_state_table$abb == state, "fips"],
                        toupper(state)))
      return(fips_state_table[fips_state_table$abb == state, "fips"])
      
    } else if (nchar(state) > 2 && state %in% fips_state_table$name) { # yay, a name!
      
      if (.msg)
        message(sprintf("Using FIPS code '%s' for state '%s'",
                        fips_state_table[fips_state_table$name == state, "fips"],
                        simpleCapSO(state)))
      return(fips_state_table[fips_state_table$name == state, "fips"])
      
    } else {
      stop(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)
      
    }
    
  } else {
    stop(sprintf("'%s' is not a valid FIPS code or state name/abbreviation", state), call.=FALSE)
    
  }
  
}

fips_state_table <- structure(list(abb = c("ak", "al", "ar", "as", "az", "ca", "co",
                                           "ct", "dc", "de", "fl", "ga", "gu", "hi", "ia", "id", "il", "in",
                                           "ks", "ky", "la", "ma", "md", "me", "mi", "mn", "mo", "ms", "mt",
                                           "nc", "nd", "ne", "nh", "nj", "nm", "nv", "ny", "oh", "ok", "or",
                                           "pa", "pr", "ri", "sc", "sd", "tn", "tx", "ut", "va", "vi", "vt",
                                           "wa", "wi", "wv", "wy", "mp"), fips = c("02", "01", "05", "60", "04",
                                                                                   "06", "08", "09", "11", "10", "12", "13", "66", "15", "19", "16",
                                                                                   "17", "18", "20", "21", "22", "25", "24", "23", "26", "27", "29",
                                                                                   "28", "30", "37", "38", "31", "33", "34", "35", "32", "36", "39",
                                                                                   "40", "41", "42", "72", "44", "45", "46", "47", "48", "49", "51",
                                                                                   "78", "50", "53", "55", "54", "56", "69"), name = c("alaska", "alabama",
                                                                                                                                       "arkansas", "american samoa", "arizona", "california", "colorado",
                                                                                                                                       "connecticut", "district of columbia", "delaware", "florida",
                                                                                                                                       "georgia", "guam", "hawaii", "iowa", "idaho", "illinois", "indiana",
                                                                                                                                       "kansas", "kentucky", "louisiana", "massachusetts", "maryland",
                                                                                                                                       "maine", "michigan", "minnesota", "missouri", "mississippi",
                                                                                                                                       "montana", "north carolina", "north dakota", "nebraska", "new hampshire",
                                                                                                                                       "new jersey", "new mexico", "nevada", "new york", "ohio", "oklahoma",
                                                                                                                                       "oregon", "pennsylvania", "puerto rico", "rhode island", "south carolina",
                                                                                                                                       "south dakota", "tennessee", "texas", "utah", "virginia", "virgin islands",
                                                                                                                                       "vermont", "washington", "wisconsin", "west virginia", "wyoming", "northern mariana islands"
                                                                                   )), .Names = c("abb", "fips", "name"), row.names = c(NA, -56L
                                                                                   ), class = "data.frame")

simpleCapSO <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste0(toupper(substring(s, 1,1)), substring(s, 2),
         collapse=" ")
}
