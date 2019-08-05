#' Fetch WOA data
#' 
#' given a set of variables to fetch, download WOA in netCDF format into the specified folder.
#'
#' @param output_dir location to save netCDF files˛
#' @param res char array of resolutions to fetch. c("5deg", "1.00", "0.25") for 5degree grid, 1 degree grid or 0.25 degree grid. Can also be a logical or integer subset
#' @param decade character array of decades to fetch, c("decav", "5564", "6574", "7584", "8594", "95A4", "A5B7", "decav81B0"). This parameter is ignored for env_var's that don't have decadal averages.
#' @param env_var char array of environment variables c("temperature", "salinity", "oxygen", "oxy_sat", "oxy_util", "silicate", "phosphate", "nitrate"). Can also be a logical or integer subset, eg c(1,4) for temperature and oxygen saturation. Default is to choose all.
#' @param version WOA version, c("13", "18")
#' @param season integer vector. Including 0 in the vector returns annual climatology, 1 to 12 returns January to December (1:12 for all) and 13 to 16 return Winter, Spring, Summer, Autumn. Default 0:16 returns all climatologies
#' @param dl_method specify how download.file() will fetch file. See ?download.file
#' @param verbose control printing to terminal
#' @param redownload if TRUE, overwrite existing files 
woa_fetcher <- function(output_dir, 
                        res = "1.00",
                        decade = "A5B7",
                        env_var = NULL,
                        version = "18",
                        season = 0:16,
                        dl_method = "auto",
                        verbose = FALSE
                        redownload = FALSE) {
    #valid names
    woa_names <- list(res = c("5deg",
                           "1.00",
                           "0.25"),
                        decade = c("decav",
                             "5564",
                             "6574",
                             "7584",
                             "8594",
                             "95A4",
                             "A5B7",
                             "decav81B0"),
                        env_var = c("temperature",
                        "salinity",
                        "oxygen",
                        "o2sat",
                        "AOU",
                        "silicate",
                        "phosphate",
                        "nitrate"),
                  version = c("13",
                              "18")
                  )

    param_list <- list(res = unique(res),
                       decade = unique(decade),
                       env_var = unique(env_var),
                       version = unique(version),
                       season = unique(season))

    if(is.null(env_var)) {
        param_list$env_var <- woa_names$env_var
    }

    #Check for valid parameters
    mapply(param_list[!(names(param_list) %in% "season")], woa_names, FUN =  function(param, wn){
        if(!(all(param %in%  wn) | all(param %in% seq.int(1, length(wn))) | (is.logical(param) & length(param) == length(wn)))){
            stop(paste0("parameter has invalid input: ", param, ". \nMust be in ", wn, " or provide a integer/logical subset for wn"))
        }
        return("valid")
      }
    )

    if(!all(param_list$season %in% 0:16)) {
        stop(paste0("'season' must be integer vector containing only numbers from 0 to 16 inclusively. Got: ", season))
    }

    #convert all parameters to char vectors
    param_char <- sapply(names(param_list[!(names(param_list) %in% "season")]), function(param){
                             ret <- switch(class(param_list[[param]]), 
                  "logical" = {unique(woa_names[[param]][param_list[[param]]])},
                  "numeric" = {unique(woa_names[[param]][param_list[[param]]])},
                  "integer" = {unique(woa_names[[param]][param_list[[param]]])},
                  "character" = {unique(param_list[[param]])},
                  stop(paste0(param, " seems to be misspecified. Got: ", param_list[[param]], 
". Expected logical, numeric or character vector." ))
                  )
                             return(ret)
        }
    )

    param_char$season <- switch(class(param_list$season),
                                "logical" = {unique(0:16[param_list$season])},
                                "numeric" = {unique(param_list$season)},
                                "integer" = {unique(param_list$season)},
                  stop(paste0("season seems to be misspecified. Got: ", param_list[["season"]], 
". Expected logical or numeric vector."))
                  )





    #define mappings to urls and filenames in NOAA

    res_url_map <- list(`5deg` = "5d",
                         `1.00` = "01",
                         `0.25` = "04")

    env_var_f_map <- list(temperature = "t",
                        salinity = "s",
                        oxygen = "o",
                        o2sat = "O",
                        AOU = "A",
                        silicate = "i",
                        phosphate = "p",
                        nitrate = "n")

    env_var_url_map <- list(temperature = "temperature",
                        salinity = "salinity",
                        oxygen = "oxygen",
                        o2sat = "o2sat",
                        AOU = "AOU",
                        silicate = "silicate",
                        phosphate = "phosphate",
                        nitrate = "nitrate")

    to_fetch <- tidyr::crossing(env_var = param_char$env_var, decade =  param_char$decade, res = param_char$res, version =  param_char$version, season = param_char$season)

    #dplyr approach: case_when
    to_fetch_dec <- dplyr::mutate(to_fetch, decade = dplyr::case_when(
                                                  env_var %in% c("temperature", "salinity") ~ decade,
                                                  TRUE ~ "all")
    )
    #collapse, to_fetch_dec has non_unique rows now
    to_fetch_uni <- unique(to_fetch_dec)

    #ignore invalid resolutions
    #temperature, salinity: 5deg only allowed with decav. 1 and 0.25 always ok
    #all others, 5deg and 1 ok, 0.25 never allowed
    to_fetch_res <- dplyr::filter(to_fetch_uni,
                                  (env_var %in% c("temperature", "salinity") & ((decade == "decav" & res == "5deg") | (res %in% c("1.00", "0.25") ))) |
                                   (!(env_var %in% c("temperature", "salinity")) & (res %in% c("1.00", "5deg") ))
                                  )

    if (verbose) {
        dropped_rows <- dplyr::setdiff(to_fetch_uni, to_fetch_res)
        print("Dropped invalid combinations")
        print(dropped_rows)
    }



    base_url <- "https://data.nodc.noaa.gov/thredds/fileServer/ncei/woa"

    fetched_urls <- apply(to_fetch_res, 1, function(f){
        nc_url <- file.path(base_url, f["env_var"], f["decade"], f["res"])
        nc_file <- paste0("woa", f["version"], "_", f["decade"], "_", env_var_f_map[[f["env_var"]]], stringr::str_pad(as.integer(f["season"]), width = 2, side = "left", pad = "0"), "_", res_url_map[[f["res"]]], ".nc") 
        if(!file.exists(file.path(output_dir, nc_file)) | redownload){
            dl_ret <- download.file(file.path(nc_url, nc_file), file.path(output_dir, nc_file), method = dl_method, quiet = !verbose, mode = "wb")
        } else {
            #file exists, don't fetch again
            dl_ret = 0
        }

        if(dl_ret != 0){
            stop(paste0("Download failed. download.file reported error code: ", dl_ret))
        }
        return(file.path(output_dir, nc_file))
    })


    if (verbose) {
        message(fetched_urls)
    }

}
