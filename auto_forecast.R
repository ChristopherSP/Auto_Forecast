library(forecast)
library(data.table)
library(readr)
library(yaml)
library(stringi)

######################################
# Global Variables
######################################
parameters = read_yaml("/home/user/Documents/Codes/Auto_Forecast/parameters.yaml")

file_path = parameters$file_path
file_separator = parameters$file_separator
levels = parameters$levels
date_var = parameters$date_column
date_frequency = parameters$date_frequency
target = parameters$target
h_obs = parameters$h_obs
output_path = parameters$output_path
output_path = ifelse(endsWith(output_path, "/"), output_path, paste0(output_path, "/"))

######################################
# Functions
######################################
rename = function(name){
  stri_replace_all_fixed(
    stri_trim_both(
      stri_replace_all_regex(
        stri_replace_all_regex(
          stri_trans_general(
            tolower(name),
            "latin-ascii"
          ), "[º[[:punct:]]-[_]]", ""
        ), " +", " "
      )
    ), " ", "_"
  )
}

format_forecast = function(fit, type = c("sarima", "hw")){
  if(type == "sarima" | !is.null(fit[[1]]$hw)){
    fit_name = names(fit)
    result = as.data.table(forecast(fit[[1]][[type]], h = h_obs), keep.rownames = T)
    names(result)[1] = "date"
    result[, type := type]
    result[, c(levels) := tstrsplit(fit_name, "\t", fixed = T)]
  } else {
    result = NULL
  }
  return(result)
}

ts_fit = function(dt){
  ts_obj = ts(dt[[target]],
              start = c(year(min(dt[[date_var]])), month(min(dt[[date_var]]))),
              end = c(year(max(dt[[date_var]])), month(max(dt[[date_var]]))),
              frequency = date_frequency
  )
  sarima = auto.arima(ts_obj)
  holt = tryCatch(HoltWinters(ts_obj),
                  error = function(e){NULL}
  )
  return(list(ts_obj = ts_obj, sarima = sarima, hw = holt))
}
######################################
# Main
######################################
levels = rename(levels)
date_var = rename(date_var)
target = rename(target)

data = fread(file_path, sep = file_separator)
names(data) = rename(names(data))
data[[date_var]] = as.Date(data[[date_var]])

if(any(is.na(data[[target]]))){
  data[is.na(data[[target]])][[target]] = 0
}

setorderv(data, c(levels, date_var))

count_level = data[,.N, by = levels]
count_level[, level_name := do.call(paste, c(.SD, sep = "\t")), .SDcols = levels]

reps = count_level$N
breaks = seq_along(reps)
level_names = count_level$level_name
data[, breaks := rep(breaks, times = reps)]

fitted = lapply(breaks, FUN = function(level){
  dt = copy(data[breaks == level])
  mod = ts_fit(dt)
  return(list(ts = mod$ts_obj, sarima = mod$sarima, hw = mod$holt))
})

names(fitted) = level_names

dt_result_sarima = lapply(seq_along(fitted), function(idx){
  format_forecast(fitted[idx], type = "sarima")
})
dt_result_sarima = rbindlist(dt_result_sarima)

dt_result_hw = lapply(seq_along(fitted), function(idx){
  format_forecast(fitted[idx], type = "hw")
})
dt_result_hw = rbindlist(dt_result_hw)

fwrite(dt_result_sarima, paste0(output_path, "sarima.txt"), sep = file_separator, row.names = F)
fwrite(dt_result_hw, paste0(output_path, "hw.txt"), sep = file_separator, row.names = F)

lapply(names(fitted), function(nm_serie){
  partes = unlist(strsplit(nm_serie, "\t", fixed = T))
  dir.create(paste0(output_path, "img/sarima/", paste(partes, collapse = "/")), recursive = T)
  dir.create(paste0(output_path, "img/hw/", paste(partes, collapse = "/")), recursive = T)
})

lapply(names(fitted), function(nm_series){
  partes = unlist(strsplit(nm_series, "\t", fixed = T))
  png(filename = paste0(output_path, "img/sarima/", paste(partes, collapse = "/"), "/sarima.png"))
  plot(forecast(fitted[[nm_series]]$sarima, h = h_obs), main = nm_series)
  dev.off()
  
  if(!is.null(fitted[[nm_series]]$hw)){
    png(filename = paste0(output_path, "img/hw/", paste(partes, collapse = "/"), "/hw.png"))
    plot(forecast(fitted[[nm_series]]$hw, h = h_obs), main = nm_series)
    dev.off()
  }
})

