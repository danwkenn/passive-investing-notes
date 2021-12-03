
#' Update the CPI information:
#' @export
update_cpi <- function(source = NULL){
  
  cpi <- setDT(read_cpi(path = 'data',retain_files = TRUE))
  fwrite(cpi,file = glue('data/cpi_',as.character(Sys.Date()),".csv"))
  TRUE
}

#' Convert a price at a given date to a new date using inflation statistics.
#' @export
inflate <- function(price, date, new_date = Sys.Date(), cpi_table){
  initial <- data.table(price = price, date = date)
  initial[,`:=`(quarter = floor_date(date, 'quarter') %m-% months(1))]
  initial[cpi_data,on = c(quarter = 'date'), cpi := i.cpi]
  new_date_cpi <- cpi_table[date == floor_date(new_date, 'quarter') %m-% months(1), cpi]
  initial[,`:=`(inflation_factor = new_date_cpi / cpi)]
  initial[,price * inflation_factor]
}

#' Inverse function to adjust a figures for inflation.
#' @export
adjust_for_inflation <- function(price, date, new_date = Sys.Date(), cpi_table){
  initial <- data.table(price = price, date = date)
  initial[,`:=`(quarter = floor_date(date, 'quarter') %m-% months(1))]
  initial[cpi_data,on = c(quarter = 'date'), cpi := i.cpi]
  new_date_cpi <- cpi_table[date == floor_date(new_date, 'quarter') %m-% months(1), cpi]
  initial[,`:=`(inflation_factor = new_date_cpi / cpi)]
  initial[,price / inflation_factor]
}