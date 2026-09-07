# funcoes


meanfill <- function(x){
  x = str_squish(x)
  x = fifelse(x=="",NA,x)
  a = mean(is.na(x),na.rm=T)
  b = round((1-a)*100,3)
  b
}


# identifica NA e valores zerados/vazios como NA
whatsna <- function(x){
  a = fcase(is.na(x), NA,
            str_length(str_trim(x)) == 0, NA,
            default = x)
  b = is.na(a)
  return(b)
}


makena <- function(x){
  a = fcase(is.na(x) | str_length(str_trim(x)) > 0, x ,
                x == "", NA)
  return(a)
}


makeone <- function(x){
  a = fcase(is.na(x), 0,
            str_length(str_trim(x)) == 0, 0,
            default = 1)
  return(a)
}
