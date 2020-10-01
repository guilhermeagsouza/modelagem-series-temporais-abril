fun_calc_coord_polygon <- function(min_x_num, max_x_num, y) {
  
  list(
    c(min_x_num, 0),
    c(min_x_num, y),
    c(max_x_num, y),
    c(max_x_num, 0)
  )
  
}