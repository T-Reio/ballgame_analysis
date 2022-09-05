pitch_caller <- function(
    plate_x, plate_z, 
    sz_top = 3.5 * 30.48, sz_bot = 1.6 * 30.48, 
    sz_sideIn = -0.85 * 30.48, sz_sideOut = 0.85 * 30.48
  ) {
  out <- if_else(
    (plate_x >= sz_sideIn & plate_x <= sz_sideOut) &
      (plate_z >= sz_bot & plate_z <= sz_top),
    1, 0
  )
  out
}
