geom_zonebox <- function(..., colour = 'red', scale = 'cm') {
  if (scale == 'ft') {
    topKzone <- 3.5
    botKzone <- 1.6
    inKzone <- -0.85
    outKzone <- 0.85
  } else {
    topKzone <- 3.5 * 30.48
    botKzone <- 1.6 * 30.48
    inKzone <- -0.85 * 30.48
    outKzone <- 0.85 * 30.48
  }
  kZone <- data.frame(
    x=c(inKzone, inKzone, outKzone, outKzone, inKzone),
    y=c(botKzone, topKzone, topKzone, botKzone, botKzone)
  )
  
  geom_path(aes(x, y), data = kZone,
              lwd = 1, colour = colour)
}
