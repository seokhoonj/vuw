
pad  <- function(x, width =  2) str_pad(x, width = width, pad = "0")
pad0 <- function(x, width = 14) str_pad(x, width = width, pad = " ")
decimal <- function(x, digit = 1) sprintf(paste0("%.", digit, "f"), x)

theme_view <- function(x.angle = 0) {
  theme(
    text  = element_text(family = "Malgun Gothic"),
    title = element_text(family = "Malgun Gothic"),
    strip.text.x = element_text(face = "bold"),
    axis.text.x  = element_text(face = "bold", angle = x.angle),
    axis.text.y  = element_text(face = "bold"),
    legend.position = "bottom"
  )
}

theme_save <- function(x.angle = 0) {
  theme(
    text  = element_text(family = "Malgun Gothic"),
    title = element_text(family = "Malgun Gothic"),
    strip.text.x = element_text(size = 17, face = "bold"),
    axis.text.x  = element_text(size = 12, face = "bold", angle = x.angle),
    axis.text.y  = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )
}
