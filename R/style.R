
pad  <- function(x, width =  2) str_pad(x, width = width, pad = "0")
pad0 <- function(x, width = 14) str_pad(x, width = width, pad = " ")
decimal <- function(x, digit = 1) sprintf(paste0("%.", digit, "f"), x)

theme_view <- function(family = "Cascade Code",
                       x.size = NULL, y.size = NULL, t.size = NULL, s.size = NULL,
                       x.angle = 0, y.angle = 0, y.comma = TRUE,
                       legend.position = "right") {
  list(
    theme(
      text  = element_text(family = family),
      title = element_text(family = family, size = t.size, face = "bold"),
      strip.text.x = element_text(size = s.size, face = "bold"),
      axis.text.x  = element_text(size = x.size, face = "bold", angle = x.angle),
      axis.text.y  = element_text(size = y.size, face = "bold", angle = y.angle),
      legend.position = legend.position
    ),
    if (y.comma) scale_y_continuous(labels = comma)
  )
}

theme_shiny <- function(family = "Cascade Code",
                        x.size = 12, y.size = 12, t.size = 17, s.size = 17,
                        x.angle = 0, y.angle = 0, y.comma = TRUE,
                        legend.position = "bottom") {
  list(
    theme(
      text  = element_text(family = family),
      title = element_text(family = family, size = t.size, face = "bold"),
      strip.text.x = element_text(size = s.size, face = "bold"),
      axis.text.x  = element_text(size = x.size, face = "bold", angle = x.angle),
      axis.text.y  = element_text(size = y.size, face = "bold", angle = y.angle),
      legend.position = legend.position
    ),
    if (y.comma) scale_y_continuous(labels = comma)
  )
}

theme_save <- function(family = "Cascade Code",
                       x.size = 12, y.size = 12, t.size = NULL, s.size = 17,
                       x.angle = 0, y.angle = 0, y.comma = TRUE,
                       legend.position = "bottom") {
  list(
    theme(
      text  = element_text(family = family),
      title = element_text(family = family, size = t.size, face = "bold"),
      strip.text.x = element_text(size = s.size, face = "bold"),
      axis.text.x  = element_text(size = x.size, face = "bold", angle = x.angle),
      axis.text.y  = element_text(size = y.size, face = "bold", angle = y.angle),
      legend.position = legend.position
    ),
    if (y.comma) scale_y_continuous(labels = comma)
  )
}

scale_gender_manual <- function(x, gender = c("1", "2"), colors = "vuw.double.colors1") {
  list(
    if (unilen(x) == 2) {
      scale_color_manual(
        values = options()[[colors]]
      )
    } else if (unique(x) == gender[1L]) {
      scale_color_manual(
        values = options()[[colors]][1L]
      )
    } else if (unique(x) == gender[2L]) {
      scale_color_manual(
        values = options()[[colors]][2L]
      )
    },
    if (unilen(x) == 2) {
      scale_fill_manual(
        values = options()[[colors]]
      )
    } else if (unique(x) == gender[1L]) {
      scale_fill_manual(
        values = options()[[colors]][1L]
      )
    } else if (unique(x) == gender[2L]) {
      scale_fill_manual(
        values = options()[[colors]][2L]
      )
    }
  )
}
