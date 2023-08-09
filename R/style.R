
pad  <- function(x, width =  2) str_pad(x, width = width, pad = "0")
pad0 <- function(x, width = 14) str_pad(x, width = width, pad = " ")
decimal <- function(x, digit = 1) sprintf(paste0("%.", digit, "f"), x)

theme_view <- function(family = "Comic Sans MS",
                       x.size  =   NULL , y.size =    NULL, t.size =    NULL, s.size =    NULL, l.size =    NULL,
                       x.face  = "plain", y.face = "plain", t.face = "plain", s.face = "plain", l.face = "plain",
                       x.angle =    0, y.angle =    0,
                       x.hjust =   .5, x.vjust =   .5,
                       y.hjust = NULL, y.vjust = NULL,
                       legend.key.height = NULL,
                       legend.key.width = NULL,
                       legend.position = "right",
                       panel.background = element_rect()) {
  list(
    theme(
      text  = element_text(family = family),
      title = element_text(family = family, size = t.size, face = t.face),
      strip.text.x = element_text(size = s.size, face = s.face),
      axis.text.x  = element_text(size = x.size, face = x.face, angle = x.angle, hjust = x.hjust, vjust = x.vjust),
      axis.text.y  = element_text(size = y.size, face = y.face, angle = y.angle, hjust = y.hjust, vjust = y.vjust),
      legend.title = element_text(size = l.size, face = l.face),
      legend.text  = element_text(size = l.size, face = l.face),
      legend.key.height = legend.key.height,
      legend.key.width = legend.key.width,
      legend.position = legend.position,
      panel.background = panel.background,
      strip.background = element_rect(colour = "black")
    )
  )
}

theme_shiny <- function(family = "Comic Sans MS",
                        x.size  =      12, y.size =      12, t.size =      12, s.size =      12, l.size =      12,
                        x.face  = "plain", y.face = "plain", t.face = "plain", s.face = "plain", l.face = "plain",
                        x.angle =    0, y.angle =    0,
                        x.hjust =   .5, x.vjust =   .5,
                        y.hjust = NULL, y.vjust = NULL,
                        legend.key.height = NULL,
                        legend.key.width = NULL,
                        legend.position = "right",
                        panel.background = element_rect()) {
  list(
    theme(
      text  = element_text(family = family),
      title = element_text(family = family, size = t.size, face = t.face),
      strip.text.x = element_text(size = s.size, face = s.face),
      axis.text.x  = element_text(size = x.size, face = x.face, angle = x.angle, hjust = x.hjust, vjust = x.vjust),
      axis.text.y  = element_text(size = y.size, face = y.face, angle = y.angle, hjust = y.hjust, vjust = y.vjust),
      legend.title = element_text(size = l.size, face = l.face),
      legend.text  = element_text(size = l.size, face = l.face),
      legend.key.height = legend.key.height,
      legend.key.width  = legend.key.width,
      legend.position   = legend.position,
      panel.background  = panel.background,
      strip.background = element_rect(colour = "black")
    )
  )
}

theme_save <- function(family = "Comic Sans MS",
                       x.size  =      12, y.size =      12, t.size =      14, s.size =      14, l.size =      12,
                       x.face  = "plain", y.face = "plain", t.face = "plain", s.face = "plain", l.face = "plain",
                       x.angle =    0, y.angle =    0, # y.comma = TRUE,
                       x.hjust =   .5, x.vjust =   .5,
                       y.hjust = NULL, y.vjust = NULL,
                       legend.key.height = NULL,
                       legend.key.width = NULL,
                       legend.position = "right",
                       panel.background = element_rect()) {
  list(
    theme(
      text  = element_text(family = family),
      title = element_text(family = family, size = t.size, face = t.face),
      strip.text.x = element_text(size = s.size, face = s.face),
      axis.text.x  = element_text(size = x.size, face = x.face, angle = x.angle, hjust = x.hjust, vjust = x.vjust),
      axis.text.y  = element_text(size = y.size, face = y.face, angle = y.angle, hjust = y.hjust, vjust = y.vjust),
      legend.title = element_text(size = l.size, face = l.face),
      legend.text  = element_text(size = l.size, face = l.face),
      legend.key.height = legend.key.height,
      legend.key.width = legend.key.width,
      legend.position = legend.position,
      panel.background  = panel.background,
      strip.background = element_rect(colour = "black")
    )
  )
}

scale_gender_manual <- function(x, gender = c("1", "2"), colors = "vuw.two.colors1") {
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
