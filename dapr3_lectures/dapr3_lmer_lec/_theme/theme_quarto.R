

blend_colors <- function(x, y, alpha = 0.5) {
  x <- colorspace::hex2RGB(x)
  y <- colorspace::hex2RGB(y)
  z <- colorspace::mixcolor(alpha, x, y)
  colorspace::hex(z)
}
color_blender <-
  function(x, y)
    function(alpha = 0.5)
      blend_colors(x, y, alpha)
theme_quarto <- function (text_color = color_text,
                          background_color = color_bg,
                          text_font_size = 30,
                          accent_color = color_base,
                          title_font_size = 30) {
  blend <- color_blender(text_color, background_color)
  ggplot2::theme(
    line = ggplot2::element_line(color = blend(0.2)),
    rect = ggplot2::element_rect(fill = background_color),
    title = ggplot2::element_text(
      color = accent_color,
      size = title_font_size
    ),
    plot.background = ggplot2::element_rect(fill = background_color,
                                            color = background_color),
    panel.background = ggplot2::element_rect(fill = background_color,
                                             color = background_color),
    panel.grid.major = ggplot2::element_line(color = blend(0.8),
                                             inherit.blank = TRUE),
    panel.grid.minor = ggplot2::element_line(color = blend(0.9),
                                             inherit.blank = TRUE),
    axis.title = ggplot2::element_text(size = title_font_size * 0.8),
    axis.ticks = ggplot2::element_line(color = blend(0.8)),
    axis.text = ggplot2::element_text(color = blend(0.4), size = title_font_size * 0.7),
    legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.text = ggplot2::element_text(size = title_font_size * 0.8),
    plot.caption = ggplot2::element_text(size = text_font_size * 0.8,
                                         color = blend(0.3)),
    strip.text.x = element_text(size = 16)
  )
}

update_geom_defaults("line",list(size=2))

knitr::opts_chunk$set(dev.args = list(bg="transparent"))

options(digits=3,scipen=8)


knitr::opts_chunk$set(
  fig.asp=.8,
  tidy=TRUE,
  tidy.opts=list(width.cutoff=70, arrow=TRUE, indent=2, args.newline=TRUE)
)
knitr::knit_hooks$set(crop = knitr::hook_pdfcrop)
color_base <- '#458c3c'
color_text <- '#000000'
color_bg <- '#ffffff'
theme_set(theme_quarto())

### my functions

.pp <- function(command,top=0,bottom=0,l=FALSE) {
  t <- capture.output(eval(command))
  ln <- length(t)
  if (class(l)!='logical') {
    i=0
    for (n in l) {
      if (i>0) {
        cat("...",sep="\n")
      }
      i=i+1
      # if (length(n)==1 && (i == 1 || i == length(l))) {
      #   if (n<0) {
      #     s <- n+1+ln
      #     n <- c(s:ln)
      #   } else {
      #     n <- c(1:n)
      #   }
      # }
      if (!(0 %in% n)) {
        cat(t[n],sep="\n")
      }
    }
  } else {
    if (top != 0) {
      cat(t[1:top],sep="\n")
    }
    cat("...",sep="\n")
    if (bottom !=0) {
      bottom <- (1-bottom)+ln
      cat(t[bottom:ln],sep="\n")
    }
  }
}

.rround <- function(x,d=NA,drop.zero=F) {
  if (!is.na(d)) {
    y <- format(round(as.numeric(x),d),nsmall=d)
  } else {
    y <- format(x)
  }
  if (drop.zero) {
    y <-  sub('^(-)?0\\.','\\1.',y)
  }
  y
}

.pv <- function(x) {
  .rround(x,4,T)
}
