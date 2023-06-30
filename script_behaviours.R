
# Hello

# -----------------  User input  --------------------

# replace read_excel(name_File) with read_csv(name_File)
# if data is in a different type of file

# EDIT the next two lines with main_Title and name_File !!!!!

# ---------------------------------------------------

main_Title <-
  c("CS") # Write title between quotation marks

name_File <-
  c("dest_cactus.xlsm") # Write data filename between quotation marks

# --------------  loading packages   ----------------

extras <-
  c("tidyverse", "grid", "gridExtra", "gtable", "readxl", "tools", "flextable", "officer", "magick")

if (length(setdiff(extras, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(extras, rownames(installed.packages())))
}

# ---------------------------------------------------

lapply(extras, require, character.only = TRUE)

# ---------------------------------------------------

pdfOutput <-
  paste(file_path_sans_ext(basename(name_File)), ".pdf", sep = "")

# dt <- read.csv(name_File)

dt <- read_excel(name_File)

dt2 <- dt %>%
  mutate(ID = 1:n()) %>%
  gather(Group, x,-ID) %>%
  select(-ID) %>%
  count(Group, x) %>%
  spread(Group, n, fill = 0L) %>%
  mutate(x = paste("behaviour", x, sep = "-"))

dt2 <- dt2 %>%
  gather(var, val, 2:ncol(dt2)) %>%
  spread(x, val)  %>%
  select(-10)

head(dt2, 5)

behaviour_proportion <- function(d) {
  require(tidyverse)
  
  d_gather <- d %>%
    gather(key = "behaviour", value = "behaviour_count",-var)
  
  d_sum <- d_gather %>%
    group_by(var) %>%
    summarise(var_sum = sum(behaviour_count))
  
  d_prop <- d_gather %>%
    left_join(d_sum, by = "var") %>%
    mutate(prop = behaviour_count / var_sum) %>%
    select(-behaviour_count,-var_sum) %>%
    mutate(behaviour = paste0(behaviour, "_prop")) %>%
    spread(key = behaviour, value = prop)
  
  d %>% left_join(d_prop, by = "var")
  
}

dt3 <- behaviour_proportion(dt2)

dt3 <- dt3 %>% select(-1:-9)

head(dt3, 5)

mov <- dt3 %>% select(1:4) %>% rowSums(na.rm = TRUE)

imm <- dt3 %>% select(5:8) %>% rowSums(na.rm = TRUE)

imm_mov <- round(tibble(imm, mov) * 100, 2)

head(imm_mov, 5)

statistics <-
  imm_mov %>% summarise_all(list(mean= ~mean(.), ci = ~sd(.) / sqrt(n()) * 1.95))

statistics <- round(statistics, 2)

statistics[,c(1,3,2,4)]

nQnF <- dt3 %>% select(c(1,5)) %>% rowSums(na.rm = TRUE)

Quiv <- dt3 %>% select(c(2,6)) %>% rowSums(na.rm = TRUE)

Flut <- dt3 %>% select(c(3,7)) %>% rowSums(na.rm = TRUE)

QF <- dt3 %>% select(c(4,8)) %>% rowSums(na.rm = TRUE)

Alles <- round(tibble(nQnF, Quiv, Flut, QF) * 100, 2)

statisticsQF <- Alles %>% summarise_all(list(mean= ~mean(.), ci = ~sd(.) / sqrt(n()) * 1.95))

statisticsQF <- round(statisticsQF, 2)

statisticsQF[,c(1,5,2,6,3,7,4,8)]

dt2I <- dt2[,c(1,6:9)]

dt2M <- dt2[,c(1,2:5)]

dt3I <- behaviour_proportion(dt2I)

dt3M <- behaviour_proportion(dt2M)

dt3I <- dt3I %>% select(-1:-5)

dt3M <- dt3M %>% select(-1:-5)


movQ <- dt3M %>% select(2, 4) %>% rowSums(na.rm = TRUE)

movF <- dt3M %>% select(3, 4) %>% rowSums(na.rm = TRUE)

immQ <- dt3I %>% select(2, 4) %>% rowSums(na.rm = TRUE)

immF <- dt3I %>% select(3, 4) %>% rowSums(na.rm = TRUE)

Q_F <- round(tibble(immQ, immF, movQ, movF) * 100, 2)

head(Q_F, 5)

temp <- gather(rownames_to_column(Q_F), mobility, value, immQ:movF)

tempimm <- temp %>% dplyr::filter(mobility %in% c("immQ", "immF"))

tempmov <- temp %>% dplyr::filter(mobility %in% c("movQ", "movF"))


immPlot <-
  ggplot(tempimm, aes(x = reorder(rowname, sort(as.numeric(rowname))), y = value)) + geom_point(size = 4, aes(colour = factor(mobility))) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) + scale_y_continuous(position = "right", breaks = seq(0, 100, 20), limits = c(-1, 100)) + labs(
    title = "Female immobile",
    subtitle = paste(
      format(round(statistics$imm_mean, digits = 1), nsmall = 1),
      "% \u00B1 ",
      format(round(statistics$imm_ci, digits = 1), nsmall = 1),
      sep = ""
    ),
    x = "pair #",
    y = "  "
  ) + scale_colour_discrete(name = "Male Behaviour:",
                            labels = c("Fluttering", "Quivering"))

movPlot <-
  ggplot(tempmov, aes(x = reorder(rowname, sort(as.numeric(rowname))), y = value)) + geom_point(size = 4, aes(colour = factor(mobility))) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) + scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(-1, 100)
  ) + labs(
    title = "Female mobile",
    subtitle = paste(
      format(round(statistics$mov_mean, digits = 1), nsmall = 1),
      "% \u00B1 ",
      format(round(statistics$mov_ci, digits = 1), nsmall = 1),
      sep = ""
    ),
    x = "pair #",
    y = "Male Behaviour (%)"
  ) + scale_colour_discrete(name = "Male Behaviour:",
                            labels = c("Fluttering", "Quivering"))

#gg <- grid.arrange(movPlot, immPlot, ncol = 2)

# https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}


gg <- grid_arrange_shared_legend(movPlot, immPlot, ncol = 2, nrow = 1)

dt4M <- dt3M[, -1] * 100

colnames(dt4M) <- c("Quivering", "Fluttering", "Both")

dt4I <- dt3I[, -1] * 100

colnames(dt4I) <- c("Quivering", "Fluttering", "Both")


dt5M <-
  dt4M %>% summarise_all(list(mean= ~mean(.), ci = ~sd(.) / sqrt(n()) * 1.95))

dt5M <- format(round(dt5M, digits = 1), nsmall = 1)

dt5I <-
  dt4I %>% summarise_all(list(mean= ~mean(.), ci = ~sd(.) / sqrt(n()) * 1.95))

dt5I <- format(round(dt5I, digits = 1), nsmall = 1)

Fancy <- tribble(
  ~ Female,
  ~ Quivering,
  ~ Fluttering,
  ~ Both,
  "Mobile",
  paste(dt5M[1, 1], "% \u00B1 ", dt5M[1, 4], sep = ""),
  paste(dt5M[1, 2], "% \u00B1 ", dt5M[1, 5], sep = ""),
  paste(dt5M[1, 3], "% \u00B1 ", dt5M[1, 6], sep = ""),
  "Immobile",
  paste(dt5I[1, 1], "% \u00B1 ", dt5I[1, 4], sep = ""),
  paste(dt5I[1, 2], "% \u00B1 ", dt5I[1, 5], sep = ""),
  paste(dt5I[1, 3], "% \u00B1 ", dt5I[1, 6], sep = ""),
)

Fancy

grobFancy <- tableGrob(Fancy, rows = NULL)

grobFancy <- gtable_add_row_space(grobFancy,  unit(0.2, "cm"))

grid.newpage()

h <- grobHeight(grobFancy)

w <- grobWidth(grobFancy)

title <-
  textGrob(
    "Male Behaviour",
    y = unit(0.5, "npc") + 0.5 * h,
    vjust = -3,
    gp = gpar(fontsize = 14)
  )
footnote <-
  textGrob(
    "",
    x = unit(0.5, "npc") - 3 * w,
    y = unit(0.5, "npc") - 0.5 * h,
    vjust = 5,
    hjust = 0,
    gp = gpar(fontface = "italic")
  )
gt <- gTree(children = gList(grobFancy, title, footnote))

grid.draw(gt)

pdf(pdfOutput, width = 7.5, height = 7)

grid.arrange(gg,
             gt,
             heights = c(4, 2.0),
             top = textGrob(main_Title, gp = gpar(fontsize = 20, font = 1)))

dev.off()


Fancy <- flextable(Fancy)

Fancy <- theme_zebra(Fancy)

Fancy <- autofit(Fancy)

ft <- flextable(statistics[,c(1,3,2,4)])

ft <- theme_zebra(ft)

ft <- autofit(ft)

ft1 <- flextable(statisticsQF[,c(1,5,2,6,3,7,4,8)])

ft1 <- theme_zebra(ft1)

ft1 <- autofit(ft1)


text_style <- fp_text(font.size = 14, font.family = "Arial", bold = TRUE)
par_style <- fp_par(text.align = "center")

doc <- read_docx()

doc <- body_add_fpar(doc, fpar( ftext(main_Title, prop = text_style), fp_p = par_style ) )

doc <- body_add_par(doc, value = "   ", style = "centered")

doc <- body_add_par(doc, value = "Female behaviour", style = "centered")

doc <- body_add_flextable(doc, value = ft)

doc <- body_add_par(doc, value = "   ", style = "centered")

doc <- body_add_par(doc, value = "Male behaviour", style = "centered")

doc <- body_add_flextable(doc, value = ft1)

doc <- body_add_par(doc, value = "   ", style = "centered")

doc <- body_add_par(doc, value = "Behaviour (mean ± ci)", style = "centered")

doc <- body_add_flextable(doc, value = Fancy)

print(doc, target = paste(file_path_sans_ext(basename(name_File)), ".docx", sep = ""))
