
# Hello

# -----------------  User input  --------------------

# replace read_excel(name_File) with read_csv(name_File)
# if data is in a different type of file

# EDIT the next two lines with main_Title and name_File !!!!!

# ---------------------------------------------------

main_Title <-
  c("Blue Foam") # Write title between quotation marks

name_File <-
  c("dest_bluefoam.xlsm") # Write data filename between quotation marks

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
  paste(file_path_sans_ext(basename(name_File)), "_female.pdf", sep = "")

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


nQui <- dt3 %>% select(1,3,5,7) %>% rowSums(na.rm = TRUE)

Qui <- dt3 %>% select(2,4,6,8) %>% rowSums(na.rm = TRUE)

nQui_Qui <- round(tibble(nQui, Qui) * 100, 2)

head(nQui_Qui , 5)

statistics <-
  nQui_Qui  %>% summarise_all(list(mean= ~mean(.), ci = ~sd(.) / sqrt(n()) * 1.95))

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


dt2Q <- dt2[,c(1,3,5,7,9)]

dt2nQ <- dt2[,c(1,2,4,6,8)]

dt3Q <- behaviour_proportion(dt2Q)

dt3nQ <- behaviour_proportion(dt2nQ)

dt3Q <- dt3Q %>% select(-1:-5)

dt3nQ <- dt3nQ %>% select(-1:-5)

movQ <- dt3Q %>% select(1,2) %>% rowSums(na.rm = TRUE)

movnQ <- dt3nQ %>% select(1,2) %>% rowSums(na.rm = TRUE)

immQ <- dt3Q %>% select(3,4) %>% rowSums(na.rm = TRUE)

immnQ <- dt3nQ %>% select(3,4) %>% rowSums(na.rm = TRUE)

Q_F <- round(tibble(immQ, immnQ, movQ, movnQ) * 100, 2)

head(Q_F, 5)

temp <- gather(rownames_to_column(Q_F), quivering, value, immQ:movnQ)

tempQ <- temp %>% dplyr::filter(quivering %in% c("immQ", "movQ"))

tempnQ <- temp %>% dplyr::filter(quivering %in% c("immnQ", "movnQ"))

QPlot <-
  ggplot(tempQ, aes(x = reorder(rowname, sort(as.numeric(rowname))), y = value)) + geom_point(size = 4, aes(colour = factor(quivering))) + theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) + scale_y_continuous(position = "right", breaks = seq(0, 100, 20), limits = c(-1, 100)) + labs(
    title = "Male quivering",
    subtitle = paste(
      format(round(statistics$Qui_mean, digits = 1), nsmall = 1),
      "% \u00B1 ",
      format(round(statistics$Qui_ci, digits = 1), nsmall = 1),
      sep = ""
    ),
    x = "pair #",
    y = "  "
  ) + scale_color_manual(labels = c("Immobile", "Mobile"),values = c("immQ" = "red", "movQ" = "black")) + labs(color="Female Behaviour")

nQPlot <-
  ggplot(tempnQ, aes(x = reorder(rowname, sort(as.numeric(rowname))), y = value)) + geom_point(size = 4, aes(colour = factor(quivering))) + theme(
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
    title = "Male no quivering",
    subtitle = paste(
      format(round(statistics$nQui_mean, digits = 1), nsmall = 1),
      "% \u00B1 ",
      format(round(statistics$Qui_ci, digits = 1), nsmall = 1),
      sep = ""
    ),
    x = "pair #",
    y = "Female Behaviour (%)"
  ) + scale_color_manual(labels = c("Immobile", "Mobile"),values = c("immnQ" = "red", "movnQ" = "black")) + labs(color="Female Behaviour")

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


gg <- grid_arrange_shared_legend(nQPlot, QPlot, ncol = 2, nrow = 1)

pdf(pdfOutput, width = 7.5, height = 7)

grid.arrange(gg,
             heights = c(4, 2.0),
             top = textGrob(main_Title, gp = gpar(fontsize = 20, font = 1)))

dev.off()

