extras <-
  c("tidyverse", "readxl", "showtext", "igraph")

if (length(setdiff(extras, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(extras, rownames(installed.packages())))
}

lapply(extras, require, character.only = TRUE)

font_add(family = "Arial", regular = "Arial.ttf") ## here is the path to the font to add.

showtext_auto()

########################################
######### Change Next 3 Lines ##########

frequencies <- c(0.15)

genotype <- c("OrR")

data <- read_excel("or.xlsm")

########################################
########################################
  
summ_data <- data %>% map(function(x) tibble(behaviour = x[-length(x)], followed_by = x[-1])) %>% bind_rows()  %>%
  group_by(behaviour, followed_by) %>%
  filter(behaviour != followed_by) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N)) %>%
  mutate_at(c(1,2), as.numeric)


ggplot(summ_data, aes(x = behaviour, y = followed_by)) +
  geom_point(aes(size =freq, colour=cut(freq, seq(0, 1, by = 0.2)))) +
  scale_y_continuous(limits = c(0.5, 8.5), breaks = seq(1, 8, by = 1)) +
  scale_x_continuous(limits = c(0.5, 8.5), breaks = seq(1, 8, by = 1)) +
  guides(size = FALSE, color = guide_legend(override.aes = list(size=5))) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(), text = element_text(size=20)) +
  scale_size(range=c(2,30),breaks=seq(0.1,1.0, by = 0.2), limits=c(0,1)) +
  labs(title= paste("Next behaviour ", genotype), subtitle="Excluding itself") +
  scale_color_discrete(name = "Frequency", labels = c("0.0 - 0.2", "0.2 - 0.4", "0.4 - 0.6", "0.6 - 0.8"))

# https://stackoverflow.com/questions/16875547/using-igraph-how-to-force-curvature-when-arrows-point-in-opposite-directions

autocurve.edges2 <-function (graph, start = 0.5)
{
  cm <- count.multiple(graph)
  mut <-is.mutual(graph)  #are connections mutual?
  el <- apply(get.edgelist(graph, names = FALSE), 1, paste,
              collapse = ":")
  ord <- order(el)
  res <- numeric(length(ord))
  p <- 1
  while (p <= length(res)) {
    m <- cm[ord[p]]
    mut.obs <-mut[ord[p]] #are the connections mutual for this point?
    idx <- p:(p + m - 1)
    if (m == 1 & mut.obs==FALSE) { #no mutual conn = no curve
      r <- 0
    }
    else {
      r <- seq(-start, start, length = m)
    }
    res[ord[idx]] <- r
    p <- p + m
  }
  res
}

cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

cbp <- tibble(behaviour=1:8, color=cbp1)

summ_data <- inner_join(summ_data, cbp)

g <- graph_from_data_frame(select(mutate(filter(summ_data, freq > frequencies), freq=round(freq,2)), -3))

curves <- autocurve.edges2(g)

par(bg = 'black')

plot(g,
     edge.label=E(g)$freq,
     edge.loop.angle=-pi/2,
     edge.label.cex=0.6,
     vertex.color="gold3",
     vertex.label.cex=1.2,
     edge.curved=curves,
     edge.label.color="white",
     edge.box.col="red",
     edge.arrow.size=.4)

title(paste("OrR (frequencies ≥ ", frequencies, " )"), col.main="white")



