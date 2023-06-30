
extras <-
  c("tidyverse", "readxl", "showtext")

if (length(setdiff(extras, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(extras, rownames(installed.packages())))
}

lapply(extras, require, character.only = TRUE)

data <- read_excel("or.xlsm")

font_add(family = "Arial", regular = "Arial.ttf") ## here is the path to the font to add.

showtext_auto()

cat("\nUsage: bin.movies(DATA)\n")

bin.movies <- function(DATA, return_as_data.frame = F, ...)
{
number_of_columns <- dim(DATA)[2]
final_number <- (8*number_of_columns)+1
total <- seq_len(number_of_columns)
OUT <- matrix(c(0,0,0,0),ncol=1,byrow=TRUE)
OUT <- as.table(OUT)
bev <- seq_len(8)

if(number_of_columns>1){
	for(a in total){
	x <- DATA[,a]
		for(i in bev){
		prop <- which(x==i) / sum(!is.na(x))*100
		result <- cut(prop,breaks=c(0,25,50,75,100),dig.lab = 3, right=  + FALSE)
		result <- as.data.frame(table(result))
		OUT <- cbind(OUT, as.numeric(result[,2]))
		colnames(OUT)[ncol(OUT)] <- i
		}
	OUTa <- OUT[1:4,2:9]}

}

if(number_of_columns==1){
for(i in bev){
x = DATA[,1]
prop <- which(x==i) / sum(!is.na(x))*100

result <- cut(prop,breaks=c(0,25,50,75,100),dig.lab = 0, right=  + FALSE)
result <- as.data.frame(table(result))
OUT <- cbind(OUT, as.numeric(result[,2]))
colnames(OUT)[ncol(OUT)] <- i
}

}
OUT <- t(rowsum(t(OUT), group = colnames(OUT), na.rm = T)) # merge same name columns
OUT <- round(prop.table(OUT,2), digits=4)
OUT <- OUT[,-9]
#OUT <- OUT[1:4,2:final_number]

rownames(OUT) <- c("0-25", "25-50", "50-75", "75-100")
OUT


}

test <- gather(as_tibble(bin.movies(data)) %>% mutate(bin=rownames(bin.movies(data))), behaviour, value, -bin)

ggplot(test, aes(x=bin, y=value, color=behaviour)) + geom_point(position = position_jitter(w = 0.2, h = 0), size=7) + scale_color_manual(values=cbp1, name="behaviour") + theme( legend.background = element_blank(), text = element_text(size = 20), panel.grid.major = element_blank(), panel.grid.minor = element_blank())