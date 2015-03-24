virus = read.table("c:/Users/Farjala/Documents/R/virus.txt", header=TRUE)
head(virus)
summary(virus)
virus1<-lm(Value~Treatment, data = virus)
summary(virus1)
library(ggplot2)
ggplot(virus, aes(x=as.factor(Treatment), y=Value)) + geom_boxplot()+
  scale_y_continuous(limits = c(0, 4000000)) + 
  xlab("Treatment") + ylab("Virus Abundance") +
  theme_bw() + theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.x = element_blank()) + # retirei as linhas de grade e peinel de fundo cinza
  theme(axis.title = element_text(size = rel(1.5))) + theme(axis.text = element_text(size = rel(1.2)))