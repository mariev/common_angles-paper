setwd("/Users/heike/papers/2012-common_angles-paper/images/")
library(RColorBrewer)
library(ggparallel)
cols <- c(brewer.pal(name="Blues", 6)[-c(1,2)], rev(brewer.pal(name="Oranges", 3)[-1]), rev(brewer.pal(name="Greens",3)[-1]))
ggparallel(names(titanic)[c(1,4,2,1)], order=c(0,1,1,0), method="hammock", ratio=.25, text.angle=0, titanic, weight="Freq") +
  scale_fill_manual(values=cols, guide="none") +
  scale_colour_manual(values=cols, guide="none") + coord_flip() +theme_bw()
ggsave("hammock-titanic.pdf", width=6, height=6)

ggparallel(names(titanic)[c(1,4,2,1)], order=c(0,1,1,0), titanic, weight="Freq", text.angle=0) + 
  scale_fill_manual(values=cols, guide="none") +
  scale_colour_manual(values=cols, guide="none") + coord_flip() +theme_bw()
ggsave("ca-titanic.pdf", width=6, height=6)


ggparallel(names(titanic)[c(1,4,2,1)], order=c(0,1,1,0), method='adj.angle', text.angle=0, weight="Freq", titanic, ratio=0.035) + 
  scale_fill_manual(values=cols, guide="none") +
  scale_colour_manual(values=cols, guide="none") + coord_flip() +theme_bw() +
  theme(axis.ticks=element_blank(), axis.title.x=element_blank(), 
        axis.text.x=element_blank()) 
ggsave("adj-angle.pdf", width=6, height=6)

#############################
titanic$SS <- with(titanic, interaction(Survived, Sex))
titanic$SSA <- with(titanic, interaction(Survived, Sex, Age))
titanic$CSSA <- with(titanic, interaction(Survived, Sex, Age, Class))


tit <-titanic
tit$Sex <- titanic$SS
tit$Age <- titanic$SSA
tit$Class <- titanic$CSSA

cols <- rep(c("orange", "steelblue"),23)

gg <- ggparallel(names(titanic)[c(1, 3, 2, 4)],  order=0, text.angle=0, label=FALSE, tit, weight="Freq", width=0.1) +
  scale_fill_manual(values=cols, guide="none") +
  scale_colour_manual(values=cols, guide="none") + coord_flip()

levels(titanic$Sex) <- c("Female", "Male")
library(reshape)
dfm <- melt(titanic, id.vars="Freq", measure.vars=c(1, 2, 3, 4))

gg2 <- geom_bar(aes(x=variable, weight=Freq, group=value), width=.125, fill="grey40", colour="grey80", data=dfm)

dfm$variable <- factor(dfm$variable, levels=levels(dfm$variable)[c(1,3,2,4)])

label.stats <- ddply(dfm, .(variable, value), summarize,
                     n = length(Freq),
                     weight=sum(Freq)
)
maxWeight <- sum(label.stats$weight)/length(unique(label.stats$variable))
label.stats$ypos <- cumsum(label.stats$weight)-(as.numeric(label.stats$variable)-1)*maxWeight
label.stats$ypos <- label.stats$ypos-label.stats$weight/2

text.offset <- 0
label.stats$text.offset <- rep(text.offset, length=nrow(label.stats))

label.stats$labels <- as.character(label.stats$value)
label.stats$labels[7:8] <- c("Male", "Female")
gt1 <- geom_text(aes(x=as.numeric(variable), y=ypos-0.01, label=labels),
                colour = "black", data=label.stats, angle=0, size=4)
gt <- geom_text(aes(x=as.numeric(variable)+0.01+text.offset, y=ypos-0.01, label=labels),
          colour = "grey90", data=label.stats, angle=0, size=4)

gg + gg2 + gt1 + gt
ggsave(file="ca-hierarchy.pdf", height=7, width=6)  
#############################
data(genes)
genes$chromN <- gsub("chr","", as.character(genes$chrom))
genes$chrom <- with(genes, reorder(chrom, as.numeric(chromN), mean))
levels(genes$chrom)

chrom.stats <- ddply(genes, .(chrom), summarise, n=length(cdsEnd))
chrom.stats$add <- c(0,cumsum(as.numeric(chrom.stats$n))[-nrow(chrom.stats)])

genes <- merge(genes, chrom.stats[,c("chrom", "add")])
genes <- ddply(genes, .(chrom), transform, order=rank(txStart)+add)

qplot(order, chrom, data=genes)

qplot(order, path, colour=chrom, data=genes)


qplot(order, data=subset(genes, path %in% c("hsa04110", "hsa00232", "hsa03410")))

genesub <- subset(genes, path %in% c("hsa04110", "hsa00232", "hsa03410"))
ggparallel(c("path", "order", "path"), order=0, method='angle', label=FALSE,
           text.angle=0, data=genesub) + 
  scale_fill_manual(values=rep("grey", 288), guide="none") +
  scale_colour_manual(values=rep("white",288), guide="none") + coord_flip() 



