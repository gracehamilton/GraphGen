#R script for generating graphs of activity status and work needed according to an individual retailer
#By Grace Hamilton, Data Coordinator/Intern
#August 2021
library(tidyverse)
library(data.table)
retailer <- read.csv(file.choose())
options <- data.table(Amazon.com = c("Active", "Unavailable"), Walmart.com = c("Active", "Out of Stock"), Instacart.com = c("Active", "Not available"), Target.com = c("Active", "Not available"), Kroger.com = c("Active", "Unavailable"))
rname <- retailer$Retailer[1]
rlang <- select(options, matches(rname))
naming<- sprintf("%s_%s", rname, format(Sys.time(), "%m-%d-%y_%H.%M.%S"))
retailer <- retailer %>% group_by(retailer$Retailer.Brand)
#filter data
bcount <- as.data.frame(table(retailer$Retailer.Brand))
bcount <- bcount %>% filter(Freq>3)
bcount <- bcount %>% filter(Freq > mean(bcount$Freq))
retailer <- retailer %>% filter(Retailer.Brand %in% bcount$Var1)
#activity status
status <- table(retailer$Retailer.Brand, retailer$Status)
status.df <- as.data.frame(status)
status.df <- status.df %>% rename(Brand = Var1, Status = Var2, Count = Freq)
status.graph <- ggplot(data = status.df, aes(Brand, Count, fill=Status, width=0.5)) + geom_bar(stat='identity') + theme(axis.text.x = element_text(angle=45, hjust = 1))
status.graph + scale_fill_manual(values = c("chocolate1", "darkturquoise"))
ggsave(sprintf("%s_Activity.jpg", naming))
#work needed
work.needed <- table(retailer$Retailer.Brand, retailer$Status, retailer$Status, retailer$Title.Match, retailer$Description.Match, retailer$Bullet.Match, retailer$Images.Match, retailer$Image.Order.Match)
title.df <- as.data.frame(table(retailer$Retailer.Brand, retailer$Status, retailer$Title.Match)) %>% filter(Var3 == "n")
desc.df <- as.data.frame(table(retailer$Retailer.Brand, retailer$Status, retailer$Description.Match)) %>% filter(Var3 == "n")
bullet.df <- as.data.frame(table(retailer$Retailer.Brand, retailer$Status, retailer$Bullet.Match)) %>% filter(Var3 == "n")
images.df <- as.data.frame(table(retailer$Retailer.Brand, retailer$Status, retailer$Images.Match)) %>% filter(Var3 == "n")
image.order.df <-as.data.frame(table(retailer$Retailer.Brand,retailer$Status, retailer$Image.Order.Match)) %>% filter(Var3 == "n")
together <- as.vector(rbind(title.df, desc.df, bullet.df, images.df, image.order.df))
together <- together %>% rename(Brand = Var1, Status = Var2)
active.need.df <- together %>% filter(Status == toString(rlang[1]))
oos.need.df <- together %>% filter(Status == toString(rlang[2]))
active.need.df <- active.need.df %>% arrange(across(Brand))
vec <- rep(c("Title", "Description", "Bullets", "Images", "Image Order"), times = nrow(bcount))
active.need.df$Category <- vec
active.graph <-ggplot(active.need.df, aes(fill=Category, y=Freq, x=Brand)) + geom_bar(position="dodge", stat="identity")+ theme(axis.text.x = element_text(angle=45, hjust = 1))
active.graph + scale_fill_manual(values = c("chocolate1", "darkturquoise", "chartreuse4", "darkorchid4", "azure4"))
ggsave(sprintf("%s_WorkA.jpg", naming))
oos.need.df <- oos.need.df %>% arrange(across(Brand))
oos.need.df$Category <- vec
oos.graph <-ggplot(oos.need.df, aes(fill=Category, y=Freq, x=Brand)) + geom_bar(position="dodge", stat="identity")+ theme(axis.text.x = element_text(angle=45, hjust = 1))
oos.graph + scale_fill_manual(values = c("chocolate1", "darkturquoise", "chartreuse4", "darkorchid4", "azure4"))
ggsave(sprintf("%s_WorkNA.jpg", naming))