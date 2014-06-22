ggplot(data=x, aes(x=Type, y=value, fill=factor(variable))) + geom_bar(colour="black", stat="identity")

plot_1 <- ggplot(xxx, aes(x=Type, y=value, fill=variable)) + geom_bar(position="stack")

p <- qplot(x=Type, y=value, fill=variable,
                       data=xxx, geom="bar", stat="identity",
                       position="dodge")





a <- ddply(tmp, .(Injuries, Fatalities), summarize,
           Total = Injuries + Fatalities)
a <- a[order(a$Total, decreasing = TRUE), ]
a$Type <- tmp$Type
b <- melt(a, id.vars = "Type")
p <- qplot(x=Type, y=value, fill=variable,
data=b, geom="bar", stat="identity",
position="dodge")

