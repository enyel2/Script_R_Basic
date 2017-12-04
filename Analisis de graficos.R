library(ggplot2)
require(ggplot2)
data(diamonds)
head(diamonds)

hist(diamonds$carat, main = "Carat Histogram", xlab = "Carat")

plot(price ~ carat, data = diamonds)

plot(diamonds$carat, diamonds$price)

boxplot(diamonds$carat)

ggplot(data = diamonds) + geom_histogram(aes(x = carat))

ggplot(data = diamonds) + geom_density(aes(x = carat), fill = "grey50")

ggplot(diamonds, aes(x = carat, y = price)) + geom_point()

## basico ggplot de una variable

g <- ggplot(diamonds, aes(x = carat, y = price))

g + geom_point(aes(color=color))

g + geom_point(aes(color=color)) + facet_wrap(~color)

g + geom_point(aes(color = color)) + facet_grid(cut ~ clarity)

ggplot(diamonds, aes(x = carat)) + geom_histogram() + facet_wrap(~color)

ggplot(diamonds, aes(y = carat, x = cut)) + geom_boxplot()

ggplot(diamonds, aes(y = carat, x = cut)) + geom_violin()

ggplot(diamonds, aes(y = carat, x = cut)) + geom_point() + geom_violin()

ggplot(diamonds, aes(y = carat, x = cut)) + geom_violin() + geom_point()

## graficos lineales 

ggplot(economics, aes(x = date, y = pop)) + geom_line()

r <- economics
head(r)

require(lubridate)
economics$year <- year(economics$date)
economics$month <- month(economics$date, label=TRUE)
econ2000 <- economics[which(economics$year >=2000), ]

require(scales)
g <- ggplot(econ2000, aes(x=month, y=pop))
g
g <- g + geom_line(aes(color=factor(year), group=year))
g
g <- g + scale_color_discrete(name="Year")
g
g <- g + scale_y_continuous(labels=comma)
g
g <- g + labs(title="Population Growth", x="Month", y="Population")
g

require(ggthemes)
## construiyendo un grafico del tipo g2
g2 <- ggplot(diamonds, aes(x=carat, y=price)) + geom_point(aes(color=color))

head(diamonds)
summary(diamonds$carat)
summary(diamonds$price)
## aplicando unos pocos "themes"
g2 + theme_economist() + scale_colour_economist()
g2 + theme_excel() + scale_colour_excel()
g2 + theme_tufte()
g2 + theme_wsj()



