data(mtcars)
mtcars$cyl <- factor(mtcars$cyl)
mtcars$vs <- factor(mtcars$vs)
mtcars$gear <- factor(mtcars$gear)
mtcars$carb <- factor(mtcars$carb)
mtcars$am <- factor(mtcars$am,labels=c('Automatic','Manual'))

base_model <- lm(mpg ~ ., data = mtcars)
best_model <- step(base_model, direction = "both")

summary(best_model)

simple_model <- lm(mpg ~ am, data = mtcars)
anova(simple_model, best_model)

par(mfrow = c(2,2))
plot(best_model)

leverage <- hatvalues(best_model)
tail(sort(leverage),4)

group1 <- mtcars[mtcars$am == "Manual",]
group2 <- mtcars[mtcars$am == "Automatic",]
t.test(group1$mpg,group2$mpg)

g = ggplot(mtcars, aes(factor(am), mpg, fill=factor(am)))
g = g + geom_boxplot()
#g = g + geom_jitter(position=position_jitter(width=.1, height=0))
g = g + scale_colour_discrete(name = "Type")
g = g + scale_fill_discrete(name="Type", breaks=c("Automatic", "Manual"),labels=c("Automatic", "Manual"))
g = g + scale_x_discrete(breaks=c("Automatic", "Manual"), labels=c("Automatic", "Manual"))
g = g + xlab("")
g
