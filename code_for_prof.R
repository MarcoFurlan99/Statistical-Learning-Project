
# NOTE: this code contains all of the code and the outputs shown in the pdf and
# in the slides. For a more detailed explanation of this code, refer to the pdf
# and the slides.


#############################
### Clean and filter data ###
#############################

#import dataset
library(readxl)
data <- read_excel("Sample - Superstore.xls")


sum(is.na(data))
data <- data.frame(data)

# NA values all belong to the Postal.Code variable
# we remove redundant or useless columns (Postal.Code is one of them)

data <- subset(data, select=-c(Row.ID,Country.Region,Customer.Name,Postal.Code,Product.Name))

#Clean dates
library(lubridate)
data$Order.Date.Y <- as.factor(year(data$Order.Date))
data$Order.Date.YM <- as.factor(format(as.Date(data$Order.Date), "%Y-%m"))
data$Order.Date.YMD <- as.factor(format(as.Date(data$Order.Date), "%Y-%m-%d"))
data$Order.Date.M <- as.factor(month(data$Order.Date))
data$Order.Date.D <- as.factor(day(data$Order.Date))
table(data$Discount)
length(unique(data$Discount))

#add categorical variable "Discount"
library(dplyr)
data$Discount.Level[data$Discount == 0] <- "No Discount"
data$Discount.Level[data$Discount > 0 & data$Discount <= 0.3] <- "Low Discount"
data$Discount.Level[data$Discount > 0.3 & data$Discount <= 0.6] <- "Median Discount"
data$Discount.Level[data$Discount > 0.6 & data$Discount <= 1] <- "High Discount"
data$Discount.Level <- as.factor(data$Discount.Level)
attach(data)

names(data)
for (i in c(4,6,7,8,9,11,12)){
  data[,i] <- as.factor(data[,i])
}


####################
### Explore data ###
####################

#histograms of Sales and Profit
par(mfrow=c(1,2))
hist(Sales, breaks = 500, xlab = "Sales Amount(USD)", ylab = '', main = 'Distribution of Sales Amount',cex.main = 0.8)
hist(Profit, breaks = 2000, xlab = "Profit(USD)", ylab = '', main = 'Distribution of Profit', cex.main = 0.8)
par(mfrow=c(1,1))

#remove outliers

outlier.s <- which(Sales > 5000)
data[outlier.s,]
proportion.sample <- (dim(data)[1]-dim(data[outlier.s,])[1]) / dim(data)[1]
proportion.sample
sub.data <- subset(data, Sales<=5000)

outlier.p <- which(sub.data$Profit < -2000)
sub.data[outlier.p,]
proportion.sample <- (dim(sub.data)[1] - dim(sub.data[outlier.p,])[1])/ dim(data)[1]

# the proportion shows we still have 99.7% of our samples
proportion.sample
sub.data <- subset(sub.data, Profit >= -2000)

detach(data)
attach(sub.data)

# plot Sales and Profit (outliers removed)
par(mfrow=c(1,2))
hist(Sales, breaks = 500, xlim = c(0,2000),xlab = "Sales Amount(USD)", ylab = '', main = 'Distribution of Sales Amount',cex.main = 0.8)
hist(Profit, breaks = 2000, xlim = c(-200,500), xlab = "Profit(USD)", ylab = '', main = 'Distribution of Profit', cex.main = 0.8)
par(mfrow=c(1,1))

# plot log(Sales)
library(ggplot2)
ggplot(sub.data, aes(x = log(Sales),y = ..density..)) +
  geom_histogram(bins = 30, fill = "#8dd3c7", size = 0.2) +
  geom_density(size = 1) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))


### Single variable analysis ###

# Compare Sales with Shipmode, Segment, Region and State
ggplot(sub.data, aes(x = Ship.Mode,y = log(Sales),color = Ship.Mode)) + 
  geom_boxplot() + ## scale_color_brewer(palette = "Set2")
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = 'Sales by Shipmode', x = "Shipmode", y = "log(Sales)")

ggplot(sub.data, mapping = aes(x = Segment,y = log(Sales),color = Segment)) + 
  geom_boxplot() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = 'Sales by Segment', x = "Segment", y = "log(Sales)") +
  scale_color_brewer(palette = "Set2") 

ggplot(sub.data, aes(x = Region,y = log(Sales),color = Region)) +
  geom_boxplot() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.key = element_blank(),
        axis.line = element_line(colour = "black")) +
  labs(title = 'Sales by Region', x = "Region", y = "log(Sales)") +
  scale_color_brewer(palette = "Set2")

ggplot(sub.data, aes( x = State,y = log(Sales),color = Region)) + 
  geom_boxplot() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        legend.key = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90),
        axis.ticks.x = element_blank()) +
  labs(title = 'Sales by State', x = "State", y = "log(Sales)") +
  scale_color_brewer(palette = "Set2")

# City contains too many possibilities
length(unique(City))

# Compare Sales with Category and Sub-Category
ggplot(sub.data, aes(x = Category,
                     y = log(Sales),
                     color = Category)) + 
  geom_boxplot() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = 'Sales by Category', x = "Category", y = "log(Sales)") +
  scale_color_brewer(palette = "Set2") 

ggplot(sub.data, aes(x = Sub.Category,
                     y = log(Sales),
                     color = Category)) + 
  geom_boxplot() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = 'Sales by Sub.Category', x = "Sub.Category", y = "log(Sales)") +
  scale_color_brewer(palette = "Set2") 

# Compare Sales with year and month
ggplot(sub.data, aes(x = Order.Date.Y,
                     y = log(Sales),
                     color = Order.Date.Y)) + 
  geom_boxplot() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = 'Sales by Year', x = "Year", y = "log(Sales)") +
  scale_color_brewer(palette = "Set2") 

ggplot(sub.data, aes(x = Order.Date.YM,
                     y = log(Sales),
                     color = Order.Date.Y)) + 
  geom_boxplot() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.text.x = element_text(angle = 90)) +
  labs(title = 'Sales by Year-Month', x = "Year-Month", y = "log(Sales)") +
  scale_color_brewer(palette = "Set2")

# print the correlation matrix between the numerical variables
cor(sub.data[,c(13,14,15,16)])

ggplot(sub.data, aes(x = reorder(Discount.Level, Discount),
                     y = log(Sales),
                     color = Discount.Level)) + 
  geom_boxplot() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = 'Sales by Discount Level', x = "Discount Level", y = "log(Sales)") +
  scale_color_brewer(palette = "Set2")


### Multiple variable analysis ###


#Compare Category and Sub-Category with Sales and Profit
subtotal.category <- sub.data %>%
  group_by(Category, Sub.Category) %>%
  summarise(C.Sales = sum(Sales), C.Profit = sum(Profit))
library(treemapify)

ggplot(subtotal.category,aes(area=C.Sales, label=Sub.Category, 
                             fill=Category, subgroup=Category))+
  geom_treemap()+
  geom_treemap_subgroup_border(colour = "white", size = 5) +
  geom_treemap_subgroup_text(color='white', alpha=0.5, grow = TRUE,  place='bottom', 
                             fontface='italic', reflow = T)+
  scale_fill_brewer(palette = "Set2")+
  geom_treemap_text(color='white', place='topleft', reflow = T)+
  labs(title = "Proportion of Total Sales by Category/Sub category")

ggplot(subtotal.category,aes(area=C.Profit, label=Sub.Category, 
                             fill=Category, subgroup=Category))+
  geom_treemap()+
  geom_treemap_subgroup_border(colour = "white", size = 5)+
  geom_treemap_subgroup_text(color='white', alpha=0.5, grow = TRUE,  place='bottom', 
                             fontface='italic', reflow = T)+
  scale_fill_brewer(palette = "Set2")+
  geom_treemap_text(color='white', place='topleft', reflow = T)+
  labs(title = "Proportion of Total Profit by Category/Sub category")

#Compare Sales and Ship.Mode
 sub.data%>%
  group_by(Sub.Category)%>%
  ggplot(aes(Sub.Category, fill=Ship.Mode))+
  geom_bar(position = 'fill')+
  coord_flip()+
  labs(title='Proportion of different ship mode by Sub Category',
       y='Proportion',
       x='Sub Category')+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"))+
  scale_fill_brewer(palette = "Set2") 


# Compare Sales, Segment and Sub.Category
subtotal.sc <- sub.data %>%
  group_by(Segment,Sub.Category) %>%
  summarize(SC.Sales = sum(Sales), SC.Profit = sum(Profit)) %>%
  arrange(Segment,-SC.Sales)

ggplot(subtotal.sc,aes(Segment, Sub.Category, fill=SC.Sales))+
  scale_fill_distiller(palette = "GnBu", direction = 1)+
  geom_tile(color='white')+
  geom_text(aes(label=paste(round(SC.Sales,0),'$')), color = 'white', size=3)+
  theme_bw()+
  labs(title='Sales: Segment vs. Sub-Category',
       fill='Total Sales')

# Compare State and Sales
subtotal.state <- sub.data %>%
  group_by(State) %>%
  summarize(S.Sales = sum(Sales), S.Profit = sum(Profit)) %>%
  arrange(-S.Sales)

subtotal.state$region <- tolower(subtotal.state$State)
stateMap <- merge(map_data('state'), subtotal.state, by='region')
stateMap %>%
  ggplot(aes(long, lat, group=group))+
  geom_polygon(aes(fill= S.Sales), color = 'white')+
  scale_fill_distiller(palette = "GnBu")+
  labs(title='Total Sales by State', x ="", y="", fill = "Total Sales")+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        plot.title=element_text(size=20, face='bold', hjust=0.5),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

# Top 5 states by sales
top5.state <- subtotal.state[1:5,1:2]
top5.state

# Compare Shipmode and State
sub.data%>%
  group_by(State)%>%
  ggplot(aes(State, fill=Ship.Mode))+
  geom_bar(position = 'fill')+
  coord_flip()+
  labs(title='Proportion of different ship mode by State',
       y='Proportion',
       x='State')+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  scale_fill_brewer(palette = "Set2") 


# Compare Sales and Time

subtotal.m <- sub.data %>%
  group_by(Order.Date.YM) %>%
  summarise(M.Sales = sum(Sales), M.Profit = sum(Profit))

subtotal.mon <- sub.data %>%
  group_by(Order.Date.M) %>%
  summarise(M.Sales = sum(Sales), M.Profit = sum(Profit))

ggplot(subtotal.m, aes(x = Order.Date.YM,
                       y = M.Sales, group = 1)) +
  geom_line(color = "#28BBD7", size = 1) +
  geom_point(color = "#28BBD7", size = 2) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
        axis.line = element_line(colour = "black")) +
  labs(title = 'Sales', x = "", y = "Sales")

ggplot(subtotal.mon, aes(x = Order.Date.M,
                         y = M.Sales, group = 1)) + 
  geom_line(color = "#E78ECD", size = 1) +
  geom_point(color = "#E78ECD", size = 2) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.text.x = element_text( hjust = 1, vjust = .5),
        axis.line = element_line(colour = "black")) +
  labs(title = 'Monthly Sales', x = "Month", y = "Sales") 

# Comparing Sales, Profit and Discount
ggplot(sub.data, aes(x = Sales,
                     y = Profit,
                     color = reorder(Discount.Level,Discount))) + 
  geom_point() +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black")) +
  labs(title = 'Sales vs Profit', x = "Sales", y = "Profit") +
  scale_color_brewer(palette = "Set2")

# colored histogram of discount
hist(Discount, col = c("aquamarine3","darkorange1","darkorange1","darkorange1","darkorange1","darkorange1","darkorange1","lightslateblue","lightslateblue","lightslateblue","lightslateblue","lightslateblue","mediumorchid1","mediumorchid1","mediumorchid1","mediumorchid1"))

# Comparing Discount and other variables
ggplot(sub.data, aes(x = Sub.Category,
                     y = Discount,
)) + 
  geom_boxplot(color = "#00C1B2" ) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.line = element_line(colour = "black")) +
  labs(title = 'Discount by Sub Category', x = "Sub Category", y = "Discount") +
  scale_color_brewer(palette = "Set2") 

ggplot(sub.data, aes(x = State,
                     y = Discount,
)) + 
  geom_boxplot(color = "#00C1B2") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.line = element_line(colour = "black")) +
  labs(title = 'Discount by State', x = "State", y = "Discount")

ggplot(sub.data, aes(x = Segment,
                     y = Discount,
)) + 
  geom_boxplot(color = "#00C1B2") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.line = element_line(colour = "black")) +
  labs(title = 'Discount by Segment', x = "Segment", y = "Discount") +
  scale_color_brewer(palette = "Set2") 

ggplot(sub.data, aes(x = Order.Date.YM,
                     y = Discount,
)) + 
  geom_boxplot(color = "#00C1B2") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.line = element_line(colour = "black")) +
  labs(title = 'Discount by Month', x = "Order Month", y = "Discount") +
  scale_color_brewer(palette = "Set2") 

# Comparing Region, Sub.Category, Sales and Discount ...
subtotal.dc <- sub.data %>%
  group_by(Region,Sub.Category) %>%
  summarize(max.discount = max(Discount), avg.discount = mean(Discount),
            s.Sales = sum(Sales), s.Profit = sum(Profit)) %>%
  arrange(-max.discount)

# ... with max discount
ggplot(subtotal.dc,aes(Region, Sub.Category, fill=s.Sales))+
  scale_fill_distiller(palette = "GnBu", direction = 1)+
  geom_tile(color='white')+
  geom_text(aes(label=paste(max.discount)), color = 'white', size=3)+
  theme_bw()+
  labs(title='Discount: Region vs. Sub-Category',
       fill='Total Sales')

# ... with average discount
ggplot(subtotal.dc,aes(Region, Sub.Category, fill=s.Sales))+
  scale_fill_distiller(palette = "GnBu", direction = 1)+
  geom_tile(color='white')+
  geom_text(aes(label=paste(round(avg.discount,2))), color = 'white', size=3)+
  theme_bw()+
  labs(title='Discount: Region vs. Sub-Category',
       fill='Total Sales')

# same as before, Profit in place of Sales ...
# ... with max discount
ggplot(subtotal.dc,aes(Region, Sub.Category, fill=s.Profit))+
  scale_fill_distiller(palette = "GnBu", direction = 1)+
  geom_tile(color='white')+
  geom_text(aes(label=paste(max.discount)), color = 'white',size=3)+
  theme_bw()+
  labs(title='Profit: Region vs. Sub-Category',
       fill='Total Profit')

# ... with average discount
ggplot(subtotal.dc,aes(Region, Sub.Category, fill=s.Profit))+
  scale_fill_distiller(palette = "GnBu", direction = 1)+
  geom_tile(color='white')+
  geom_text(aes(label=paste(round(avg.discount,2))), color = 'white',size=3)+
  theme_bw()+
  labs(title='Profit: Region vs. Sub-Category',
       fill='Total Profit')

# Compare Segment, Sub-Category, Sales and Discount ...
subtotal.ds <- sub.data %>%
  group_by(Segment, Sub.Category) %>%
  summarize(max.discount = max(Discount), avg.discount = mean(Discount),
            s.Sales = sum(Sales), s.Profit = (Profit)) %>%
  arrange(-max.discount)

# ... with max discount
ggplot(subtotal.ds,aes(Segment, Sub.Category, fill=s.Sales))+
  scale_fill_distiller(palette = "GnBu", direction = 1)+
  geom_tile(color='white')+
  geom_text(aes(label=paste(max.discount)), color = 'white', size=3)+
  theme_bw()+
  labs(title='Discount: Segment vs. Sub-Category',
       fill='Total Sales')

# ... with average discount
ggplot(subtotal.ds,aes(Segment, Sub.Category, fill=s.Sales))+
  scale_fill_distiller(palette = "GnBu", direction = 1)+
  geom_tile(color='white')+
  geom_text(aes(label=paste(round(avg.discount,2))), color = 'white', size=3)+
  theme_bw()+
  labs(title='Discount: Segment vs. Sub-Category',
       fill='Total Sales')

### Other plots (not shown in pdf) ###

#Sales: State vs Month'
subtotal.ds <- sub.data %>%
  group_by(State, Order.Date.M) %>%
  summarize(max.discount = max(Discount), avg.discount = mean(Discount),
            s.Sales = sum(Sales), s.Profit = (Profit))

ggplot(subtotal.ds,aes(State, Order.Date.M, fill=s.Sales))+
  scale_fill_distiller(palette = "GnBu", direction = 1)+
  geom_tile(color='white')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title='Sales: State vs Month',
       x = "State", y = "Order Month",
       fill='Total Sales')

#Sales: State vs Sub Category
subtotal.ds <- sub.data %>%
  group_by(State, Sub.Category) %>%
  summarize(max.discount = max(Discount), avg.discount = mean(Discount),
            s.Sales = sum(Sales), s.Profit = (Profit))

ggplot(subtotal.ds,aes(State, Sub.Category, fill=s.Sales))+
  scale_fill_distiller(palette = "GnBu", direction = 1)+
  geom_tile(color='white')+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90))+
  labs(title='Sales: State vs Sub Category',
       x = "State", y = "Sub Category",
       fill='Total Sales')

#Month vs Sub category
subtotal.ds <- sub.data %>%
  group_by(Order.Date.M, Sub.Category) %>%
  summarize(s.Sales = sum(Sales), s.Profit = (Profit))
#arrange(-max.discount)

ggplot(subtotal.ds,aes(Order.Date.M, Sub.Category, fill=s.Sales))+
  scale_fill_distiller(palette = "GnBu", direction = 1)+
  geom_tile(color='white')+
  #geom_text(aes(label=paste(round(s.Sales,0),'$')), color = 'white', size=3)+
  theme_bw()+
  labs(title='Sales: Order Month vs. Sub Category',
       x = "Order Month", y = 'Sub Category',
       fill='Total Sales')

#Month vs Category
subtotal.ds <- sub.data %>%
  group_by(Order.Date.M, Category) %>%
  summarize(s.Sales = sum(Sales), s.Profit = (Profit))
#arrange(-max.discount)

ggplot(subtotal.ds,aes(Order.Date.M, Category, fill=s.Sales))+
  scale_fill_distiller(palette = "GnBu", direction = 1)+
  geom_tile(color='white')+
  #geom_text(aes(label=paste(round(s.Sales,0),'$')), color = 'white', size=3)+
  theme_bw()+
  labs(title='Sales: Order Month vs. Category',
       x = "Order Month", y = 'Category',
       fill='Total Sales')

#month vs quantity
ggplot(sub.data, aes(x = Order.Date.M,
                     y = Quantity,
)) + 
  geom_boxplot(color = "#00C1B2") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.line = element_line(colour = "black")) +
  labs(title = 'Quantity by Order Month', x = "Order Month", y = "Quantity")

#Sub category vs Quantity
ggplot(sub.data, aes(x = Sub.Category,
                     y = Quantity,
)) + 
  geom_boxplot(color = "#00C1B2") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.line = element_line(colour = "black")) +
  labs(title = 'Quantity by Sub', x = "Sub Category", y = "Quantity")

#State with Quantity
ggplot(sub.data, aes(x = State,
                     y = Quantity,
)) + 
  geom_boxplot(color = "#00C1B2") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.line = element_line(colour = "black")) +
  labs(title = 'Quantity by State', x = "State", y = "Quantity")

#################
### Modelling ###
#################

### Model 1: based on previous results

lm.mod1 <- lm(log(Sales)~State+Sub.Category+Order.Date.M+Discount.Level+Quantity
              +Discount.Level:Sub.Category+Discount.Level:State
              ,sub.data)

par(mfrow=c(2,2))
plot(lm.mod1)
par(mfrow=c(1,1))

### Model 2: constraint backward elimination

library(leaps)

mod.F <- lm(log(Sales)~State+Sub.Category+Order.Date.M+Discount.Level+Quantity
            +Discount.Level:Sub.Category+Discount.Level:State
            ,sub.data)
summary(mod.F)

# step 1

mod.R <- update(mod.F, .~.-Order.Date.M)
anova(mod.R, mod.F)
summary(mod.R)

# step 2

mod.R <- update(mod.R, .~.-Discount.Level:Sub.Category)
anova(mod.R, mod.F)
summary(mod.R)

# step3

mod.R <- update(mod.R, .~.-Discount.Level:State)
anova(mod.R, mod.F)
summary(mod.R)

# best model
con.best <- lm(log(Sales)~State+Sub.Category+Discount.Level+Quantity
               +Discount.Level:State,sub.data)

summary(con.best)

par(mfrow=c(2,2))
plot(con.best)
par(mfrow=c(1,1))

### Model 3: Lasso Regression

library(glmnet)
# design matrix 
X <- model.matrix(log(Sales)~State+Sub.Category+Order.Date.M+Discount.Level+Quantity
                  +Discount.Level:Sub.Category+Discount.Level:State, sub.data)

# remove the first column relative to the intercept 
X <- X[,-1]

# vector of responses
y <- sub.data$Sales

#select 75%*n observation for training set

set.seed(25)
train <- sample(1:nrow(X), nrow(X)*0.8)
test <- (-train)
y.test <- y[test]

# apply lasso to the training set without specifying lambda
lasso.mod <- glmnet(X[train,], y[train], alpha=1)
plot(lasso.mod, label=TRUE)

# use 10 folds cross-validation to choose the value of lambda 
cv.out <- cv.glmnet(X[train, ], y[train], alpha = 1, nfold=10)
plot(cv.out)

# identify the best lambda value estimated test MSE
bestlam <- cv.out$lambda.min
bestlam

# fit the coefficient with lambda=bestlam on all the data
out <- glmnet(X, y, alpha = 1)
lasso.coef <- predict(out, type="coefficients", s=bestlam)
i <- which(lasso.coef[,c("s1")]!=0)
names(lasso.coef[i,])


### Comparing Model 2 and Model 3

# Model 2:
con.best <- lm(log(Sales)~State+Sub.Category+Discount.Level+Quantity
               +Discount.Level:State,sub.data)
con.pred <- predict(con.best, newdata=sub.data)
mean((con.pred-y)^2)

# Model 3:
lasso.pred <- predict(lasso.mod, s=bestlam, newx=X[test,])
mean((lasso.pred-y.test)^2)

### Model 4: A simpler model

mod <- lm(log(Sales)~Sub.Category+Discount.Level+Quantity
              ,sub.data[train,])
mod.pred <- predict(mod, newdata=sub.data[test,])
mean((mod.pred-y.test)^2)
summary(mod)

