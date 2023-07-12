library(tidyverse)
library(cowplot)

theme_set(theme_minimal()+
            theme(plot.background = element_rect(colour = "white"),
                  legend.position = c(.85,.8),
                  panel.grid.major = element_blank(),
                  text = element_text(family = "serif", colour = "black"),
                  axis.text = element_text(colour = "black"),
                  axis.line.x = element_line(colour = "lightgrey"),
                  axis.ticks.x = element_line(colour = "lightgrey", lineend = "round")
                  )
          )

#########BUILDING THE DATASET #########

# set.seed(8)
# l1 <- runif(10000, 0, 1)
# l2 <- runif(10000, 0, 1)
# c<- runif(10000, 0, 1)
# #c <- rep(c(.3), each=10000)
# 
# 
# df<- data.frame(l1, l2, c)
# 
# ###WHEN LAMBDAS ARE DIFFERENT ###
# 
# dfst<- df %>% 
#   mutate(q11= (1-c)/2,
#          q21= (1-c)/4,
#          q12= ((l2*(q21*(l1 - 2) - 2*c + l1*q11 - 2*q11 + 2) + l1*(q21 + c + q11 - 1))/(2*l2*l1)),
#          q22 = 1/4*((q21*(l1 + 2) + 2*c + l1*q11 + 2*q11 - 2)/l1 - (3*(q21 + c + q11 - 1))/l2),
#          ratioq= q12/q22,
#          ratiol=l1/l2, 
#          p1= 1-q11-q21,
#          p2= 1- q12-q22,
#          profit11= q11*(p1-c),
#          profit21= q21*(p1-c),
#          profit12= l1*(p2-c)*q12 + (1-l1)*(p1-c)*q12 ,
#          profit22= l2*(p2-c)*q22 + (1-l2)*(p1-c)*q22,
#          type= "stackelberg"
#   )
# 
# dfcourn <- df %>% 
#   mutate(q12= -((c - 1)*(2*l2*(l1 + 1) - l1))/(9*l2*l1), 
#          q22= -((c - 1)*(l2*(2*l1 - 1) + 2*l1))/(9*l2*l1), 
#          q11= (1 - c)/3, 
#          q21= (1 - c)/3,
#          ratioq= q12/q22,
#          ratiol=l1/l2, 
#          p1= 1-q11-q21,
#          p2= 1- q12-q22,
#          profit11= q11*(p1-c),
#          profit21= q21*(p1-c),
#          profit12= l1*(p2-c)*q12 + (1-l1)*(p1-c)*q12 ,
#          profit22= l2*(p2-c)*q22 + (1-l2)*(p1-c)*q22,
#          type= "cournot"
#   )
# 
# # dfcourn %>% 
# #   mutate(dif=q12-q22) %>% 
# #   summarise(mean(q12-q22))
# 
# 
# df1 <- rbind(dfst, dfcourn)
# 
# ###WHEN LAMBDAS ARE EQUAL ###
# 
# dfst2 <- df %>% 
#   mutate(l2=l1,
#          q11= (1-c)/2,
#          q21= (1-c)/4,
#          q12= ((l2*(q21*(l1 - 2) - 2*c + l1*q11 - 2*q11 + 2) + l1*(q21 + c + q11 - 1))/(2*l2*l1)),
#          q22 = 1/4*((q21*(l1 + 2) + 2*c + l1*q11 + 2*q11 - 2)/l1 - (3*(q21 + c + q11 - 1))/l2),
#          ratioq= q12/q22,
#          ratiol=l1/l2, 
#          p1= 1-q11-q21,
#          p2= 1- q12-q22,
#          profit11= q11*(p1-c),
#          profit21= q21*(p1-c),
#          profit12= l1*(p2-c)*q12 + (1-l1)*(p1-c)*q12 ,
#          profit22= l2*(p2-c)*q22 + (1-l2)*(p1-c)*q22,
#          type= "stackelberg"
#   )
# 
# dfcourn2 <- df %>% 
#   mutate(l2=l1,
#          q12= -((c - 1)*(2*l2*(l1 + 1) - l1))/(9*l2*l1), 
#          q22= -((c - 1)*(l2*(2*l1 - 1) + 2*l1))/(9*l2*l1), 
#          q11= (1 - c)/3, 
#          q21= (1 - c)/3,
#          ratioq= q12/q22,
#          ratiol=l1/l2, 
#          p1= 1-q11-q21,
#          p2= 1- q12-q22,
#          profit11= q11*(p1-c),
#          profit21= q21*(p1-c),
#          profit12= l1*(p2-c)*q12 + (1-l1)*(p1-c)*q12 ,
#          profit22= l2*(p2-c)*q22 + (1-l2)*(p1-c)*q22, 
#          type= "cournot"
#   )
# 
# 
# dfmon <- df %>% 
#   mutate(l2=l1,
#          q11=(1-c)/4,
#          q21=q11,
#          q12= (1+l1)*(1-c)/8*l1,
#          q22= q12,
#          ratioq= q12/q22,
#          ratiol=l1/l2,
#          p1= 1-q11*2,
#          p2=1-q12*2,
#          profit11= q11*(p1-c),
#          profit21= q21*(p1-c),
#          profit12= l1*(p2-c)*q12+(1-l1)*(p1-c)*q12,
#          profit22=profit12,
#          type="monopoly"
#          )
# 
# df2 <- rbind(dfst2, dfcourn2, dfmon)
# 
# rm(df, dfcourn, dfcourn2, dfst, dfst2, dfmon)
# 
# save(df1, df2, file = "Paper/game.RData")


##### LOADING THE DATA ########

setwd("C:/Users/ignac/OneDrive - Universidad Loyola Andalucía/Trabajo/Universidad/Máster/1º/Segundo semestre/Micro II/Paper/Graphs")

load("../game.Rdata")

### RATIOS of quantity

#ratios plot
#CAMAVINGA

plot1 <- 
  df1 %>%
  filter(type=="stackelberg") %>% 
  ggplot(aes(x=ratiol, y=ratioq))+
  geom_point(aes(color=c), alpha=.7)+
  scale_y_continuous(limits = c(-.5, 10))+
  scale_x_continuous(limits = c(0, 2))+
  geom_smooth(color="#b67182", se=F)+
  ylab("q12/q22")+
  xlab(expression(lambda[1]/lambda[2]))+
  scale_color_continuous(name="Cost", low = "#0e387a", high = "#9fafca")
  
# df1 %>%
#     filter(type=="stackelberg", round(c, digits = 2)%in% c(.3,.5,.8)) %>% 
#     ggplot(aes(x=ratiol, y=ratioq))+
#     geom_point(aes(color=as.factor(round(c, digits = 2))))+
#     scale_y_continuous(limits = c(-.5, 10))+
#     scale_x_continuous(limits = c(0, 2))+
#     geom_smooth(color="#b67182", se=F)+
#     ylab("q12/q22")+
#     xlab(expression(lambda[1]/lambda[2]))
#     scale_color_continuous(name="Cost", low = "#0e387a", high = "#9fafca")  

ggsave(plot=plot1, filename="Stackelberg/Plot1.png", type = "Cairo", height=4, width =5)


ggpricesl<- df2 %>% 
  mutate(dummyq2 = as.factor(ifelse(q12+q22-q11-q21>0,1,0))) %>% 
  filter(p1>0, p2>0, round(c, digits = 2) %in% c(.3,.5,.8), q11>0, q21>0, q22>0, q12>0) %>% 
  ggplot(aes(x=l1, y=p2/p1))+
  geom_point(aes(color=as.factor(round(c, digits = 2)),
                 shape=dummyq2
                 ), 
             alpha=.7
             )+
  geom_hline(yintercept = 1, color="grey50", size=.8, alpha=.7)+
  coord_cartesian(ylim = c(0,1.75))+
  scale_colour_manual(values= c("#9fafca", "#0e387a", "grey50"), name="Cost")+
  scale_shape_discrete(name="Total quantity", labels=c(expression(Q["t=1"]>Q["t=2"]), expression(Q["t=2"]>Q["t=1"])))+
  theme(legend.position = c(.8,.2), 
        legend.direction = "horizontal", 
        legend.background = element_rect(color = "white"))+
  guides(color= guide_legend(title.position = "top", title.hjust = .5), 
         shape= guide_legend(title.position = "top", title.hjust = .5))+
  xlab(expression(lambda))+
  ylab(expression(P[2]/P[1]))

ggsave(plot = ggpricesl, filename = "Stackelberg/ggrelprices.png", type="cairo", width = 6, height=4 )

#### FIXING LAMBDA ####

ggq1fixl<- 
  df2 %>% 
  filter(type=="stackelberg") %>% 
  ggplot(aes(x=l1, y=q12)) +
  geom_point(aes(color=c))+
  scale_y_continuous(limits = c(0,5))+
  geom_smooth(color="#b67182", se=F, method = "loess")+
  xlab(expression(lambda))+
  scale_color_continuous(name="Cost", low = "#0e387a", high = "#9fafca")

ggq2fixl <- df2 %>% 
  filter(type=="stackelberg") %>% 
  ggplot(aes(x=l1, y=q22)) +
  geom_point(aes(color=c))+
  scale_y_continuous(limits = c(0,5))+
  geom_smooth(color="#b67182", se=F, method = "loess")+
  xlab(expression(lambda))+
  scale_color_continuous(name="Cost", low = "#0e387a", high = "#9fafca")+
  guides(color="none")
  

ggsave(plot= plot_grid(ggq1fixl, ggq2fixl),
       filename = "Stackelberg/fix_q.png",
       type="cairo",
       width = 8 )

### FIXING 1 LAMBDA

#l1 on q1 and q2

grid1a<- 
  df1 %>%
  filter(type=="stackelberg") %>% 
  ggplot(aes(x=l1, y=q12))+
  geom_point(aes(color=c))+
  scale_y_continuous(limits = c(-5, 5))+
  scale_x_continuous(limits = c(0,1))+
  xlab(expression(lambda[1]))+
  scale_colour_gradient(low = "#0e387a", high = "#9fafca", name= "Cost")+
    guides(color= guide_colourbar(title.position="top", title.hjust = .5))+
  theme(legend.direction = "horizontal")

grid1b<-df1 %>%
  filter(type=="stackelberg") %>% 
  ggplot(aes(x=l1, y=q22))+
  geom_point(aes(color=c))+
  scale_y_continuous(limits = c(-5, 5))+
  scale_x_continuous(limits = c(0,1))+
  scale_colour_gradient(low = "#0e387a", high = "#9fafca", name= "Cost")+
  xlab(expression(lambda[1]))+
  theme(legend.position = "none") #depends on c until aroung l=.2


#lambda2 on q1 and q2

grid1c<-
  df1 %>%
  filter(type=="stackelberg") %>% 
  ggplot(aes(x=l2, y=q12))+
  geom_point(aes(color=c))+
  scale_y_continuous(limits = c(-10, 15))+
  scale_colour_gradient(low = "#0e387a", high = "#9fafca")+
  xlab(expression(lambda[2]))+
  theme(legend.position = "none")

grid1d <- df1 %>%
  filter(type=="stackelberg") %>% 
  ggplot(aes(x=l2, y=q22))+
  geom_point(aes(color=c))+
  scale_y_continuous(limits = c(-10, 15))+
  #scale_x_continuous(limits = c(0.1,.2))+
  scale_colour_gradient(low = "#0e387a", high = "#9fafca")+
  xlab(expression(lambda[2]))+
  theme(legend.position = "none")

ggsave(plot_grid(grid1a, grid1b, grid1c, grid1d), 
       filename="Stackelberg/Gridq1.png", type="Cairo", width = 8)

#NOTE: FACET THIS IN 4 

#ratiol on each q

# df1 %>%  #out but we can mention in text
#   filter(type=="stackelberg") %>% 
#   ggplot(aes(x=ratiol, y=q22))+
#   geom_point(aes(color=c))+
#   scale_y_continuous(limits = c(-3, 5))+
#   scale_x_continuous(limits = c(0,10))+
#   scale_colour_gradient(low = "#0e387a", high = "#9fafca")+
#   theme_minimal() #depends on c until aroung l=.2

#FACET THIS INTO 2

### PRICES

#when l1=l2

ggpricest<- 
  df2 %>%
  filter(type=="stackelberg", round(c, digits = 2) %in% c(.3, .5, .8)) %>%  
  ggplot(aes(x=l1, y=p2)) + 
  geom_point(aes(color= as.factor(round(c, digits = 2))), alpha=.8)+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(-10,1))+
  scale_colour_manual(values= c("#9fafca", "#0e387a", "grey50"), name="Cost")+
  xlab(expression(lambda))+
  theme(legend.position = c(.88,.25), 
        legend.background = element_rect(color="white"))+
  ylab("Price")
 
ggsave(plot=ggpricest, filename="Stackelberg/ggpricest.png")

varprice2<- df1 %>%
  filter(type=="stackelberg") %>%  
  ggplot(aes(x=ratiol, y=p2)) + 
  geom_point(aes(color= c), alpha=.8)+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(-10,1))+
  scale_colour_gradient(low = "#0e387a", high = "#9fafca", name ="Cost")+
  xlab(expression(lambda[1]/lambda[2]))+
  theme(legend.position = c(.88,.25), 
        legend.background = element_rect(color="white"))+
  ylab(expression(P[2]))
 
ggsave(plot=varprice2, filename="Stackelberg/varprice2.png", type="cairo", width = 5.5, height = 4.5)


#PROFITS

#When l1=l2 profits of each

ggprofst1<- 
df2 %>%
  filter(type=="stackelberg", round(c, digits = 2) %in% c(.3,.5,.8)) %>%  
  ggplot(aes(x=l1, y=profit12)) +
  geom_point(aes(color=as.factor(round(c, digits = 2))), alpha=.8)+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,.2))+
  ylab("Firm 1 profits")+
  xlab(expression(lambda))+
  scale_colour_manual(values= c("#9fafca", "#0e387a", "grey50"), name="Cost")
  
  

ggprofst2<-
  df2 %>%
  filter(type=="stackelberg", round(c, digits = 2) %in% c(.3,.5,.8) ) %>%  
  ggplot(aes(x=l1, y=profit22)) +
  geom_point(aes(color=as.factor(round(c, digits = 2))))+
  scale_x_continuous(limits = c(0,1))+
  scale_y_continuous(limits = c(0,.2))+
  scale_colour_manual(values= c("#9fafca", "#0e387a", "grey50"), name="Cost")+
  theme(legend.position = "none")+
  ylab("Firm 2 profits")+
  xlab(expression(lambda))
  

ggsave(plot_grid(ggprofst1, ggprofst2, nrow = 2), 
       filename="Stackelberg/gridprof.png", type="Cairo", width = 6, height = 5)


#firm 2 is the same but the level is lower, profits behave equally with c but higher c less profit

#Only at lower levels of lambda the firm can. Once lambda is high, we're back to fixed prices so there's not really a point

#When l1!=l2

#plotprofratiost<- 
  df1 %>%
  filter(type=="stackelberg") %>% 
  ggplot(aes(x=ratiol, y=profit12/profit22)) +
  geom_point(aes(color=c))+
  scale_x_continuous(limits = c(0,2))+
  scale_y_continuous(limits = c(0,40))+
  geom_smooth(se=F, color="#d0a5ae", method = "loess")+
  ylab(expression(pi["1,2"]/ pi["2,2"]))+
  xlab(expression(lambda[1]/lambda[2]))+
  scale_colour_gradient(low = "#0e387a", 
                        high = "#9fafca")+
  scale_colour_gradient(low = "#0e387a", high = "#9fafca", name="Cost")+
  theme(legend.background = element_rect(color = "white")) 

 
  
  
ggsave(plotprofratiost, filename="Stackelberg/plotprofratiost.png",type= "cairo")

#For lower values of the lambda ratio, i.e. l2>l1, firm 2 profits are higher because it has more "market power" (Like Yann said)
#But, as l1 starts rising, firm 1 is able to take advantage and get higher profits, but this is limited (as we saw in the graph for profit of firm 1) and the ´"advantage" of sticky prices reduces and so the ratio/gap between profits of firms 1 and 2 also goes down.



##################### COMPARE 3 GAMES #########################


##COMPARE EVOLUTION OF QUANTITIES ###



#For firm 1
compareq1<-   df2 %>% 
  filter(round(c, digits = 2) %in% c(.3,.5)) %>%  
  ggplot(aes(x=l1, 
             y=q12, 
             color=type, 
             alpha= as.factor(round(c, digits = 2))
             )
         )+
  geom_line(aes(linetype=type), size=1)+
  coord_cartesian(ylim=c(0, 4))+
  scale_color_manual(values = c("#4C837A", "#B80D48", "#04253A"), name="Game")+
  scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
  xlab(expression(lambda))+
  guides(linetype="none", alpha="none")+
  theme(legend.background = element_rect(color = "white"))

#For firm 2

compareq2<- 
  df2 %>% 
  filter(round(c, digits = 2) %in% c(.3,.5)) %>%  
  ggplot(aes(x=l1,
             y=q22, 
             alpha=as.factor(round(c, digits = 2))
             )
         )+
  geom_line(aes(color=type, linetype=type), size=1)+
  coord_cartesian(ylim=c(0, 4))+
  scale_color_manual(values = c("#4C837A", "#B80D48", "#04253A"))+
  scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
  xlab(expression(lambda))+
  guides(linetype="none", color="none", alpha="none")+
  theme(legend.background = element_rect(color = "white"))

ggsave(plot=plot_grid(compareq1, compareq2),
       filename = "Comparison/compareQ.png", type= "cairo", width= 8
       )

#Total quantity

ggcomparetq<- df2 %>% 
  filter(round(c, digits = 2) %in% c(.5)) %>%  
  ggplot(aes(x=l1, y=q12+q22))+
  geom_line(aes(color=type, linetype=type), size=1, alpha=.7)+
  coord_cartesian(ylim=c(0, 2))+
  scale_color_manual(values = c("#4C837A", "#B80D48", "#04253A"), name= "Game")+
  scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
  xlab(expression(lambda))+
  ylab("Total Q in t = 2")+
  guides(linetype="none")+
  theme(legend.background = element_rect(color = "white"))

ggsave(plot=ggcomparetq, filename = "Comparison/comparetq.png", type="cairo", width=6)

###COMPARE EVOLUTION OF PRICES ###

#For t=1


ggprice1c <-
  df2 %>% 
    group_by(type) %>% 
    mutate(meanp1= mean(p1)) %>% 
    ungroup() %>% 
    ggplot(aes(x=type, y=p1, fill= type)) +
  geom_boxplot()+
  geom_point(aes(y=meanp1), color="#E1DDBF", size=3)+
  coord_cartesian(ylim=c(0, 1.25))+
  scale_fill_manual(values = c("#4C837A", "#B80D48", "#04253A"), name= "Game")+
  xlab("Game")+
  ylab("Price in t=1")+
  theme(legend.background = element_rect(color = "white"), 
        legend.direction = "horizontal",
        legend.position = c(.7,.85))+
  guides(fill= "none")

  
  
ggsave(plot=ggprice1c,
       filename = "Comparison/ggprice1c.png", 
       type="cairo", width = 6)

#For t=2

# ggprice2c <- df2 %>% 
#   ggplot(aes(x=type, y=p2, fill= type)) +
#   geom_boxplot()+
#   geom_point(aes(y=mean(p1)), color="#E1DDBF", size=3)+
#   coord_cartesian(ylim=c(0, 1.5))+
#   scale_fill_manual(values = c("#4C837A", "#B80D48", "#04253A"), name= "Game")+
#   xlab("Game")+
#   ylab("Price in t=2")+
#   guides(fill="none")
# 
# ggsave(plot=plot_grid(ggprice1c, ggprice2c),
#        filename = "Comparison/ggpricec.png", 
#        type="cairo", width = 7)

ggprice2c <- 
  df2 %>% 
  filter(round(c, digits = 2) %in% c(.5, .3)) %>%  
  ggplot(aes(x=l1, y=p2))+
  geom_line(aes(color=type, 
                linetype= type, 
                alpha=as.factor(round(c, digits = 2))), 
            size=.8)+
  coord_cartesian(ylim=c(-2, 1))+
  geom_hline(yintercept = 0, color="black")+
  scale_color_manual(values = c("#4C837A", "#B80D48", "#04253A"), name= "Game")+
  scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
  xlab(expression(lambda))+
  ylab("Price in t=2")+
  guides(linetype="none", 
         alpha="none")+
  theme(legend.position = c(.85,.2))

ggsave(plot=ggprice2c,
       filename = "Comparison/ggprice2c.png", 
       type="cairo",width = 7)

##this one is better with a line and fixing the cost to see the threshold. Different cs so it is shown that it doesn't affect the threshold of lambda


## COMPARE EVOLUTION OF PROFITS ##

#For firm 1

# df2 %>% #This is in time period 1 so it doesn't make sense
#   filter(round(c, digits = 2) %in% c(.5, .3)) %>%  
#   ggplot(aes(x=l1, y=profit11))+
#   geom_line(aes(color=type, 
#                 linetype= type, 
#                 alpha=as.factor(round(c, digits = 2))), 
#             size=1)+
#   coord_cartesian(ylim=c(0, .15))+
#   scale_color_manual(values = c("#4C837A", "#B80D48", "#04253A"), name= "Game")+
#   scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
#   xlab(expression(lambda))+
#   ylab(expression(pi["1,2"]))+
#   guides(linetype="none", alpha="none")


ggprof12<-df2 %>% 
  filter(as.factor(round(c, digits = 2)) %in% c(.5, .3)) %>%  
  ggplot(aes(x=l1, y=profit12))+
  geom_line(aes(color=type, alpha=as.factor(round(c, digits = 2)), linetype=type), size=1)+
  coord_cartesian(ylim=c(0, .15))+
  scale_color_manual(values = c("#4C837A", "#B80D48", "#04253A"), name= "Game")+
  scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
  xlab(expression(lambda))+
  ylab(expression(pi["1,2"]))+
  guides(linetype="none", alpha="none")
#Again c doesn't affect the level of lambda but just scales the profit.



#For firm 2
  
ggprof22<- 
  df2 %>% 
    filter(round(c, digits = 2) %in% c(.5, .3)) %>%  
    ggplot(aes(x=l1, y=profit22))+
    geom_line(aes(color=type, 
                  alpha= as.factor(round(c, digits = 2)), 
                  linetype=type
                    ), 
              size=1)+
    coord_cartesian(ylim=c(0, .2))+
    coord_cartesian(ylim=c(0, .15))+
    scale_color_manual(values = c("#4C837A", "#B80D48", "#04253A"), name= "Game")+
    scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
    xlab(expression(lambda))+
    ylab(expression(pi["2,2"]))+
    guides(linetype="none", alpha="none", color="none")

ggsave(plot=plot_grid(ggprof12, ggprof22), 
       filename = "Comparison/ggproft2.png",
       width = 8,
       type="cairo")



##COMPARATION OF TOTAL PROFITS FOR EACH FIRM #####

ggprof1<-
  df2 %>% 
  filter(round(c, digits = 2) %in% c(.5, .3)) %>%  
  ggplot(aes(x=l1, y=profit11+profit12))+
  geom_line(aes(color=type, 
                alpha= as.factor(round(c, digits = 2)), 
                linetype=type
  ), 
  size=1)+
  coord_cartesian(ylim=c(0, .2))+
  coord_cartesian(ylim=c(0, .15))+
  scale_color_manual(values = c("#4C837A", "#B80D48", "#04253A"), name= "Game")+
  scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
  xlab(expression(lambda))+
  ylab(expression(pi[1]))+
  guides(linetype="none", alpha="none", color="none")


ggprof2<- 
  df2 %>% 
  filter(round(c, digits = 2) %in% c(.5, .3)) %>%  
  ggplot(aes(x=l1, y=profit21+profit22))+
  geom_line(aes(color=type, 
                alpha= as.factor(round(c, digits = 2)), 
                linetype=type
  ), 
  size=1)+
  coord_cartesian(ylim=c(0, .2))+
  coord_cartesian(ylim=c(0, .15))+
  scale_color_manual(values = c("#4C837A", "#B80D48", "#04253A"), name= "Game")+
  scale_linetype_manual(values=c("dashed", "dotted", "solid"))+
  xlab(expression(lambda))+
  ylab(expression(pi[2]))+
  guides(linetype="none", alpha="none", color="none")

  
  
ggsave(plot=plot_grid(ggprof1, ggprof2), 
       filename = "Comparison/ggtotprof.png",
       width = 8,
       type="cairo")


######### MONOPOLY ######
##Q

monq1<-
  df2 %>% 
  filter(type=="monopoly") %>% 
  ggplot(aes(x=l1, y=q12, color=c))+
  geom_point()+
  geom_smooth(se=F, color="#b67182", method = "loess")+
  xlab(expression(lambda))+
  scale_colour_gradient(low = "#E1DDBF", high = "#990011FF", name="Cost")+
  theme(legend.background = element_rect(color = "white"))+
  guides(color="none")

monq2<-
  df2 %>% 
  filter(type=="monopoly") %>% 
  ggplot(aes(x=l1, y=q22, color=c))+
  geom_point()+
  geom_smooth(se=F, color="#b67182", method = "loess")+
  xlab(expression(lambda))+
  scale_colour_gradient(low = "#E1DDBF", high = "#990011FF", name="Cost")+
  theme(legend.background = element_rect(color = "white"))+
  guides(color="none")


#P

idk <- ggplot()
monp2<- df2 %>% 
  filter(type=="monopoly") %>% 
  ggplot(aes(x=l1, y=p2, color=c))+
  geom_point()+
  geom_smooth(se=F, color="#b67182", method = "loess")+
  xlab(expression(lambda))+
  scale_colour_gradient(low = "#E1DDBF", high = "#990011FF", name="Cost")+
  theme(legend.background = element_rect(color = "white"))+
  guides(color="none")

#Profits

#monprof1<-
df2 %>% 
  filter(type=="monopoly") %>% 
  ggplot(aes(x=l1, y=profit12, color=c))+
  geom_point()+
  geom_smooth(se=F, color="#b67182", method = "loess")+
  xlab(expression(lambda))+
  ylab(expression(pi["1,2"]))+
  scale_colour_gradient(low = "#E1DDBF", high = "#990011FF", name="Cost")+
  theme(legend.background = element_rect(color = "white"))+
  guides(color="none")


#monprof2<-
  df2 %>% 
  filter(type=="monopoly") %>% 
  ggplot(aes(x=l1, y=profit22, color=c))+
  geom_point()+
    geom_smooth(se=F, color="#b67182", method = "loess")+
    xlab(expression(lambda))+
    ylab(expression(pi["1,2"]))+
  scale_colour_gradient(low = "#E1DDBF", high = "#990011FF", name="Cost")+
  theme(legend.background = element_rect(color = "white"))+
    guides(color="none")


ggsave(plot=plot_grid(monq1, monq2, monp2, idk, monprof1, monprof2,
          nrow = 3, 
          ncol = 2),
       filename = "Monopoly/monopoly.png", 
       height = 15, 
       width = 9)

ggsave(plot=monq1,
       filename = "Monopoly/monopolyleg.png", 
       height = 15, 
       width = 9)

###### COURNOT ########


##Q

cournq1<-
  df2 %>% 
  filter(type=="cournot") %>% 
  ggplot(aes(x=l1, y=q12, color=c))+
  geom_point()+
  geom_smooth(se=F, color="#7DB46CFF", method = "loess")+
  scale_y_continuous(limits = c(0,5))+
  xlab(expression(lambda))+
  scale_colour_gradient(low = "#E1DDBF", high = "#4C837A", name="Cost")+
  theme(legend.background = element_rect(color = "white"))+
  guides(color="none")

cournq2<-
df1 %>% 
  filter(type=="cournot") %>% 
  ggplot(aes(x=ratiol, y=ratioq, color=c))+
  geom_point()+
  geom_smooth(se=F, color="#7DB46CFF", method = "loess")+
  scale_y_continuous(limits = c(0,5))+
  scale_x_continuous(limits = c(0.5,2))+
  xlab(expression(lambda[1]/lambda[2]))+
  ylab(expression(q["1,2"]/q["2,2"]))+
  scale_colour_gradient(low = "#E1DDBF", high = "#4C837A", name="Cost")+
  theme(legend.background = element_rect(color = "white"))+
  guides(color="none")


#P

cournp2<-
  df2 %>% 
  filter(type=="cournot") %>% 
  ggplot(aes(x=l1, y=p2, color=c))+
  geom_point()+
  geom_smooth(se=F, color="#7DB46CFF", method = "loess")+
  scale_y_continuous(limits = c(-2,1.5))+
  xlab(expression(lambda))+
  scale_colour_gradient(low = "#E1DDBF", high = "#4C837A", name="Cost")+
  theme(legend.background = element_rect(color = "white"))+
  guides(color="none")

#Profits

cournprof1<-
df2 %>% 
  filter(type=="cournot") %>% 
  ggplot(aes(x=l1, y=profit12, color=c))+
  geom_point()+
  geom_smooth(se=F, color="#7DB46CFF", method = "loess")+
  scale_y_continuous(limits = c(0,.5))+
  xlab(expression(lambda))+
  ylab(expression(pi["1,2"]))+
  scale_colour_gradient(low = "#E1DDBF", high = "#4C837A", name="Cost")+
  theme(legend.background = element_rect(color = "white"))+
  guides(color="none")


cournprof2<-
df1 %>% 
  filter(type=="cournot") %>% 
  ggplot(aes(x=ratiol, y=profit12/profit22, color=c))+
  geom_point()+
  geom_smooth(se=F, color="#7DB46CFF", method = "loess")+
    scale_x_continuous(limits = c(0,2))+
  scale_y_continuous(limits = c(0,15))+
  xlab(expression(lambda[1]/lambda[2]))+
  ylab(expression(pi["1,2"]/pi["2,2"]))+
  scale_colour_gradient(low = "#E1DDBF", high = "#4C837A", name="Cost")+
  theme(legend.background = element_rect(color = "white"))+
  guides(color="none")

ggsave(plot=plot_grid(cournq1, cournq2, cournp2, ggplot(), cournprof1, cournprof2,
                      nrow = 3, 
                      ncol = 2),
       filename = "Cournot/Cournot1.png", 
       height = 15, 
       width = 9)

 ggsave(plot=cournq1,
        filename = "Cournot/Cournotleg.png", height = 10, width=10)
