### heatmap plot
library(corrplot)
library(ggplot2)
library(ggpubr)
library(haven)
correlation_prenatal_o3 <- read_sav("D:/ME/BNU/ABCD/environment-culture-brain/correlation_prenatal o3.sav")
View(correlation_prenatal_o3)
class(correlation_prenatal_o3)
cor_O3 <- cor(correlation_prenatal_o3, 
              use = "complete.obs",
              method="pearson")
cor_O3

library(showtext)

font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")

showtext_auto()

theme_set(theme_minimal(base_family = "Arial"))

library(Cairo)
Cairo.capabilities() 


Cairo::CairoPNG( 
  filename = "o3_cor.png", 
  width = 7,           
  height = 7,         
  units = "in",        
  dpi = 300)           


mycol <- colorRampPalette(c("#3A4B6E","white", "#BA3E45"), alpha = TRUE)


p <- corrplot(cor_O3, method = c('square'), 
              type = c('lower'), 
              col = mycol(100),
              outline = FALSE,
              order = c('AOE'),
              diag = TRUE,
              tl.cex = 2, 
              tl.col = 'black',
              tl.srt = 90,
              cl.cex = 2,
              cl.ratio = 0.10,
              addCoef.col = 'black', 
              number.cex = 2, 
)

dev.off() 

### o3 map 
library(data.table)
library(dplyr)
library(sf)
library(ggplot2)
library(maps)
library(mapdata)
library(ggmap)
library(ggspatial)
library(mapproj)
library(haven)
o3_site <- read_sav("D:/ME/BNU/ABCD/environment-culture-brain/o3_site.sav")
View(o3_site)

o3_site <- replace(o3_site, o3_site  ==  999.000, NA);o3_site
summary(o3_site)


newdata1<-o3_site[complete.cases(o3_site),]
newdata1<-na.omit(o3_site)
summary(newdata1)
sum(is.na(newdata1))

library(showtext)

font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")

showtext_auto()

theme_set(theme_minimal(base_family = "Arial"))

usa_states <- map_data("state") 
print(usa_states)

p <- ggplot()+
  geom_polygon(data = usa_states, aes(long, lat, group = group, fill = region), color = "black")+
  coord_quickmap()
print(p)


ozone_sf <- st_as_sf(newdata1, coords = c("long", "lat"), crs = 4326)
p <- ggplot() +
  geom_sf(data = usa, fill = "white", color = "darkgray") +
  geom_sf(data = ozone_sf, aes(color = o3), size = 1) +
  scale_color_viridis_c(option = "C", name = "O3 (ppb)") +
  theme_minimal() + 
  theme(
    axis.text.x = element_text(size = 16, color = "black"), 
    axis.text.y = element_text(size = 16, color = "black"),   
    legend.title = element_text(size = 16, face = "bold"), 
    legend.text = element_text(size = 12),  
    panel.grid.major = element_line(color = "grey", linetype = "dashed", size = 0.2), 
    panel.grid.minor = element_line(color = "lightgrey", linetype = "dotted", size = 0.25),  
  )+
  guides(color = guide_colourbar(barwidth = 0.5, barheight = 2.5))

print(p)



ggsave(
  filename = "o3_map.png", 
  device = "png",  
  path = "D:/ME/BNU/ABCD/environment-culture-brain/plot",
  width = 80,             
  height = 80,            
  units = "mm",          
  bg = "white",            
  dpi = 300             
)


##ridgeline plot

library(ggridges)
library(ggplot2)
library(cols4all)
library(haven)
o3_site <- read_sav("D:/ME/BNU/ABCD/environment-culture-brain/o3_site.sav")
head(o3_site)


o3_site <- replace(o3_site, o3_site  ==  999.000, NA);o3_site
summary(o3_site)


newdata1<-o3_site[complete.cases(o3_site),]
newdata1<-na.omit(o3_site)
summary(newdata1)
sum(is.na(newdata1))

library(showtext)


font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")

showtext_auto()

theme_set(theme_minimal(base_family = "Arial"))


p1 <- ggplot(data = newdata1,
             aes(x = o3, y = site, fill = site),
) +
  geom_density_ridges() +
  theme_classic()+
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 28, face = "bold"),  
    axis.title.y = element_text(size = 28, face = "bold"),  
    axis.text.x = element_text(size = 24, color = "black"), 
    axis.text.y = element_text(size = 24, color = "black")  
  ) +
  labs(
    x = "O3(ppb)", 
    y = "ABCD Study Site"                     
  )

p1


ggsave(
  filename = "o3_ridgeline.png", 
  device = "png",  
  path = "D:/ME/BNU/ABCD/environment-culture-brain/plot",
  width = 100,             
  height = 100,            
  units = "mm",          
  bg = "white",            
  dpi = 300              
)

###moderation model：O3&school→L-hippo effect size（95%CI）
df1 <- data.frame(
  variable = c("O3→L-hippo", "School→L-hippo", "O3*School→L-hippo"),
  correlation = c(-0.009, -0.003,0.007),
  lower = c(-0.015, -0.012, 0.001),
  upper = c(-0.003, 0.005, 0.013)
)


df1$variable <- factor(df1$variable, levels = df1$variable)
library(ggplot2)


library(showtext)


font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")


showtext_auto()

theme_set(theme_minimal(base_family = "Arial"))


library(ggplot2)

ggplot(df2, aes(x = variable, y = correlation)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, size = 1.0, color = "#CCF2F3") +  
  geom_point(size = 1.5, shape = 21, fill = "#3FA4A7",color = "black", stroke = 0.3) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", size = 0.3) +  
  coord_flip() +  
  labs(y = "Standardized effect size (95% CI)", x = "") +  
  scale_y_continuous(limits = c(-0.030, 0.030),  
                     breaks = seq(-0.030, 0.030, by = 0.01),  
                     labels = scales::number_format(accuracy = 0.001)) +  
  theme_minimal() +  
  theme(
    axis.text.x = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title = element_text(size = 22, face = "bold"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.background = element_rect(fill = "white", color = NA), 
    axis.ticks.x = element_line(color = "black", size = 0.2),  
    axis.ticks.y = element_line(color = "black", size = 0.2), 
    axis.line.x = element_line(color = "black", size = 0.2),   
    axis.line.y = element_line(color = "black", size = 0.2)    
  )


ggsave(
  filename = "o3_effect size_school.png",
  device = "png", 
  path = "D:/ME/BNU/ABCD/environment-culture-brain/plot",
  width = 100,             
  height = 50,            
  units = "mm",          
  bg = "white",           
  dpi = 300              
)

###moderation model：O3&neighbor→L-hippo  effect size（95%CI）
df2 <- data.frame(
  variable = c("O3→R-amyg","Neighbor→R-amyg", "O3*Neighbor→R-amyg"),
  correlation = c(0.004,-0.001,-0.008),
  lower = c(-0.007,-0.012,-0.016),
  upper = c(0.016,0.012,-0.001)
)


df2$variable <- factor(df2$variable, levels = df2$variable)
library(ggplot2)


library(showtext)


font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")


showtext_auto()

theme_set(theme_minimal(base_family = "Arial"))


library(ggplot2)


ggplot(df2, aes(x = variable, y = correlation)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, size = 1.0, color = "#CCF2F3") +
  geom_point(size = 1.5, shape = 21, fill = "#3FA4A7",color = "black", stroke = 0.3) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", size = 0.3) +  
  coord_flip() + 
  labs(y = "Standardized effect size (95% CI)", x = "") +  
  scale_y_continuous(limits = c(-0.030, 0.030),  
                     breaks = seq(-0.030, 0.030, by = 0.01),  
                     labels = scales::number_format(accuracy = 0.001)) +  
  theme_minimal() + 
  theme(
    axis.text.x = element_text(size = 22, color = "black"),
    axis.text.y = element_text(size = 22, color = "black"),
    axis.title = element_text(size = 22, face = "bold"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.background = element_rect(fill = "white", color = NA),  
    axis.ticks.x = element_line(color = "black", size = 0.2),  
    axis.ticks.y = element_line(color = "black", size = 0.2),  
    axis.line.x = element_line(color = "black", size = 0.2),   
    axis.line.y = element_line(color = "black", size = 0.2)    
  )


ggsave(
  filename = "o3_effect size_neighbor.png", 
  device = "png",  
  path = "D:/ME/BNU/ABCD/environment-culture-brain/plot",
  width = 100,             
  height = 50,            
  units = "mm",          
  bg = "white",           
  dpi = 300              
)

###MI-based data
###moderated mediation model1：O3&school→L-hippo→cognition  effect size（95%CI）
df3 <- data.frame(
  variable = c("O3*School→L-hippo→cognition(low)", "O3*School→L-hippo→cognition(high)","O3*School→L-hippo→cognition(moderated mediation)"),
  correlation = c(-0.0009,-0.0002,-0.0011),
  lower = c(-0.0018,-0.0003,-0.0021),
  upper = c(-0.0001,0.0007,-0.0002)
)


df3$variable <- factor(df3$variable, levels = df3$variable)
library(ggplot2)


library(showtext)


font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")


showtext_auto()

theme_set(theme_minimal(base_family = "Arial"))


library(ggplot2)


ggplot(df3, aes(x = variable, y = correlation)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, size = 2.5, color = "#CCF2F3") + 
  geom_point(size = 3.5, shape = 21, fill = "#3FA4A7",color = "black", stroke = 0.3) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", size = 0.3) +  
  coord_flip() +  
  labs(y = "Standardized effect size (95% CI)", x = "") +  
  scale_y_continuous(limits = c(-0.0025, 0.0025),  
                     breaks = seq(-0.0015, 0.0015, by = 0.0005), 
                     labels = scales::number_format(accuracy = 0.0001)) +  
  theme_minimal() +  
  theme(
    axis.text.x = element_text(size = 36, color = "black"),
    axis.text.y = element_text(size = 36, color = "black"),
    axis.title = element_text(size = 36, face = "bold"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.background = element_rect(fill = "white", color = NA),  
    axis.ticks.x = element_line(color = "black", size = 0.2),  
    axis.ticks.y = element_line(color = "black", size = 0.2),  
    axis.line.x = element_line(color = "black", size = 0.2),   
    axis.line.y = element_line(color = "black", size = 0.2)    
  )


ggsave(
  filename = "o3_moderated mediation_school1.png", 
  device = "png",  
  path = "D:/ME/BNU/ABCD/environment-culture-brain/plot",
  width = 300,             
  height = 100,            
  units = "mm",          
  bg = "white",           
  dpi = 300              
)


###moderated mediation model2：O3&school→L-hippo→PLEs  effect size（95%CI）
df4 <- data.frame(
  variable = c("O3*School→L-hippo→PLEs(low)", "O3*School→L-hippo→PLEs(high)","O3*School→L-hippo→PLEs(moderated mediation)"),
  correlation = c(0.0010,-0.0002,0.0012),
  lower = c(0.0001,-0.0006,0.0002),
  upper = c(0.0018,0.0003,0.0022)
)


df4$variable <- factor(df4$variable, levels = df4$variable)
library(ggplot2)


library(showtext)


font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")


showtext_auto()

theme_set(theme_minimal(base_family = "Arial"))


library(ggplot2)


ggplot(df4, aes(x = variable, y = correlation)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, size = 2.5, color = "#CCF2F3") + 
  geom_point(size = 3.5, shape = 21, fill = "#3FA4A7",color = "black", stroke = 0.3) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", size = 0.3) +  
  coord_flip() +  
  labs(y = "Standardized effect size (95% CI)", x = "") +  
  scale_y_continuous(limits = c(-0.0025, 0.0025),  
                     breaks = seq(-0.0015, 0.0015, by = 0.0005),  
                     labels = scales::number_format(accuracy = 0.0001)) +  
  theme_minimal() +  
  theme(
    axis.text.x = element_text(size = 36, color = "black"),
    axis.text.y = element_text(size = 36, color = "black"),
    axis.title = element_text(size = 36, face = "bold"),
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    panel.background = element_rect(fill = "white", color = NA), 
    axis.ticks.x = element_line(color = "black", size = 0.2),  
    axis.ticks.y = element_line(color = "black", size = 0.2),  
    axis.line.x = element_line(color = "black", size = 0.2),   
    axis.line.y = element_line(color = "black", size = 0.2)    
  )


ggsave(
  filename = "o3_moderated mediation_school2.png", 
  device = "png", 
  path = "D:/ME/BNU/ABCD/environment-culture-brain/plot",
  width = 300,             
  height = 100,            
  units = "mm",          
  bg = "white",            
  dpi = 300              
)

