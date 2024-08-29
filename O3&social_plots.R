###Figure 2. Distribution of prenatal ozone estimates by ABCD sites.
### Figure 2a 
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
o3_site <- read_sav("D:/ME/ABCD/environment-culture-brain/o3_site.sav")
View(o3_site)

o3_site <- replace(o3_site, o3_site  ==  999.000, NA);o3_site
summary(o3_site)


newdata1<-o3_site[complete.cases(o3_site),]
newdata1<-na.omit(o3_site)
summary(newdata1)
sum(is.na(newdata1))

usa <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE))
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
  scale_color_viridis_c(option = "C", name = expression(bold(O[3] ~ "(ppb)"))) + 
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
  path = "D:/ME/ABCD/environment-culture-brain/plot",
  width = 80,             
  height = 80,            
  units = "mm",          
  bg = "white",            
  dpi = 300             
)


##Figure 2b 

library(ggridges)
library(ggplot2)
library(cols4all)
library(haven)
o3_site <- read_sav("D:/ME/ABCD/environment-culture-brain/o3_site.sav")
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


p1 <- ggplot(data = newdata1, aes(x = o3, y = site, fill = site)) +
  geom_density_ridges() +
  theme_classic() +
  theme(
    legend.position = 'none',
    axis.title.x = element_text(size = 28, face = "bold"),
    axis.title.y = element_text(size = 28, face = "bold"),
    axis.text.x = element_text(size = 24, color = "black"),
    axis.text.y = element_text(size = 24, color = "black")
  ) +
  labs(
    x = expression(bold(O[3] * " (ppb)")),  
    y = "ABCD Study Site"
  )


p1



ggsave(
  filename = "o3_ridgeline.png", 
  device = "png",  
  path = "D:/ME/ABCD/environment-culture-brain/plot",
  width = 100,             
  height = 100,            
  units = "mm",          
  bg = "white",            
  dpi = 300              
)
###Figure 3. Prenatal ozone exposure, school/neighborhood environment and limbic system
### Figure 3b 

library(ggplot2)
library(showtext)


font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")
showtext_auto()


theme_set(theme_minimal(base_family = "Arial"))


df1 <- data.frame(
  variable = c("O3→L-hippo", "School→L-hippo", "O3*School→L-hippo"),
  correlation = c(-0.009, -0.003, 0.007),
  lower = c(-0.015, -0.012, 0.001),
  upper = c(-0.003, 0.005, 0.013)
)


df1$variable <- factor(df1$variable, levels = df1$variable)


ggplot(df1, aes(x = variable, y = correlation)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, size = 1.0, color = "#CCF2F3") +  
  geom_point(size = 1.5, shape = 21, fill = "#3FA4A7", color = "black", stroke = 0.3) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", size = 0.3) +  
  coord_flip() +  
  labs(y = "Standardized effect size (95% CI)", x = "") +  
  scale_y_continuous(limits = c(-0.030, 0.030),  
                     breaks = seq(-0.030, 0.030, by = 0.01),  
                     labels = scales::number_format(accuracy = 0.001)) +  
  scale_x_discrete(labels = c(
    "O3→L-hippo" = expression(paste("O", phantom()[3], "→L-hippo")),
    "School→L-hippo" = "School→L-hippo",
    "O3*School→L-hippo" = expression(paste("O", phantom()[3], "*School→L-hippo"))
  )) +
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
  path = "D:/ME/ABCD/environment-culture-brain/plot",
  width = 100,             
  height = 50,            
  units = "mm",          
  bg = "white",           
  dpi = 300              
)

### Figure 3d

library(ggplot2)
library(showtext)


font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")
showtext_auto()


theme_set(theme_minimal(base_family = "Arial"))


df2 <- data.frame(
  variable = c("O3→R-amyg", "Neighbor→R-amyg", "O3*Neighbor→R-amyg"),
  correlation = c(0.004, -0.001, -0.008),
  lower = c(-0.007, -0.012, -0.016),
  upper = c(0.016, 0.012, -0.001)
)


df2$variable <- factor(df2$variable, levels = df2$variable)


ggplot(df2, aes(x = variable, y = correlation)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, size = 1.0, color = "#CCF2F3") +
  geom_point(size = 1.5, shape = 21, fill = "#3FA4A7", color = "black", stroke = 0.3) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", size = 0.3) +  
  coord_flip() + 
  labs(y = "Standardized effect size (95% CI)", x = "") +  
  scale_y_continuous(limits = c(-0.030, 0.030),  
                     breaks = seq(-0.030, 0.030, by = 0.01),  
                     labels = scales::number_format(accuracy = 0.001)) +  
  scale_x_discrete(labels = c(
    "O3→R-amyg" = expression(paste("O", phantom()[3], "→R-amyg")),
    "Neighbor→R-amyg" = "Neighbor→R-amyg",
    "O3*Neighbor→R-amyg" = expression(paste("O", phantom()[3], "*Neighbor→R-amyg"))
  )) +
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
  path = "D:/ME/ABCD/environment-culture-brain/plot",
  width = 100,             
  height = 50,            
  units = "mm",          
  bg = "white",           
  dpi = 300              
)

### Figure 4. Prenatal ozone exposure & school environment, limbic system,  cognition and psychotic-like experiences
###Figure 4b
library(ggplot2)
library(showtext)
df3 <- data.frame(
  variable = c("O3*School→L-hippo→cognition(low)", "O3*School→L-hippo→cognition(high)","O3*School→L-hippo→cognition(moderated mediation)"),
  correlation = c(-0.0009,-0.0002,-0.0011),
  lower = c(-0.0018,-0.0003,-0.0021),
  upper = c(-0.0001,0.0007,-0.0002)
)


df3$variable <- factor(df3$variable, levels = df3$variable)


font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")


showtext_auto()

theme_set(theme_minimal(base_family = "Arial"))


library(ggplot2)


ggplot(df3, aes(x = variable, y = correlation)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, size = 2.5, color = "#CCF2F3") + 
  geom_point(size = 3.5, shape = 21, fill = "#3FA4A7", color = "black", stroke = 0.3) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", size = 0.3) +  
  coord_flip() +  
  labs(y = "Standardized effect size (95% CI)", x = "") +  
  scale_y_continuous(limits = c(-0.0025, 0.0025),  
                     breaks = seq(-0.0015, 0.0015, by = 0.0005),  
                     labels = scales::number_format(accuracy = 0.0001)) +  
  scale_x_discrete(labels = c(
    "O3*School→L-hippo→cognition(low)" = expression(paste("O", phantom()[3], "*School→L-hippo→cognition(low)")),
    "O3*School→L-hippo→cognition(high)" = expression(paste("O", phantom()[3], "*School→L-hippo→cognition(high)")),
    "O3*School→L-hippo→cognition(moderated mediation)" = expression(paste("O", phantom()[3], "*School→L-hippo→cognition(moderated mediation)"))
  )) +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(size = 42, color = "black"),
    axis.text.y = element_text(size = 42, color = "black"),
    axis.title = element_text(size = 44, face = "bold"),
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
  path = "D:/ME/ABCD/environment-culture-brain/plot",
  width = 300,             
  height = 100,            
  units = "mm",          
  bg = "white",           
  dpi = 300              
)


###Figure 4d
library(ggplot2)
library(showtext)


font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")
showtext_auto()


theme_set(theme_minimal(base_family = "Arial"))


df4 <- data.frame(
  variable = c("O3*School→L-hippo→PLEs(low)", "O3*School→L-hippo→PLEs(high)",
               "O3*School→L-hippo→PLEs(moderated mediation)"),
  correlation = c(0.0010, -0.0002, 0.0012),
  lower = c(0.0001, -0.0006, 0.0002),
  upper = c(0.0018, 0.0003, 0.0022)
)


df4$variable <- factor(df4$variable, levels = df4$variable)


ggplot(df4, aes(x = variable, y = correlation)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0, size = 2.5, color = "#CCF2F3") + 
  geom_point(size = 3.5, shape = 21, fill = "#3FA4A7", color = "black", stroke = 0.3) +  
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray45", size = 0.3) +  
  coord_flip() +  
  labs(y = "Standardized effect size (95% CI)", x = "") +  
  scale_y_continuous(limits = c(-0.0025, 0.0025),  
                     breaks = seq(-0.0015, 0.0015, by = 0.0005),  
                     labels = scales::number_format(accuracy = 0.0001)) +  
  scale_x_discrete(labels = c(
    "O3*School→L-hippo→PLEs(low)" = expression(paste("O", phantom()[3], "*School→L-hippo→PLEs(low)")),
    "O3*School→L-hippo→PLEs(high)" = expression(paste("O", phantom()[3], "*School→L-hippo→PLEs(high)")),
    "O3*School→L-hippo→PLEs(moderated mediation)" = expression(paste("O", phantom()[3], "*School→L-hippo→PLEs(moderated mediation)"))
  )) +
  theme_minimal() +  
  theme(
    axis.text.x = element_text(size = 42, color = "black"),
    axis.text.y = element_text(size = 42, color = "black"),
    axis.title = element_text(size = 44, face = "bold"),
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
  path = "D:/ME/ABCD/environment-culture-brain/plot",
  width = 300,             
  height = 100,            
  units = "mm",          
  bg = "white",            
  dpi = 300              
)

### Supplemental Figure 2. Correlation plot
library(corrplot)
library(ggplot2)
library(ggpubr)
library(haven)
correlation_prenatal_o3 <- read_sav("D:/ME/ABCD/environment-culture-brain/correlation_prenatal o3.sav")
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

####supplemental figure 3 

library(showtext)


font_add("Arial", regular = "C:/WINDOWS/FONTS/Arial.ttf")


showtext_auto()

theme_set(theme_minimal(base_family = "Arial"))

library(haven)
participant_structure_total_brain <- read_sav("D:/ME/ABCD/environment-culture-brain/participant_structure_total_brain.sav")

participant_structure_total_brain <- replace(participant_structure_total_brain, participant_structure_total_brain  ==  999.000, NA);participant_structure_total_brain
summary(participant_structure_total_brain)


####### supplemental figure 3a:average_hippo
library(dplyr)
participant_structure_total_brain_clean <- participant_structure_total_brain %>%
  filter(!is.na(average_hippo))
summary(participant_structure_total_brain_clean)


participant_structure_total_brain_clean$site_id1 <- as.factor(participant_structure_total_brain_clean$site_id1)


library(ggplot2)
library(ggpubr)  

filtered_data <- subset(participant_structure_total_brain_clean, site_id1 %in% c("1", "2"))

ggplot(filtered_data, aes(x = site_id1, y = average_hippo)) +
  geom_boxplot(aes(color = site_id1), width = 0.4) +
  scale_color_manual(values = c("#BA3E45", "#3A4B6E"),
                     labels = c("Average", "Highest")) +  
  scale_x_discrete(labels = c("Average", "Highest")) +  
  stat_compare_means(method = "t.test",  
                     comparisons = list(c("Average", "Highest")),  
                     label.x = 0.8,
                     label.y = 78) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),    
        panel.grid.minor = element_blank(),    
        panel.border = element_blank(),        
        axis.line = element_line(color = "black"),  
        axis.title.x = element_text(size = 26),     
        axis.title.y = element_text(size = 26),    
        axis.text.x = element_text(size = 24),      
        axis.text.y = element_text(size = 24),     
        legend.title = element_text(size = 24),     
        legend.text = element_text(size =20)) +     
  labs(x = "", y = "Average Hippocampus Volume") +
  guides(color = guide_legend(title = expression(O[3])))  



ggsave(
  filename = "o3_test_average_hippo.png", 
  device = "png", 
  path = "D:/ME/ABCD/environment-culture-brain/plot",
  width = 80,             
  height = 100,            
  units = "mm",          
  bg = "white",            
  dpi = 300              
)


### supplemental figure 3b:average_amyg
library(dplyr)
participant_structure_total_brain_clean <- participant_structure_total_brain %>%
  filter(!is.na(average_amyg))
summary(participant_structure_total_brain_clean)


participant_structure_total_brain_clean$site_id1 <- as.factor(participant_structure_total_brain_clean$site_id1)


library(ggplot2)
library(ggpubr)  

filtered_data <- subset(participant_structure_total_brain_clean, site_id1 %in% c("1", "2"))

ggplot(filtered_data, aes(x = site_id1, y = average_amyg)) +
  geom_boxplot(aes(color = site_id1), width = 0.4) +
  scale_color_manual(values = c("#BA3E45", "#3A4B6E"),
                     labels = c("Average", "Highest")) +  
  scale_x_discrete(labels = c("Average", "Highest")) +  
  stat_compare_means(method = "t.test",  
                     comparisons = list(c("Average", "Highest")),  
                     label.x = 0.8,
                     label.y = 78) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),    
        panel.grid.minor = element_blank(),    
        panel.border = element_blank(),        
        axis.line = element_line(color = "black"),  
        axis.title.x = element_text(size = 26),     
        axis.title.y = element_text(size = 26),    
        axis.text.x = element_text(size = 24),      
        axis.text.y = element_text(size = 24),      
        legend.title = element_text(size = 24),     
        legend.text = element_text(size =20)) +     
  labs(x = "", y = "Average Amygdala Volume") +
  guides(color = guide_legend(title = expression(O[3])))  



ggsave(
  filename = "o3_test_average_amyg.png", 
  device = "png", 
  path = "D:/ME/ABCD/environment-culture-brain/plot",
  width = 80,             
  height = 100,            
  units = "mm",          
  bg = "white",            
  dpi = 300              
)


#### supplemental figure 3c: totalcognition 
library(dplyr)

participant_structure_total_brain_clean <- participant_structure_total_brain %>%
  filter(!is.na(totalcog2))


summary(participant_structure_total_brain_clean)


participant_structure_total_brain_clean$site_id1 <- as.factor(participant_structure_total_brain_clean$site_id1)


library(ggplot2)
library(ggpubr)  

filtered_data <- subset(participant_structure_total_brain_clean, site_id1 %in% c("1", "2"))

ggplot(filtered_data, aes(x = site_id1, y = totalcog2)) +
  geom_boxplot(aes(color = site_id1), width = 0.4) +
  scale_color_manual(values = c("#BA3E45", "#3A4B6E"),
                     labels = c("Average", "Highest")) +  
  scale_x_discrete(labels = c("Average", "Highest")) +  
  stat_compare_means(method = "t.test",  
                     comparisons = list(c("Average", "Highest")),  
                     label.x = 0.8,
                     label.y = 78) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),    
        panel.grid.minor = element_blank(),   
        panel.border = element_blank(),        
        axis.line = element_line(color = "black"),  
        axis.title.x = element_text(size = 26),     
        axis.title.y = element_text(size = 26),     
        axis.text.x = element_text(size = 24),      
        axis.text.y = element_text(size = 24),      
        legend.title = element_text(size = 24),    
        legend.text = element_text(size =20)) +     
  labs(x = "", y = "Total Cognition Scores") +
  guides(color = guide_legend(title = expression(O[3])))  



ggsave(
  filename = "o3_test_cog.png", 
  device = "png", 
  path = "D:/ME/ABCD/environment-culture-brain/plot",
  width = 80,             
  height = 100,            
  units = "mm",          
  bg = "white",            
  dpi = 300              
)

####### supplemental figure 3d:ples
library(dplyr)
participant_structure_total_brain_clean <- participant_structure_total_brain %>%
  filter(!is.na(ples2))
summary(participant_structure_total_brain_clean)


participant_structure_total_brain_clean$site_id1 <- as.factor(participant_structure_total_brain_clean$site_id1)


library(ggplot2)
library(ggpubr)  

filtered_data <- subset(participant_structure_total_brain_clean, site_id1 %in% c("1", "2"))

ggplot(filtered_data, aes(x = site_id1, y = ples2)) +
  geom_boxplot(aes(color = site_id1), width = 0.4) +
  scale_color_manual(values = c("#BA3E45", "#3A4B6E"),
                     labels = c("Average", "Highest")) + 
  scale_x_discrete(labels = c("Average", "Highest")) +  
  stat_compare_means(method = "t.test",  
                     comparisons = list(c("Average", "Highest")),
                     label.x = 0.8,
                     label.y = 78) +
  theme_bw() +
  theme(panel.grid.major = element_blank(),    
        panel.grid.minor = element_blank(),    
        panel.border = element_blank(),        
        axis.line = element_line(color = "black"),  
        axis.title.x = element_text(size = 26),    
        axis.title.y = element_text(size = 26),     
        axis.text.x = element_text(size = 24),      
        axis.text.y = element_text(size = 24),      
        legend.title = element_text(size = 24),     
        legend.text = element_text(size =20)) +     
  labs(x = "", y = "Psychotic-like Experiences") +
  guides(color = guide_legend(title = expression(O[3])))  



ggsave(
  filename = "o3_test_ples.png", 
  device = "png", 
  path = "D:/ME/ABCD/environment-culture-brain/plot",
  width = 80,             
  height = 100,            
  units = "mm",          
  bg = "white",            
  dpi = 300              
)


