library(tidyverse)
library(stringr)

load("rs_fmri_extraction_data.RData")

mat <- mat %>%
  mutate(centre = c(rep("toulouse", 93), rep("aix-marseille",95)), 
         group = c(rep("group_1",21), rep("group_2",11), rep("group_3", 25), rep("group_4",15),rep("group_5",21),
                   rep("group_1",22), rep("group_2",19), rep("group_3", 14), rep("group_4",20),rep("group_5",20)))

ROIs_with_interaction <- colnames(mat)[str_detect(pattern = "interaction",colnames(mat))]
ROIs_with_main_effect <- colnames(mat)[str_detect(pattern = "main",colnames(mat))]

mat_long <- mat %>%
  select(-Condition) %>%
  gather(roi,value, -centre, -group) %>%
  mutate(value = as.numeric(value))

mat_long_int <- mat_long %>%
  split(., .$roi) %>%
  `[`(., str_detect(pattern = "interaction",names(.)))

mat_long_main <- mat_long %>%
  split(., .$roi) %>%
  `[`(., str_detect(pattern = "main",names(.)))

barplot_main_effect <- mat_long_main %>%
  map(~group_by(.,centre) %>%
  summarise(avg = mean(value), lower_ci = t.test(value)$conf.int[1], upper_ci = t.test(value)$conf.int[2])) %>%
  map2(.x = ., ROIs_with_main_effect, ~ggplot(data = .x, aes(x = centre, y = avg, fill = centre)) + 
        geom_bar(stat = "identity") + 
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2) + 
         ggtitle(.y) + 
         theme_bw() + 
         theme(plot.title = element_text(hjust = .5)) + 
         xlab("Centre") + 
         ylab(" Avg Connectivity"))

pdf("barplot_main_effect_centre.pdf", w = 6, h = 4)
print(barplot_main_effect)
dev.off()


barplot_interaction_effect <- mat_long_int %>%
  map(~group_by(.,centre, group) %>%
        summarise(avg = mean(value), lower_ci = t.test(value)$conf.int[1], upper_ci = t.test(value)$conf.int[2])) %>%
  map2(.x = ., ROIs_with_interaction, ~ggplot(data = .x, aes(x = interaction(centre, group), y = avg, fill = centre)) + 
         geom_bar(stat = "identity", position = position_dodge()) + 
         geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2, position = "dodge") + 
         ggtitle(.y) + 
         theme_bw() + 
         theme(plot.title = element_text(hjust = .5)) + 
         xlab("Centre by Group") + 
         ylab(" Avg Connectivity") +
         theme(axis.text.x = element_text(angle = 45, vjust = .5)))

pdf("barplot_interaction_centre_by_group.pdf", w = 6, h = 4)
print(barplot_interaction_effect)
dev.off()
