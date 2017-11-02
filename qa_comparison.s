library(tidyverse)
library(car)

qa_variables <- read_delim("QA_covariates_all.txt", delim = " ") %>%
  mutate(centre = c(rep("toulouse", 93), rep("aix-marseille",95)), 
         group = c(rep("group_1",21), rep("group_2",11), rep("group_3", 25), rep("group_4",15),rep("group_5",21),
                   rep("group_1",22), rep("group_2",19), rep("group_3", 14), rep("group_4",20),rep("group_5",20)))

qa_variables_centre_by_group.anova <- qa_variables %>%
  gather(., measure, values, -centre, -group) %>%
  split(., .$measure) %>%
  map(~lm(data = ., values ~ centre*group)) %>%
  map(~Anova(mod = ., t = 3))

qa_measure_with_main_effect_centre <- c("QA_GCOR_rest", "QA_InvalidScans", "QA_MaxGlobal")
qa_measure_with_main_effect_group <- c("QA_GCOR_rest", "QA_WhiteMatter_eroded_vol", "QA_WhiteMatter_vol")
qa_measure_with_interaction <- c("QA_CSF_eroded_vol", "QA_CSF_vol", "QA_MaxMotion")

barplot_main_effect_centre <- qa_variables %>%
  gather(., measure, values, -centre, -group) %>%
  split(., .$measure) %>%
  `[`(., names(.) %in% qa_measure_with_main_effect_centre) %>%
  map(~group_by(.,centre) %>%
        summarise(avg = mean(values), lower_ci = t.test(values)$conf.int[1], upper_ci = t.test(values)$conf.int[2])) %>%
  map2(.x = ., qa_measure_with_main_effect_centre, ~ggplot(data = .x, aes(x = centre, y = avg, fill = centre)) + 
        geom_bar(stat = "identity") + 
        geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2) + 
        ggtitle(.y) + 
        theme_bw() + 
        theme(plot.title = element_text(hjust = .5)))
  
pdf("qa_with_main_effect_of_centre.pdf", w = 5, h = 4)
print(barplot_main_effect_centre)
dev.off()

barplot_main_effect_group <- qa_variables %>%
  gather(., measure, values, -centre, -group) %>%
  split(., .$measure) %>%
  `[`(., names(.) %in% qa_measure_with_main_effect_group) %>%
  map(~group_by(.,group) %>%
        summarise(avg = mean(values), lower_ci = t.test(values)$conf.int[1], upper_ci = t.test(values)$conf.int[2])) %>%
  map2(.x = ., qa_measure_with_main_effect_group, ~ggplot(data = .x, aes(x = group, y = avg, fill = group)) + 
         geom_bar(stat = "identity") + 
         geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = .2) + 
         ggtitle(.y) + 
         theme_bw() + 
         theme(plot.title = element_text(hjust = .5)))

pdf("qa_with_main_effect_of_group.pdf", w = 5, h = 4)
print(barplot_main_effect_group)
dev.off()

barplot_interaction <- qa_variables %>%
  gather(., measure, values, -centre, -group) %>%
  split(., .$measure) %>%
  `[`(., names(.) %in% qa_measure_with_interaction) %>%
  map(~group_by(.,centre,group) %>%
        summarise(avg = mean(values), lower_ci = t.test(values)$conf.int[1], upper_ci = t.test(values)$conf.int[2])) %>%
  map2(.x = ., qa_measure_with_interaction, ~ggplot(data = .x, aes(x = interaction(centre,group), y = avg, fill = centre)) + 
         geom_bar(stat = "identity", position = position_dodge()) + 
         geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), position = "dodge", width = .2) + 
         ggtitle(.y) + 
         theme_bw() + 
         theme(plot.title = element_text(hjust = .5)) + 
         theme(axis.text.x = element_text(angle = 45, vjust = .5)))

pdf("qa_with_interaction.pdf", w = 5, h = 4)
print(barplot_interaction)
dev.off()
