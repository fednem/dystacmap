library(readxl)
library(tidyr)
library(stringr)

age_sex_and_group = read_excel("age_DYSTACMAP.xlsx")

rs_included_subjects = scan("DysTacMap_rs_analysis_included_subject.txt", "%s%") %>%
  str_split("_rs") %>%
  sapply(.,"[",1)

age_sex_and_group = age_sex_and_group %>%
    separate(Code,c("first","second","third")) %>%
    unite(code, first,second, sep= "") %>%
    unite(code, code, third, sep = "_")  

names(age_sex_and_group) =  tolower(names(age_sex_and_group))

group_sizes = table(age_sex_and_group$group)

re_processed_subject = c("106_PL","117_LL", "120_MS", "205_ST", "216_MO", "307_AE", "308_LM", "328_OM", "402_AC", "404_CT", "506_VL")

age_sex_and_group$re_processed = vector(length = nrow(age_sex_and_group), mode = "numeric")

age_sex_and_group$re_processed [age_sex_and_group$code %in% re_processed_subject] = 1

#Chi squared test to assess that the distribution of the preprocessed subjects does not differ among groups
chisq_re_processed_subjects = chisq.test(table(age_sex_and_group$gr, age_sex_and_group$re_processed))
