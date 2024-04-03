
library(tidyverse)
library(broom)
library(patchwork)


# balance check -----------------------------------------------------------


#recoding data for sample characteritics table
desc <- qol %>% 
  mutate(
    age = case_when(
      age<=29 ~ "18-29",
      age>=30 & age<=39 ~ "30-39",
      age>=40 & age<=49 ~ "40-49",
      age>=50 & age<=64 ~ "50-64",
      age>=65 ~ "65+"
    )
  ) %>% 
  mutate(
    reg = case_when(
      reg == 1 ~ "Capital region",
      reg == 2 ~ "Zealand",
      reg == 3 ~ "Southern Denmark",
      reg == 4 ~ "Middle Jutland",
      reg == 5 ~ "North Jutland"
    )
  ) %>% 
  mutate(
    edu = case_when(
      edu == 1 ~ "Primary education",
      edu == 2 ~ "General upper secondary",
      edu == 3 ~ "Vocational upper secondary",
      edu == 4 ~ "Bachelors (or equivalent)",
      edu == 5 ~ "Masters degree",
      edu == 6 ~ "PhD"
    )
  )

#making table of proportions for categorical variables and saving as .csv
plyr::join_all(
  type = "full",
  
  list(
    readxl::read_xlsx("data/strata.xlsx"),
    
    desc %>% 
      filter(case == "c1") %>% 
      select(age, gender, reg, edu) %>% 
      pivot_longer(1:4) %>% 
      summarize(n = n(), .by = everything()) %>% 
      mutate(p = n/sum(n), .by = name) %>% 
      select(-n),
    
    desc %>% 
      select(age, gender, reg, edu, condition, case) %>% 
      pivot_longer(1:4) %>% 
      summarize(n = n(), .by = everything()) %>% 
      mutate(p = n/sum(n), .by = c(name, condition, case)) %>% 
      arrange(case, condition) %>% 
      select(-n) %>% 
      pivot_wider(names_from = c(case, condition), values_from = p)
  )
) %>% 
  
  write_csv("tables/balancecat.csv")


#getting data for continuous data and saving as .csv
left_join(
  qol %>% 
    filter(case == "c1") %>% 
    select(pol, rel, sci) %>% 
    pivot_longer(1:3) %>% 
    summarize(m = mean(value), .by = name),
  qol %>% 
    select(pol, rel, sci, case, condition) %>% 
    pivot_longer(1:3) %>% 
    summarize(m = mean(value), .by = c(name, case, condition)) %>% 
    arrange(case, condition) %>% 
    pivot_wider(names_from = c(case, condition), values_from = m)
) %>% 
  write_csv("tables/balancecon.csv")


# regressions fig 1 --------------------------------------------------------

#Producing results shown in figure 1 as tables, saving as docx

modelsummary::modelsummary(
  title = "Effect on attitude",
  list(
    total = lm(data = qol, index~condition+factor(order)+factor(id)),
    food = lm(data = qol %>% filter(case == "c1"), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    wfh = lm(data = qol %>% filter(case == "c2"), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    phone = lm(data = qol %>% filter(case == "c3"), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    retire = lm(data = qol %>% filter(case == "c4"), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  coef_omit = "id",
  fmt = 2,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/fig1_1.docx")

modelsummary::modelsummary(
  title = "Effect on attitude",
  list(
    total = lm(data = qol, con~condition+factor(order)+factor(id)),
    food = lm(data = qol %>% filter(case == "c1"), con~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    wfh = lm(data = qol %>% filter(case == "c2"), con~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    phone = lm(data = qol %>% filter(case == "c3"), con~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    retire = lm(data = qol %>% filter(case == "c4"), con~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  coef_omit = "id",
  fmt = 2,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/fig1_2.docx")

modelsummary::modelsummary(
  title = "Effect on attitude",
  list(
    total = lm(data = qol, qual~condition+factor(order)+factor(id)),
    food = lm(data = qol %>% filter(case == "c1"), qual~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    wfh = lm(data = qol %>% filter(case == "c2"), qual~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    phone = lm(data = qol %>% filter(case == "c3"), qual~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    retire = lm(data = qol %>% filter(case == "c4"), qual~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  coef_omit = "id",
  fmt = 2,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/fig1_3.docx")

modelsummary::modelsummary(
  title = "Effect on attitude",
  list(
    total = lm(data = qol, mot~condition+factor(order)+factor(id)),
    food = lm(data = qol %>% filter(case == "c1"), mot~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    wfh = lm(data = qol %>% filter(case == "c2"), mot~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    phone = lm(data = qol %>% filter(case == "c3"), mot~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    retire = lm(data = qol %>% filter(case == "c4"), mot~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  coef_omit = "id",
  fmt = 2,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/fig1_4.docx")

modelsummary::modelsummary(
  title = "Effect on time taken",
  list(
    total = lm(data = qol, time~condition+factor(order)+factor(id)),
    food = lm(data = qol %>% filter(case == "c1"), time~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    wfh = lm(data = qol %>% filter(case == "c2"), time~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    phone = lm(data = qol %>% filter(case == "c3"), time~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    retire = lm(data = qol %>% filter(case == "c4"), time~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  coef_omit = "id",
  fmt = 2,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/fig1_5.docx")



# regressions fig 2 ----------------------------------------------------

#Producing results shown in figure 2 as tables

modelsummary::modelsummary(
  title = "food splits",
  list(
    left = lm(data = qol %>% filter(case == "c1", pol<4), index~condition+gender+age+factor(edu)+factor(reg)+rel+sci+factor(order)),
    center = lm(data = qol %>% filter(case == "c1", pol>=4 & pol<=6), index~condition+gender+age+factor(edu)+factor(reg)+rel+sci+factor(order)),
    right = lm(data = qol %>% filter(case == "c1", pol>6), index~condition+gender+age+factor(edu)+factor(reg)+rel+sci+factor(order))
  ),
  stars = T,
  coef_omit = "id",
  fmt = 2,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/fig2_1.docx")

modelsummary::modelsummary(
  title = "WfH splits",
  list(
    primary = lm(data = qol %>% filter(case == "c2", edu == 1), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order)),
    "high school" = lm(data = qol %>% filter(case == "c2", edu == 2), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order)),
    vocational = lm(data = qol %>% filter(case == "c2", edu == 3), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order)),
    bachelors = lm(data = qol %>% filter(case == "c2", edu == 4), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order)),
    masters = lm(data = qol %>% filter(case == "c2", edu >= 5), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  coef_omit = "id",
  fmt = 2,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/fig2_2.docx")

modelsummary::modelsummary(
  title = "phone splits",
  list(
    "below 30" = lm(data = qol %>% filter(case == "c3", age<30), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    "30-39" = lm(data = qol %>% filter(case == "c3", age>=30 & age<40), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    "40-59" = lm(data = qol %>% filter(case == "c3", age>=40 & age<60), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    "60 and up" = lm(data = qol %>% filter(case == "c3", age>=60), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  coef_omit = "id",
  fmt = 2,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/fig2_3.docx")

modelsummary::modelsummary(
  title = "retierement splits",
  list(
    "below 30" = lm(data = qol %>% filter(case == "c4", age<30), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    "30-39" = lm(data = qol %>% filter(case == "c4", age>=30 & age<40), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    "40-59" = lm(data = qol %>% filter(case == "c4", age>=40 & age<60), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    "60 and up" = lm(data = qol %>% filter(case == "c4", age>=60), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  coef_omit = "id",
  fmt = 2,
  output = "flextable"
) %>% 
  flextable::save_as_docx(path = "tables/fig2_4.docx")


# moderation matrix -------------------------------------------------------

#doing split sample regressions across all potential moderators

bigmod <- tibble()

for (c in c("c1", "c2", "c3", "c4")) {
  
  bind_rows(
    bigmod,
    
    tidy(lm(data = qol %>% filter(case == c, pol<4), index~condition+gender+age+factor(edu)+factor(reg)+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Left/right", mod = "Left"),
    tidy(lm(data = qol %>% filter(case == c, pol>=4 & pol<=6), index~condition+gender+age+factor(edu)+factor(reg)+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Left/right", mod = "Center"),
    tidy(lm(data = qol %>% filter(case == c, pol>6), index~condition+gender+age+factor(edu)+factor(reg)+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Left/right", mod = "Right"),
    
    tidy(lm(data = qol %>% filter(case == c, rel<4), index~condition+gender+age+factor(edu)+factor(reg)+pol+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Religion", mod = "Low"),
    tidy(lm(data = qol %>% filter(case == c, rel>=4 & rel<=6), index~condition+gender+age+factor(edu)+factor(reg)+pol+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Religion", mod = "Middle"),
    tidy(lm(data = qol %>% filter(case == c, rel>6), index~condition+gender+age+factor(edu)+factor(reg)+pol+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Religion", mod = "High"),
    
    tidy(lm(data = qol %>% filter(case == c, sci<mean(qol$sci)), index~condition+gender+age+factor(edu)+factor(reg)+rel+pol+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Science\nConfidence", mod = "Below mean"),
    tidy(lm(data = qol %>% filter(case == c, sci>mean(qol$sci)), index~condition+gender+age+factor(edu)+factor(reg)+rel+pol+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Science\nConfidence", mod = "Above mean"),
    
    tidy(lm(data = qol %>% filter(case == c, gender == "Man"), index~condition+factor(edu)+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Gender", mod = "Man"),
    tidy(lm(data = qol %>% filter(case == c, gender == "Woman"), index~condition+factor(edu)+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Gender", mod = "Woman"),
    
    tidy(lm(data = qol %>% filter(case == c, edu == 1), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Education", mod = "Primary education"),
    tidy(lm(data = qol %>% filter(case == c, edu == 2), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Education", mod = "General upper secondary"),
    tidy(lm(data = qol %>% filter(case == c, edu == 3), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Education", mod = "Vocational upper secondary"),
    tidy(lm(data = qol %>% filter(case == c, edu == 4), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Education", mod = "Bachelors (or equivalent)"),
    tidy(lm(data = qol %>% filter(case == c, edu >= 5), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Education", mod = "Masters degree"),
    
    tidy(lm(data = qol %>% filter(case == c, age<30), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Age", mod = "18-29"),
    tidy(lm(data = qol %>% filter(case == c, age>=30 & age<40), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Age", mod = "30-39"),
    tidy(lm(data = qol %>% filter(case == c, age>=40 & age<60), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Age", mod = "40-59"),
    tidy(lm(data = qol %>% filter(case == c, age>=60), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
      filter(term == "conditiont") %>% mutate(case = paste(c), var = "Age", mod = "60+"),
    
  ) -> bigmod
  
}


bigmod %>% 
  mutate(
    pred = case_when(
      case == "c1" & var == "Left/right" ~ T,
      case == "c2" & var == "Education" ~ T,
      case == "c3" & var == "Age" ~ T,
      case == "c4" & var == "Age" ~ T,
      .default = F
    )
  ) %>% 
  mutate(
    case = case_when(
      case == "c1" ~ "Eating habits",
      case == "c2" ~ "WfH regulation",
      case == "c3" ~ "Phone use",
      case == "c4" ~ "Retirement age"
    ) 
  ) %>% 
  mutate(
    mod = fct_inorder(mod),
    case = fct_inorder(case)
  ) %>%
  
  ggplot(
    aes(
      y = estimate, 
      ymax = estimate+1.96*std.error, 
      ymin = estimate-1.96*std.error,
      x = mod,
      fill = pred
    )
  ) +
  geom_pointrange(position = position_dodge(.5), shape = 22, color = aucolr::picker()) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_fill_manual(values = c("white", aucolr::picker())) +
  facet_grid(case~var, scale = "free_x") +
  ylab("Estimate (standardized)") +
  xlab("") +
  coord_cartesian(ylim = c(-1,.75)) +
  jtools::theme_nice(legend.pos = "none") +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4),
    panel.spacing = unit(2, "lines")
  )

ggsave("figures/suppmod.png", width = 8, height = 10)
