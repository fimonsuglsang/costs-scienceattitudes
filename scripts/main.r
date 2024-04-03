
library(tidyverse)
library(broom)
library(patchwork)

#regressions

regs <- tibble()

#saving regression results as a tibble
for(o in c("index", "con", "qual", "mot", "stime")){
  bind_rows(
    regs,
    tidy(lm(
      data = qol %>% rename(var = o), 
      var~condition+factor(order)+factor(id))
    ) %>% 
      filter(term == "conditiont") %>% 
      mutate(outcome = paste(o), case = "total")
  ) -> regs
  for(c in c("c1", "c2", "c3", "c4")){
    bind_rows(
      regs,
      tidy(lm(
        data = qol %>% rename(var = o) %>% filter(case == c), 
        var~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
      ) %>% 
        filter(term == "conditiont") %>% 
        mutate(outcome = paste(o), case = paste(c))
    ) -> regs
  }
}

#renaming for plotting
regs %>%
  mutate(
    case = case_when(
      case == "total" ~ "Aggregate",
      case == "c1" ~ "Eating habits",
      case == "c2" ~ "WfH regulation",
      case == "c3" ~ "Phone use",
      case == "c4" ~ "Retirement age"
    ),
    outcome = case_when(
      outcome == "index" ~ "Index",
      outcome == "con" ~ "Convincing",
      outcome == "qual" ~ "Quality",
      outcome == "mot" ~ "Motivation",
      outcome == "stime" ~ "Time taken",
    )
  ) %>% 
  #reordering for plotting
  mutate(
    case = fct_inorder(case),
    outcome = fct_inorder(outcome)
  ) %>% 
  #plotting
  ggplot(
    aes(
      y = estimate, 
      ymax = estimate+1.96*std.error, 
      ymin = estimate-1.96*std.error,
      x = outcome,
      shape = outcome,
      size = outcome
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(.5), fill = "white", color = aucolr::picker()) +
  scale_shape_manual(values = c(22,21,21,21,16)) +
  scale_size_manual(values = c(1,.5,.5,.5, .5)) +
  xlab("") +
  ylab("Estimate (standardized)") +
  facet_grid(.~case) +
  jtools::theme_nice(legend.pos = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))

ggsave("figures/fig1.png", width = 8, height = 3)


#moderations 


#getting split-sample regression results, saving as a tibble
bind_rows(
  
  tidy(lm(data = qol %>% filter(case == "c1", pol<4), index~condition+gender+age+factor(edu)+factor(reg)+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c1", mod = "Left"),
  tidy(lm(data = qol %>% filter(case == "c1", pol>=4 & pol<=6), index~condition+gender+age+factor(edu)+factor(reg)+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c1", mod = "Center"),
  tidy(lm(data = qol %>% filter(case == "c1", pol>6), index~condition+gender+age+factor(edu)+factor(reg)+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c1", mod = "Right"),
  
  tidy(lm(data = qol %>% filter(case == "c2", edu == 1), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c2", mod = "Primary education"),
  tidy(lm(data = qol %>% filter(case == "c2", edu == 2), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c2", mod = "General upper secondary"),
  tidy(lm(data = qol %>% filter(case == "c2", edu == 3), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c2", mod = "Vocational upper secondary"),
  tidy(lm(data = qol %>% filter(case == "c2", edu == 4), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c2", mod = "Bachelors (or equivalent)"),
  tidy(lm(data = qol %>% filter(case == "c2", edu >= 5), index~condition+gender+age+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c2", mod = "Masters degree"),
  
  tidy(lm(data = qol %>% filter(case == "c3", age<30), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c3", mod = "18-29"),
  tidy(lm(data = qol %>% filter(case == "c3", age>=30 & age<40), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c3", mod = "30-39"),
  tidy(lm(data = qol %>% filter(case == "c3", age>=40 & age<60), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c3", mod = "40-59"),
  tidy(lm(data = qol %>% filter(case == "c3", age>=60), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c3", mod = "60+"),
  
  tidy(lm(data = qol %>% filter(case == "c4", age<30), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c4", mod = "18-29"),
  tidy(lm(data = qol %>% filter(case == "c4", age>=30 & age<40), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c4", mod = "30-39"),
  tidy(lm(data = qol %>% filter(case == "c4", age>=40 & age<60), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c4", mod = "40-59"),
  tidy(lm(data = qol %>% filter(case == "c4", age>=60), index~condition+gender+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(case = "c4", mod = "60+")

) -> moderz

#renaming for plotting
moderz %>%
  mutate(
    case = case_when(
      case == "c1" ~ "Eating habits\nby political position",
      case == "c2" ~ "WfH regulation\nby education",
      case == "c3" ~ "Phone use\nby age",
      case == "c4" ~ "Retirement age\nby age",
    )
  ) %>% 
  #reordering for plotting
  mutate(
    case = fct_inorder(case),
    mod = fct_inorder(mod)
  ) %>% 
  #plotting
  ggplot(
    aes(
      y = estimate, 
      ymax = estimate+1.96*std.error, 
      ymin = estimate-1.96*std.error,
      x = mod
    )
  ) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_pointrange(position = position_dodge(.5), size = 1, shape = 22, color = aucolr::picker(), fill = "white") +
  xlab("") +
  ylab("Estimate (standardized)") +
  facet_grid(.~case, scale = "free_x") +
  jtools::theme_nice(legend.pos = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))

ggsave("figures/fig2.png", width = 8, height = 5)

#mancheck


#getting regressions estimating manipulationcheck level
bind_rows(
  tidy(lm(data = qol %>% filter(case == "c1"), mancheck~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(case = "Eating habits"),
  tidy(lm(data = qol %>% filter(case == "c2"), mancheck~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(case = "WfH regulation"),
  tidy(lm(data = qol %>% filter(case == "c3"), mancheck~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(case = "Phone use"),
  tidy(lm(data = qol %>% filter(case == "c4"), mancheck~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(case = "Retirement age")
) %>% 
  mutate(case = fct_inorder(case) %>% fct_rev()) %>%  
  #plotting
  ggplot(
    aes(
      x = estimate, 
      xmax = estimate+1.96*std.error,
      xmin = estimate-1.96*std.error,
      y = case
    )
  ) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_pointrange(
    position = position_dodge(.3),
    shape = 22,
    size = 1,
    fill = "white",
    color = aucolr::picker()
  )  +
  xlab("Estimated effect of treatment\non manipulation check") +
  ylab("") +
  jtools::theme_nice() +
  theme(
    legend.title = element_blank(),
    legend.position = c(.376,.95) 
  ) +
  
  #getting attitude-manipulationcorrelations, by condition
  bind_rows(
    
    tidy(lm(data = qol %>% filter(case == "c1", condition == "t"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
      filter(term == "mancheck") %>% mutate(case = "Eating habits", condition = "Treatment"),
    tidy(lm(data = qol %>% filter(case == "c2", condition == "t"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
      filter(term == "mancheck") %>% mutate(case = "WfH regulation", condition = "Treatment"),
    tidy(lm(data = qol %>% filter(case == "c3", condition == "t"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
      filter(term == "mancheck") %>% mutate(case = "Phone use", condition = "Treatment"),
    tidy(lm(data = qol %>% filter(case == "c4", condition == "t"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
      filter(term == "mancheck") %>% mutate(case = "Retirement age", condition = "Treatment"),
    
    tidy(lm(data = qol %>% filter(case == "c1", condition == "c"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
      filter(term == "mancheck") %>% mutate(case = "Eating habits", condition = "Control"),
    tidy(lm(data = qol %>% filter(case == "c2", condition == "c"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
      filter(term == "mancheck") %>% mutate(case = "WfH regulation", condition = "Control"),
    tidy(lm(data = qol %>% filter(case == "c3", condition == "c"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
      filter(term == "mancheck") %>% mutate(case = "Phone use", condition = "Control"),
    tidy(lm(data = qol %>% filter(case == "c4", condition == "c"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
      filter(term == "mancheck") %>% mutate(case = "Retirement age", condition = "Control")
    
  ) %>% 
  mutate(case = fct_inorder(case) %>% fct_rev()) %>% 
  #plotting
  ggplot(
    aes(
      x = estimate, 
      xmax = estimate+1.96*std.error,
      xmin = estimate-1.96*std.error,
      y = case,
      fill = condition
    )
  ) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  geom_pointrange(
    position = position_dodge(-.3),
    shape = 22,
    size = 1,
    color = aucolr::picker()
  )  +
  xlab("Correlation between manipulation\ncheck value and index") +
  ylab("") +
  scale_fill_manual(values = c(aucolr::picker(), "white")) +
  jtools::theme_nice() +
  theme(
    legend.title = element_blank(),
    legend.position = c(.78,.98) 
  )

ggsave("figures/fig3.png", width = 8, height = 4)


# add tests


#getting prior treatment amount for additional tests
qol %>% 
  arrange(id, order) %>% 
  mutate(
    con = case_when(
      condition == "t" ~ 1,
      condition == "c" ~ 0
    )
  ) %>% 
  mutate(
    count = case_when(order == 1 ~ con),
    count = case_when(order == 2 ~ con + lag(count), .default = count),
    count = case_when(order == 3 ~ con + lag(count), .default = count),
    count = case_when(order == 4 ~ con + lag(count), .default = count)
  ) %>% 
  
  mutate(
    pretreat = case_when(order != 1 ~ lag(count), .default = 0),
  ) -> explore

#running split sample regressions
bind_rows(
  tidy(lm(data = explore %>% filter(time<quantile(qol$time)[2]), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(var = "Time taken", val = "Lowest quartile"),
  tidy(lm(data = explore %>% filter(time>=quantile(qol$time)[2] & time<quantile(qol$time)[3]), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(var = "Time taken", val = "second quartile"),
  tidy(lm(data = explore %>% filter(time>=quantile(qol$time)[3] & time<quantile(qol$time)[4]), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(var = "Time taken", val = "Third quartile"),
  tidy(lm(data = explore %>% filter(time>=quantile(qol$time)[4]), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))) %>% 
    filter(term == "conditiont") %>% mutate(var = "Time taken", val = "Highest quartile"),
  
  tidy(lm(data = explore %>% filter(order == 1), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(var = "Case number", val = "First"),
  tidy(lm(data = explore %>% filter(order == 2), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(var = "Case number", val = "second"),
  tidy(lm(data = explore %>% filter(order == 3), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(var = "Case number", val = "Third"),
  tidy(lm(data = explore %>% filter(order == 4), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(var = "Case number", val = "Fourth"),
  
  tidy(lm(data = explore %>% filter(pretreat == 0), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(var = "Prior treatments", val = "Zero"),
  tidy(lm(data = explore %>% filter(pretreat == 1), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(var = "Prior treatments", val = "One"),
  tidy(lm(data = explore %>% filter(pretreat == 2), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(var = "Prior treatments", val = "Two"),
  tidy(lm(data = explore %>% filter(pretreat == 3), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci)) %>% 
    filter(term == "conditiont") %>% mutate(var = "Prior treatments", val = "Three")
) %>% 
  
  mutate(
    var = fct_inorder(var),
    val = fct_inorder(val)
  ) %>% 
  #and plotting
  ggplot(
    aes(
      y = estimate,
      ymax = estimate+1.96*std.error, 
      ymin = estimate-1.96*std.error,
      x = val,
      group = var
    )
  ) +
  geom_hline(yintercept = 0, linetype = "longdash") +
  geom_line(linetype = "dashed", alpha = .7, color = aucolr::picker()) +
  geom_pointrange(position = position_dodge(.5), shape = 22,size = 1, fill = "white", color = aucolr::picker()) +
  xlab("") +
  ylab("Estimate (standardized)") +
  coord_cartesian(ylim = c(-.5, .2)) +
  facet_grid(.~var, scale = "free_x") +
  jtools::theme_nice(legend.pos = "none") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .4))

ggsave("figures/fig4.png", width = 8, height = 4)




