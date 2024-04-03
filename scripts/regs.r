


#Effect of treatment

modelsummary::modelsummary(
  title = "Effect on index",
  list(
    total = lm(data = qol, index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    food = lm(data = qol %>% filter(case == "c1"), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    wfh = lm(data = qol %>% filter(case == "c2"), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    phone = lm(data = qol %>% filter(case == "c3"), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    retire = lm(data = qol %>% filter(case == "c4"), index~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  output = "flextable"
) %>% 
  flextable::save_as_docx("tables/fig1_1")

modelsummary::modelsummary(
  title = "Effect on convincing",
  list(
    total = lm(data = qol, con~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    food = lm(data = qol %>% filter(case == "c1"), con~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    wfh = lm(data = qol %>% filter(case == "c2"), con~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    phone = lm(data = qol %>% filter(case == "c3"), con~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    retire = lm(data = qol %>% filter(case == "c4"), con~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  output = "flextable"
) %>% 
  flextable::save_as_docx("tables/fig1_2")

modelsummary::modelsummary(
  title = "Effect on quality",
  list(
    total = lm(data = qol, qual~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    food = lm(data = qol %>% filter(case == "c1"), qual~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    wfh = lm(data = qol %>% filter(case == "c2"), qual~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    phone = lm(data = qol %>% filter(case == "c3"), qual~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    retire = lm(data = qol %>% filter(case == "c4"), qual~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  output = "flextable"
) %>% 
  flextable::save_as_docx("tables/fig1_3")

modelsummary::modelsummary(
  title = "Effect on motivation",
  list(
    total = lm(data = qol, mot~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    food = lm(data = qol %>% filter(case == "c1"), mot~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    wfh = lm(data = qol %>% filter(case == "c2"), mot~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    phone = lm(data = qol %>% filter(case == "c3"), mot~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    retire = lm(data = qol %>% filter(case == "c4"), mot~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  output = "flextable"
) %>% 
  flextable::save_as_docx("tables/fig1_4")

#Time taken

modelsummary::modelsummary(
  title = "Effect on time taken",
  list(
    total = lm(data = qol, time~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    food = lm(data = qol %>% filter(case == "c1"), time~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    wfh = lm(data = qol %>% filter(case == "c2"), time~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    phone = lm(data = qol %>% filter(case == "c3"), time~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order)),
    retire = lm(data = qol %>% filter(case == "c4"), time~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+factor(order))
  ),
  stars = T,
  output = "flextable"
) %>% 
  flextable::save_as_docx("tables/fig1_5")


#High cost differences

modelsummary::modelsummary(
  title = "Effect on attitude with interaction",
  list(
    food = lm(data = qol %>% filter(case == "c1"), index~condition*pol+gender+age+factor(edu)+factor(reg)+rel+sci+order),
    wfh = lm(data = qol %>% filter(case == "c2"), index~condition*edu+gender+age+factor(reg)+pol+rel+sci+order),
    phone = lm(data = qol %>% filter(case == "c3"), index~condition*age+gender+factor(edu)+factor(reg)+pol+rel+sci+order),
    retire = lm(data = qol %>% filter(case == "c4"), index~condition*edu+gender+age+factor(edu)+factor(reg)+pol+rel+sci+order)
  ),
  stars = T
)

#Manipulation check

modelsummary::modelsummary(
  title = "Effect on manipulation check",
  list(
    food = lm(data = qol %>% filter(case == "c1"), mancheck~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+order),
    wfh = lm(data = qol %>% filter(case == "c2"), mancheck~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+order),
    phone = lm(data = qol %>% filter(case == "c3"), mancheck~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+order),
    retire = lm(data = qol %>% filter(case == "c4"), mancheck~condition+gender+age+factor(edu)+factor(reg)+pol+rel+sci+order)
  ),
  stars = T
)

modelsummary::modelsummary(
  title = "Correlation with manipulation check",
  list(
    food = lm(data = qol %>% filter(case == "c1"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci+order),
    wfh = lm(data = qol %>% filter(case == "c2"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci+order),
    phone = lm(data = qol %>% filter(case == "c3"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci+order),
    retire = lm(data = qol %>% filter(case == "c4"), index~mancheck+gender+age+factor(edu)+factor(reg)+pol+rel+sci+order)
  ),
  stars = T
)


