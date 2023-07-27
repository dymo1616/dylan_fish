
library(ecolTest)

Hutcheson_t_test(
  caribbean_bout$start,
  caribbean_bout$end,
  shannon.base = exp(1),
  alternative = "two.sided",
  difference = 0
) 
hut