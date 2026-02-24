library(tidyverse)

gss_cat |>
  view()

# Write code to count the number of unique responses to the partyid question

gss_cat |>
  separate_longer_delim(partyid, delim = ",") |>
  count(partyid)
