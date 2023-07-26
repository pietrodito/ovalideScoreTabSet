library(tidyverse)

ovalide::load_ovalide_tables(ovalide::nature())

(associated_tables <- names(ovalide::ovalide_tables(ovalide::nature()))[1:4])

# associated_tables <- NULL

(
  associated_tables
  |> purrr::map(\(x) ovalide::ovalide_tables(ovalide::nature())[[x]])
  |> purrr::map(\(t) DT::renderDT(t))
)

