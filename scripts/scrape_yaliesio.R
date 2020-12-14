library(tidyverse)
library(httr)
students <- POST(
  url = "https://yalies.io/api/students",
  add_headers(Authorization = "Bearer eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJpYXQiOjE2MDc3MTUxOTIsImV4cCI6MTYzOTI1MTE5Miwic3ViIjoibmNrMjYifQ.SP7L72hL__96D2uAe54B8H6JMHHWhAZKDXdS-3HrSoc"),
  encode = "json"
)

students <- content(students, type = "application/json") 

students <- map_dfr(students, ~map_dfc(., function(x)  ifelse(is.null(x), NA, x))) 

saveRDS(students, "data/students.RDS")
