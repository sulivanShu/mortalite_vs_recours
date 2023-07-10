#!/usr/bin/Rscript
library(tibble)
library(parallel)
cl =
	detectCores() |>
	makeCluster()
setDefaultCluster(cl)
tasks_list_2 = c(
		 "hello" = "hello",
		 "bye" = "bye"
)
tasks_list_2 |> 
	parLapply(cl = NULL, function(task) {
			  if (task == "hello") {
				  salutations = print("bonjour!") 
				  return(salutations)
			  }
			  if (task == "bye") {
				  return("au revoir!")
			  }
}) |>
		 print()
