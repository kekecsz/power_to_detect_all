


sim_and_test = function(n, mean, sd){
  data = rnorm(n = n, mean = mean, sd = sd)
  p_value = t.test(data)$p.value
  significant = p_value<0.05
  return(significant)
}

multiple_effects = function(number_of_effects_tested, n, mean, sd){
results = replicate(number_of_effects_tested, sim_and_test(n = n, mean = mean, sd = sd))
return(results)
}


iterations = 10000
number_of_effects_tested = 5
result = replicate(iterations, multiple_effects(number_of_effects_tested = number_of_effects_tested, n = 119, mean = 0.3, sd = 1))
result = data.frame(t(result))
names(result) = paste0("effect_", 1:number_of_effects_tested, "_significant")

power_to_detect_effect_1 = sum(result[,1])/iterations
power_to_detect_effect_1


all_tests_in_the_study_significant = rowSums(result) == ncol(result)
power_to_detect_all_effects = sum(all_tests_in_the_study_significant)/iterations
power_to_detect_all_effects
