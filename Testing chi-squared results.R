#Checking chi-squared results 

# Define the counts
expected_counts <- c(6512, 799, 462, 4088)
observed_counts <- c(6512, 799, 462, 4088)
observed_counts <- c(6123, 1162, 938, 3637)

condition<-c("type1", "type2", "type3", "type4")
pvalues<-numeric(length(condition))


for(i in seq_along(condition)){
  cont_tab<-matrix(c(observed_counts[i], expected_counts[i], sum(observed_counts)-observed_counts[i],
                     sum(expected_counts[i])-expected_counts[i]), nrow=2, byrow=TRUE)



test<-chisq.test(cont_tab)
pvalues[i]<-test$p.value

cat(paste(condition[i], "pvalue:", round(pvalues[i], 3), "\n"))
}

results<-data.frame(Condition = condition, Simulated = observed_counts, Expected = expected_counts, P_values=pvalues)

results


# Perform Pearson's Chi-squared test
chi_squared_result <- chisq.test

# Print the chi-squared test result
print(chi_squared_result)

chi.vals <- matrix(c(6105, 1187, 929, 3639, 6512, 799, 462, 4088), ncol = 4, byrow = T)
chisq.test(chi.vals)



