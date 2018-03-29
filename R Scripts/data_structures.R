
#### DATA STRUCTURES ####

### VECTORS ###
# Numeric
a = c(4, 63.12, 2E+45, -1/2)
a[1]

# Character
b = c('Cas', '§(', 'No Hablo Español')
b[3]

# Logical
c = c(TRUE, FALSE, T, F)

# Arrays
array(5, dim=c(10, 2, 2))
matrix(sample(1:7, 50, replace=T), nrow=10, ncol=5)

# Lists
list(5.2, c(T,F,F), 'Test')

# Data frame
data.frame(col1 = c(50,4,3,2.0), col2 = c(F,F,T,F), col3 = c('Blue','Blue','Red','Blue'))








# # Factors
# # for categorical data
# 
# data = c('High', 'Low', 'Medium', 'Low', 'Medium', 'Low')
# fdata = factor(data)
# fdata 


