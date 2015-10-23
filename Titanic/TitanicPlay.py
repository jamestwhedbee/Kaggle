import csv as csv 
import numpy as np

csv_file_object = csv.reader(open('train.csv', 'rb')) 
header = csv_file_object.next()  # Skip header
data=[]                          
for row in csv_file_object:      
    data.append(row)            
data = np.array(data) 	       


women = data[0::,4] == "female"
men = data[0::,4] != "female"  

# Using the index from above we select the females and males separately
women_onboard = data[women,1].astype(np.float)     
men_onboard = data[men,1].astype(np.float)

# Then we finds the proportions of them that survived
proportion_women_survived = \
                       np.sum(women_onboard) / np.size(women_onboard)  
proportion_men_survived = \
                       np.sum(men_onboard) / np.size(men_onboard) 

# and then print it out
print 'Proportion of women who survived is %s' % proportion_women_survived
print 'Proportion of men who survived is %s' % proportion_men_survived
