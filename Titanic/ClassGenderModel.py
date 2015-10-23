import csv as csv 
import numpy as np

#Read in train file to data array
csv_file_object = csv.reader(open('train.csv', 'rb')) 
header = csv_file_object.next()  # Skip header
data=[]                          
for row in csv_file_object:      
    data.append(row)            
data = np.array(data) 


# Add a ceiling
fare_ceiling = 40
# then modify the data in the Fare column to = 39, if it is greater or equal to the ceiling
data[ data[0::,9].astype(np.float) >= fare_ceiling, 9 ] = fare_ceiling - 1.0

fare_bracket_size = 10
number_of_price_brackets = fare_ceiling / fare_bracket_size

number_of_classes = len(np.unique(data[0::,2])) 

# Initialize the survival table with all zeros
survival_table = np.zeros((2, number_of_classes, number_of_price_brackets))

for i in xrange(number_of_classes):       			  #loop through each class bin
	for j in xrange(number_of_price_brackets):   		  #loop through each price bin

		women_only_stats = data[                                    
		                     (data[0::,4] == "female")    
		                    &(data[0::,2].astype(np.float) == i+1)                       
		                    &(data[0:,9].astype(np.float) >= j*fare_bracket_size)             
		                    &(data[0:,9].astype(np.float) < (j+1)*fare_bracket_size) 
		                    , 1]                                                 
 						                                    									
		men_only_stats = data[                                      
		                     (data[0::,4] != "female")   
		                    &(data[0::,2].astype(np.float) == i+1)                                       
		                    &(data[0:,9].astype(np.float) >= j*fare_bracket_size)            
		                    &(data[0:,9].astype(np.float) < (j+1)*fare_bracket_size)  
		                    , 1] 


		survival_table[0,i,j] = 0. if np.size(women_only_stats)==0 else np.mean(women_only_stats.astype(np.float)) 
		survival_table[1,i,j] = 0. if np.size(men_only_stats)==0 else np.mean(men_only_stats.astype(np.float)) 

#All proportions greater than a half should predict survival
survival_table[ survival_table < 0.5 ] = 0
survival_table[ survival_table >= 0.5 ] = 1

test_file = open('test.csv', 'rb')
test_file_object = csv.reader(test_file)
header = test_file_object.next()
predictions_file = open("classgendermodel.csv", "wb")
p = csv.writer(predictions_file)
p.writerow(["PassengerId", "Survived"])

for row in test_file_object:                  # We are going to loop
                                              # through each passenger
                                              # in the test set                     
  for j in xrange(number_of_price_brackets):  # For each passenger we
                                              # loop through each price bin
    try:                                      # Some passengers have no
      row[8] = float(row[8])                  # Fare data so try to make a float
    except:                                   # If fails: no data, so 
      bin_fare = 3 - float(row[1])            # bin the fare according Pclass
      break                                   # Break from the loop
    if row[8] > fare_ceiling:                 # If there is data see if it is greater than
      bin_fare = number_of_price_brackets-1   # Fare ceiling we set earlier 
      break                                   # If so set to highest bin and then break loop
    if row[8] >= j * fare_bracket_size and row[8] < (j+1) * fare_bracket_size:                                   
      bin_fare = j                           
      break

    if row[3] == 'female':                             
    	p.writerow([row[0], "%d" % int(survival_table[0, float(row[1])-1, bin_fare])])
    else:                                         
        p.writerow([row[0], "%d" % int(survival_table[1, float(row[1])-1, bin_fare])])
     
# Close out the files.
test_file.close() 
predictions_file.close()


