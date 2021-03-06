# CODE BOOK

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.


Below is a list of the variables in FinalData.txt with devided into descriptive categories.

## Identifying Variables:

1. **subject:** Test subject ID
2. **activity:** What type of activity was being performed 
            
            activity labels:
              
               1. Walking: subject walking during the test
              
               2. Walking_Up: subject was walking up the stairs during the test
              
               3. Walking_Down: subject was walking down the stairs during the test
              
               4. Sitting: subject was sitting the test
               
               5. Standing: subject was standing during the test
              
               6. Laying: subject was laying down during the test

## Measurement Variables:
All variable names are descriptive of what was being measured. All measurements were between -1 and 1. std = Standard Deviation.

 * TimeBodyAccelerometer.mean...X                    
 * TimeBodyAccelerometer.mean...Y                    
 * TimeBodyAccelerometer.mean...Z                    
 * TimeGravityAccelerometer.mean...Y                 
 * TimeGravityAccelerometer.mean...
 * TimeBodyAccelerometerJerk.mean...X                
 * TimeBodyAccelerometerJerk.mean...Y                
 * TimeBodyAccelerometerJerk.mean...Z            
 * TimeBodyGyroscope.mean...X                       
 * TimeBodyGyroscope.mean...Y                        
 * TimeBodyGyroscope.mean...Z                        
 * TimeBodyGyroscopeJerk.mean...X                   
 * TimeBodyGyroscopeJerk.mean...Y                    
 * TimeBodyGyroscopeJerk.mean...Z                    
 * TimeBodyAccelerometerMagnitude.mean..             
 * TimeGravityAccelerometerMagnitude.mean..          
 * TimeBodyAccelerometerJerkMagnitude.mean..         
 * TimeBodyGyroscopeMagnitude.mean..                 
 * TimeBodyGyroscopeJerkMagnitude.mean..             
 * FrequencyBodyAccelerometer.mean...X               
 * FrequencyBodyAccelerometer.mean...Y               
 * FrequencyBodyAccelerometer.mean...Z               
 * FrequencyBodyAccelerometer.meanFreq...X           
 * FrequencyBodyAccelerometer.meanFreq...Y           
 * FrequencyBodyAccelerometer.meanFreq...Z           
 * FrequencyBodyAccelerometerJerk.mean...X           
 * FrequencyBodyAccelerometerJerk.mean...Y           
 * FrequencyBodyAccelerometerJerk.mean...Z           
 * FrequencyBodyAccelerometerJerk.meanFreq...X       
 * FrequencyBodyAccelerometerJerk.meanFreq...Y       
 * FrequencyBodyAccelerometerJerk.meanFreq...Z       
 * FrequencyBodyGyroscope.mean...X                   
 * FrequencyBodyGyroscope.mean...Y                 
 * FrequencyBodyGyroscope.mean...Z                  
 * FrequencyBodyGyroscope.meanFreq...X               
 * FrequencyBodyGyroscope.meanFreq...Y               
 * FrequencyBodyGyroscope.meanFreq...Z               
 * FrequencyBodyAccelerometerMagnitude.mean..        
 * FrequencyBodyAccelerometerMagnitude.meanFreq..    
 * FrequencyBodyAccelerometerJerkMagnitude.mean..    
 * FrequencyBodyAccelerometerJerkMagnitude.meanFreq..: 
 * FrequencyBodyGyroscopeMagnitude.mean..            
 * FrequencyBodyGyroscopeMagnitude.meanFreq..       
 * FrequencyBodyGyroscopeJerkMagnitude.mean..        
 * FrequencyBodyGyroscopeJerkMagnitude.meanFreq..    
 * Angle.TimeBodyAccelerometerMean.Gravity.          
 * Angle.TimeBodyAccelerometerJerkMean..GravityMean. 
 * Angle.TimeBodyGyroscopeMean.GravityMean.          
 * Angle.TimeBodyGyroscopeJerkMean.GravityMean.      
 * Angle.X.GravityMean.                              
 * Angle.Y.GravityMean.                              
 * Angle.Z.GravityMean.                              
 * TimeBodyAccelerometer.std...X                     
 * TimeBodyAccelerometer.std...Y                     
 * TimeBodyAccelerometer.std...Z                     
 * TimeGravityAccelerometer.std...X                  
 * TimeGravityAccelerometer.std...Y                  
 * TimeGravityAccelerometer.std...Z                  
 * TimeBodyAccelerometerJerk.std...X                 
 * TimeBodyAccelerometerJerk.std...Y                 
 * TimeBodyAccelerometerJerk.std...Z                 
 * TimeBodyGyroscope.std...X                         
 * TimeBodyGyroscope.std...Y                         
 * TimeBodyGyroscope.std...Z                         
 * TimeBodyGyroscopeJerk.std...X                     
 * TimeBodyGyroscopeJerk.std...Y                     
 * TimeBodyGyroscopeJerk.std...Z                     
 * TimeBodyAccelerometerMagnitude.std..              
 * TimeGravityAccelerometerMagnitude.std..           
 * TimeBodyAccelerometerJerkMagnitude.std..          
 * TimeBodyGyroscopeMagnitude.std..                  
 * TimeBodyGyroscopeJerkMagnitude.std..              
 * FrequencyBodyAccelerometer.std...X                
 * FrequencyBodyAccelerometer.std...Y                
 * FrequencyBodyAccelerometer.std...Z                
 * FrequencyBodyAccelerometerJerk.std...X            
 * FrequencyBodyAccelerometerJerk.std...Y            
 * FrequencyBodyAccelerometerJerk.std...Z            
 * FrequencyBodyGyroscope.std...X                    
 * FrequencyBodyGyroscope.std...Y                    
 * FrequencyBodyGyroscope.std...Z                    
 * FrequencyBodyAccelerometerMagnitude.std..         
 * FrequencyBodyAccelerometerJerkMagnitude.std..     
 * FrequencyBodyGyroscopeMagnitude.std..             
 * FrequencyBodyGyroscopeJerkMagnitude.std.. 

