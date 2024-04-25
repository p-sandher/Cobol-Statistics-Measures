       *>  Author: Puneet Sandher
       *>  Course: CIS3190
       *>  This program calculates the mean, standard deviation, geometric mean, harmonic mean, and root mean square.
       *>  The program will take an input file from the user to calculate theese statistics. 
       
       identification division.
       
       program-id. statmeasure.
       
       environment division.
       
       input-output section.
       
       *>  The input file name will be named dynamically, as the user will enter it in.
       file-control.
       select input-file assign to dynamic ws-fname
           organization is line sequential
           file status is fs. 
       
       data division.
       
       *>  File section outlines information about the input file with data    
       file section.
       fd input-file.
       01 input-record.
           02 num              pic s9(6)v9(2).
           02 data-filler      pic x(72).
       
       working-storage section.
       77  sum-of-nums-sqr     pic 9(14)v9(2) value 0.
       77  sum-of-nums         pic s9(10)v9(2) value 0.
       77  geometric-mean      pic s9(10)v9(2) value 0.
       77  harmonic-mean       pic s9(10)v9(2) value 0.
       77  sum-of-nums-recip   pic s9(10)v9(6) value 0.
       77  root-mean-sqr       pic s9(16)v9(2) value 0.
       77  num-array-len       pic s9(6) value 1.
       77  mean                pic s9(6)v9(2).
       77  i                   pic s9(9).
       77  ws-fname            pic x(300).
       77  fs                  pic x(2).
       77  feof                pic a(1).
       
       01  input-value-record.
           02 data-point             pic s9(6)v9(2) value 0.
           02 data-filler      pic x(72).
       01  output-summary-title-line.
           02 filler           pic x(20) value
                               "Statistical Summary".
       01  output-central-tendency-title-line.
           02 filler           pic x(29) value
                               "Measures of Central Tendency".
       01  output-dispersion-title-line.
           02 filler           pic x(23) value
                               "Measures of Dispersion".                  
       01  program-title.
           02 filler           pic x(131) value
                               "Statmeasure Program. Calculates the mean, standard deviation, geometric mean, harmonic mean, and root mean square of a set of data.".
       01  file-input-message.
           02 filler           pic x(38) value
                               "Enter a file name with float numbers:".
       01  file-success-message.
           02 filler           pic x(39) value
                               "The inputted file successfully opened.".
       01  file-error-message.
           02 filler           pic x(56) value
                               "Error. This file could not be opened. File status code: ".
       01  output-underline.
           02 filler           pic x(28) value
                               "----------------------------".
       01  output-data-title-line.
           02 filler           pic x(10) value spaces.
           02 filler           pic x(11) value 
                               "Data Values".
       01  output-data-line.
           02 filler           pic x(10) value spaces.
           02 data-value       pic -(4)9.9(2).
       01  output-results-mean.
           02 filler           pic x(9) value 
                               "Mean".
           02 out-mean         pic -(14)9.9(2).
       01  output-results-std-dev.
           02 filler           pic x(19) value 
                               "Standard Deviation".
           02 std-deviation    pic -(4)9.9(2).
       01  output-results-geometric.
           02 filler           pic x(20) value 
                               "Geometric Mean".
           02 out-geo-mean     pic -(3)9.9(2).
       01  output-results-harmonic.
           02 filler           pic x(20) value 
                               "Harmonic Mean".
           02 out-harmonic-mean    pic -(3)9.9(2).
       01  output-results-root-mean-square.
           02 filler           pic x(20) value 
                               "Root Mean Square".
           02 out-root-mean-sqr    pic -(3)9.9(2).
       01  arr.
           02 num-array        pic s9(6)v9(2) occurs 999999 times.
       
       procedure division.
           display program-title.
           
           *> Get a valid data file from the user, re-prompt user if its invalid. 
           perform until fs = "00"
              display file-input-message
              accept ws-fname
              open input input-file 
       
              evaluate fs 
                  when "00"
                      display file-success-message
                  when other 
                      display file-error-message, fs
              end-evaluate
       
           end-perform.
       
       *>  proc-body will excute call the paragraphs to calculate the statistics
       proc-body.
           display output-underline.
           display output-data-title-line.
           display output-underline.
             
           *> Iterate through the file and store each number in an array
           perform until feof = 'Y'
               read input-file into input-value-record
               at end
                   move 'Y' to feof
               not at end
                   perform process-record
               end-read
           end-perform.
       
           compute num-array-len = num-array-len - 1.
  
           perform print-nums.

           display output-underline.
           display output-summary-title-line.
           display output-underline.

           *> Call paragraphs to calculate each statistic
           perform calculate-mean.
           perform calculate-standard-deviation.
           display output-underline.
           display output-central-tendency-title-line.
           display output-underline. 
           perform calculate-geometric-mean.
           perform calculate-harmonic-mean.
           display output-underline.
           display output-dispersion-title-line.
           display output-underline. 
           perform calculate-root-mean-square. 
           display output-underline. 
       
           perform end-of-job.
       
       *>  calculate-mean is a paragraph that calculates the mean of the dataset
       calculate-mean.
       
           *> calculate the sum of all the numbers in the array
           perform varying i from 1 by 1 until i > num-array-len
               compute sum-of-nums = sum-of-nums + num-array(i)
           end-perform.
       
           *> calculate the mean 
           compute mean rounded = sum-of-nums / num-array-len. 
           move mean to out-mean.
           display output-results-mean.
       
       *>  calculate-standard-deviation is a paragraph that calculates the standard deviation of the dataset
       calculate-standard-deviation. 
           
           *> calculate the sum of all the numbers squared in the array
           perform varying i from 1 by 1 until i > num-array-len
               compute sum-of-nums-sqr = sum-of-nums-sqr + (num-array(i) - mean) ** 2
           end-perform.
           
           *> calculate the standard deviaation
           compute std-deviation rounded = (sum-of-nums-sqr / num-array-len) ** 0.5.
           display output-results-std-dev.
       
       *>  process record is a paragraph that stores the data in the input file to num-array
       process-record. 
           move num to num-array(num-array-len).
           compute num-array-len = num-array-len + 1.
       
       *>  calculate-geometric-mean is a paragraph that calculates the geometric mean of the dataset
       calculate-geometric-mean.

           *> calculate the sum of the logarithm for each number
           perform varying i from 1 by 1 until i > num-array-len
               compute geometric-mean  rounded = geometric-mean + function log(num-array(i))
           end-perform.
          
           *> calculate the anti-log of the sum
           compute geometric-mean   = function exp(geometric-mean   / num-array-len).
           move geometric-mean to out-geo-mean.
           display output-results-geometric.
       
       *>  calculate-harmonic-mean is a paragraph that calculates the harmonic mean of the dataset
       calculate-harmonic-mean.

           *> calculate the sum of the reciprocal of each number
           perform varying i from 1 by 1 until i > num-array-len
               compute sum-of-nums-recip rounded = sum-of-nums-recip + (1/num-array(i))
           end-perform.
           *> calculate the harmonic mean 
           compute harmonic-mean rounded = num-array-len / sum-of-nums-recip.
           move harmonic-mean to out-harmonic-mean.
           display output-results-harmonic.
       
       *>  calculate-root-mean-square is a paragraph that calculates the root mean square of the dataset
       calculate-root-mean-square. 
       
           *> calculate the square of each number 
           perform varying i from 1 by 1 until i > num-array-len
               compute root-mean-sqr rounded = root-mean-sqr + (num-array(i) ** 2)
           end-perform.
       
           *> calculate the root mean squarre
           compute root-mean-sqr rounded = function sqrt((root-mean-sqr/num-array-len)).
           move root-mean-sqr to out-root-mean-sqr.
           display output-results-root-mean-square.
       
       *>  end-of-job is a paragraph to close files and end the program
       end-of-job.
           close input-file.
           stop run.

       *>  print-nums displays all the data values from the input file    
       print-nums.
           perform varying i from 1 by 1 until i > num-array-len
               move num-array(i) to data-value
               display output-data-line 
           end-perform.
