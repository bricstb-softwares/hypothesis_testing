Análise de dados criminais - Teste de Hipótese
================
Otto Tavares
2023-03-21

## Introdução

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.0     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(purrr)
library(dlookr)
```

    ## Registered S3 methods overwritten by 'dlookr':
    ##   method          from  
    ##   plot.transform  scales
    ##   print.transform scales
    ## 
    ## Attaching package: 'dlookr'
    ## 
    ## The following object is masked from 'package:tidyr':
    ## 
    ##     extract
    ## 
    ## The following object is masked from 'package:base':
    ## 
    ##     transform

``` r
library(summarytools)
```

    ## 
    ## Attaching package: 'summarytools'
    ## 
    ## The following object is masked from 'package:tibble':
    ## 
    ##     view

``` r
library(readxl)
library(knitr)
library(data.table)
```

    ## 
    ## Attaching package: 'data.table'
    ## 
    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     hour, isoweek, mday, minute, month, quarter, second, wday, week,
    ##     yday, year
    ## 
    ## The following objects are masked from 'package:dplyr':
    ## 
    ##     between, first, last
    ## 
    ## The following object is masked from 'package:purrr':
    ## 
    ##     transpose

``` r
library(ggpubr)
library(corrplot)
```

    ## corrplot 0.92 loaded

``` r
library(rcompanion)
```

    ## New names:
    ## Rows: 450 Columns: 47
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (8): train_tag, op_name, file_name, roc, roc_val, roc_op, roc_test, inf... dbl
    ## (38): ...1, Unnamed: 0, test, sort, max_sp, auc, sens, spec, threshold, ... lgl
    ## (1): min_spec_sens_reached
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `` -> `...1`

    ## Data Frame Summary  
    ## validation  
    ## Dimensions: 90 x 47  
    ## Duplicates: 0  
    ## 
    ## -----------------------------------------------------------------------------------------------------------------------------
    ## No   Variable                Stats / Values                  Freqs (% of Valid)   Graph                  Valid      Missing  
    ## ---- ----------------------- ------------------------------- -------------------- ---------------------- ---------- ---------
    ## 1    ...1                    Mean (sd) : 44.5 (26.1)         90 distinct values   : . . . . . . .        90         0        
    ##      [numeric]               min < med < max:                                     : : : : : : : : :      (100.0%)   (0.0%)   
    ##                              0 < 44.5 < 89                                        : : : : : : : : :                          
    ##                              IQR (CV) : 44.5 (0.6)                                : : : : : : : : :                          
    ##                                                                                   : : : : : : : : :                          
    ## 
    ## 2    Unnamed: 0              Mean (sd) : 44.5 (26.1)         90 distinct values   : . . . . . . .        90         0        
    ##      [numeric]               min < med < max:                                     : : : : : : : : :      (100.0%)   (0.0%)   
    ##                              0 < 44.5 < 89                                        : : : : : : : : :                          
    ##                              IQR (CV) : 44.5 (0.6)                                : : : : : : : : :                          
    ##                                                                                   : : : : : : : : :                          
    ## 
    ## 3    train_tag               1. base.sh-sc.e                 90 (100.0%)          IIIIIIIIIIIIIIIIIIII   90         0        
    ##      [character]                                                                                         (100.0%)   (0.0%)   
    ## 
    ## 4    op_name                 1. loose                        90 (100.0%)          IIIIIIIIIIIIIIIIIIII   90         0        
    ##      [character]                                                                                         (100.0%)   (0.0%)   
    ## 
    ## 5    test                    Mean (sd) : 4.5 (2.9)           0 : 9 (10.0%)        II                     90         0        
    ##      [numeric]               min < med < max:                1 : 9 (10.0%)        II                     (100.0%)   (0.0%)   
    ##                              0 < 4.5 < 9                     2 : 9 (10.0%)        II                                         
    ##                              IQR (CV) : 5 (0.6)              3 : 9 (10.0%)        II                                         
    ##                                                              4 : 9 (10.0%)        II                                         
    ##                                                              5 : 9 (10.0%)        II                                         
    ##                                                              6 : 9 (10.0%)        II                                         
    ##                                                              7 : 9 (10.0%)        II                                         
    ##                                                              8 : 9 (10.0%)        II                                         
    ##                                                              9 : 9 (10.0%)        II                                         
    ## 
    ## 6    sort                    Mean (sd) : 4 (2.6)             0 : 10 (11.1%)       II                     90         0        
    ##      [numeric]               min < med < max:                1 : 10 (11.1%)       II                     (100.0%)   (0.0%)   
    ##                              0 < 4 < 8                       2 : 10 (11.1%)       II                                         
    ##                              IQR (CV) : 4 (0.6)              3 : 10 (11.1%)       II                                         
    ##                                                              4 : 10 (11.1%)       II                                         
    ##                                                              5 : 10 (11.1%)       II                                         
    ##                                                              6 : 10 (11.1%)       II                                         
    ##                                                              7 : 10 (11.1%)       II                                         
    ##                                                              8 : 10 (11.1%)       II                                         
    ## 
    ## 7    file_name               1. /mnt/brics_data/models/v0     1 ( 1.1%)                                  90         0        
    ##      [character]             2. /mnt/brics_data/models/v0     1 ( 1.1%)                                  (100.0%)   (0.0%)   
    ##                              3. /mnt/brics_data/models/v0     1 ( 1.1%)                                                      
    ##                              4. /mnt/brics_data/models/v0     1 ( 1.1%)                                                      
    ##                              5. /mnt/brics_data/models/v0     1 ( 1.1%)                                                      
    ##                              6. /mnt/brics_data/models/v0     1 ( 1.1%)                                                      
    ##                              7. /mnt/brics_data/models/v0     1 ( 1.1%)                                                      
    ##                              8. /mnt/brics_data/models/v0     1 ( 1.1%)                                                      
    ##                              9. /mnt/brics_data/models/v0     1 ( 1.1%)                                                      
    ##                              10. /mnt/brics_data/models/v0    1 ( 1.1%)                                                      
    ##                              [ 80 others ]                   80 (88.9%)           IIIIIIIIIIIIIIIII                          
    ## 
    ## 8    max_sp                  Mean (sd) : 0.9 (0)             90 distinct values           :              90         0        
    ##      [numeric]               min < med < max:                                           : : :            (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 1                                              : : :                                
    ##                              IQR (CV) : 0 (0)                                         . : : : :                              
    ##                                                                                       : : : : : . .                          
    ## 
    ## 9    auc                     Mean (sd) : 1 (0)               89 distinct values                 :        90         0        
    ##      [numeric]               min < med < max:                                             .   . :        (100.0%)   (0.0%)   
    ##                              0.9 < 1 < 1                                                  : : : :                            
    ##                              IQR (CV) : 0 (0)                                           . : : : : .                          
    ##                                                                                     . . : : : : : : :                        
    ## 
    ## 10   sens                    Mean (sd) : 0.9 (0)             60 distinct values             :            90         0        
    ##      [numeric]               min < med < max:                                               :            (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 1                                              : : : :                              
    ##                              IQR (CV) : 0.1 (0)                                     :   : : : : : .                          
    ##                                                                                   : : : : : : : : : :                        
    ## 
    ## 11   spec                    Mean (sd) : 0.9 (0)             61 distinct values         :                90         0        
    ##      [numeric]               min < med < max:                                           : . .            (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 1                                              : : :                                
    ##                              IQR (CV) : 0 (0)                                           : : : :                              
    ##                                                                                     . . : : : : .                            
    ## 
    ## 12   threshold               Mean (sd) : 0.4 (0.1)           90 distinct values           : .            90         0        
    ##      [numeric]               min < med < max:                                           : : :            (100.0%)   (0.0%)   
    ##                              0.2 < 0.4 < 0.7                                            : : : :                              
    ##                              IQR (CV) : 0.1 (0.2)                                       : : : : :                            
    ##                                                                                   .   : : : : : : : :                        
    ## 
    ## 13   roc                     1. {'fpr': array([0.             1 ( 1.1%)                                  90         0        
    ##      [character]             2. {'fpr': array([0.             1 ( 1.1%)                                  (100.0%)   (0.0%)   
    ##                              3. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              4. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              5. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              6. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              7. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              8. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              9. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              10. {'fpr': array([0.            1 ( 1.1%)                                                      
    ##                              [ 80 others ]                   80 (88.9%)           IIIIIIIIIIIIIIIII                          
    ## 
    ## 14   roc_val                 1. {'fpr': array([0.             1 ( 1.1%)                                  90         0        
    ##      [character]             2. {'fpr': array([0.             1 ( 1.1%)                                  (100.0%)   (0.0%)   
    ##                              3. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              4. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              5. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              6. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              7. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              8. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              9. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              10. {'fpr': array([0.            1 ( 1.1%)                                                      
    ##                              [ 80 others ]                   80 (88.9%)           IIIIIIIIIIIIIIIII                          
    ## 
    ## 15   roc_op                  1. {'fpr': array([0.             1 ( 1.1%)                                  90         0        
    ##      [character]             2. {'fpr': array([0.             1 ( 1.1%)                                  (100.0%)   (0.0%)   
    ##                              3. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              4. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              5. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              6. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              7. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              8. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              9. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              10. {'fpr': array([0.            1 ( 1.1%)                                                      
    ##                              [ 80 others ]                   80 (88.9%)           IIIIIIIIIIIIIIIII                          
    ## 
    ## 16   roc_test                1. {'fpr': array([0.             1 ( 1.1%)                                  90         0        
    ##      [character]             2. {'fpr': array([0.             1 ( 1.1%)                                  (100.0%)   (0.0%)   
    ##                              3. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              4. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              5. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              6. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              7. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              8. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              9. {'fpr': array([0.             1 ( 1.1%)                                                      
    ##                              10. {'fpr': array([0.            1 ( 1.1%)                                                      
    ##                              [ 80 others ]                   80 (88.9%)           IIIIIIIIIIIIIIIII                          
    ## 
    ## 17   min_spec_sens_reached   1. FALSE                        46 (51.1%)           IIIIIIIIII             90         0        
    ##      [logical]               2. TRUE                         44 (48.9%)           IIIIIIIII              (100.0%)   (0.0%)   
    ## 
    ## 18   max_sp_val              Mean (sd) : 0.9 (0)             69 distinct values         : .   :          90         0        
    ##      [numeric]               min < med < max:                                       . : : :   :          (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 0.9                                        : : : : : :                              
    ##                              IQR (CV) : 0.1 (0)                                     : : : : : : :                            
    ##                                                                                   : : : : : : : :                            
    ## 
    ## 19   auc_val                 Mean (sd) : 0.9 (0)             81 distinct values             :            90         0        
    ##      [numeric]               min < med < max:                                         .     :            (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 1                                          . :   : :                                
    ##                              IQR (CV) : 0.1 (0)                                     : : : : :                                
    ##                                                                                   . : : : : : .                              
    ## 
    ## 20   sens_val                Mean (sd) : 0.8 (0.1)           18 distinct values         : :              90         0        
    ##      [numeric]               min < med < max:                                           : : . :          (100.0%)   (0.0%)   
    ##                              0.6 < 0.8 < 1                                              : : : :                              
    ##                              IQR (CV) : 0.1 (0.1)                                       : : : : .                            
    ##                                                                                   : : . : : : : :                            
    ## 
    ## 21   spec_val                Mean (sd) : 0.8 (0.1)           28 distinct values         :                90         0        
    ##      [numeric]               min < med < max:                                           :                (100.0%)   (0.0%)   
    ##                              0.7 < 0.9 < 1                                              :                                    
    ##                              IQR (CV) : 0.1 (0.1)                                   : : : .                                  
    ##                                                                                   . : : : :                                  
    ## 
    ## 22   max_sp_test             Mean (sd) : 0.9 (0)             71 distinct values             :            90         0        
    ##      [numeric]               min < med < max:                                             : :   . . .    (100.0%)   (0.0%)   
    ##                              0.7 < 0.8 < 0.9                                            : : :   : : :                        
    ##                              IQR (CV) : 0.1 (0.1)                                     : : : : : : : :                        
    ##                                                                                   . : : : : : : : : :                        
    ## 
    ## 23   auc_test                Mean (sd) : 0.9 (0)             84 distinct values           :   :   .      90         0        
    ##      [numeric]               min < med < max:                                             :   :   :      (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 1                                                :   : : : :                        
    ##                              IQR (CV) : 0.1 (0)                                         . : : : : : :                        
    ##                                                                                   . . . : : : : : : :                        
    ## 
    ## 24   sens_test               Mean (sd) : 0.8 (0.1)           20 distinct values             . . :        90         0        
    ##      [numeric]               min < med < max:                                               : : :   :    (100.0%)   (0.0%)   
    ##                              0.5 < 0.8 < 1                                                . : : :   :                        
    ##                              IQR (CV) : 0.1 (0.1)                                   .     : : : :   :                        
    ##                                                                                   . : . . : : : : : :                        
    ## 
    ## 25   spec_test               Mean (sd) : 0.8 (0.1)           29 distinct values             :            90         0        
    ##      [numeric]               min < med < max:                                               :            (100.0%)   (0.0%)   
    ##                              0.6 < 0.9 < 1                                                : :                                
    ##                              IQR (CV) : 0.1 (0.1)                                       : : : :                              
    ##                                                                                       . : : : :                              
    ## 
    ## 26   max_sp_op               Mean (sd) : 0.9 (0)             88 distinct values         : .              90         0        
    ##      [numeric]               min < med < max:                                           : : .            (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 1                                              : : :                                
    ##                              IQR (CV) : 0 (0)                                         : : : :                                
    ##                                                                                       : : : : :                              
    ## 
    ## 27   auc_op                  Mean (sd) : 1 (0)               89 distinct values                 :        90         0        
    ##      [numeric]               min < med < max:                                             . .   :        (100.0%)   (0.0%)   
    ##                              0.9 < 1 < 1                                                  : : . :                            
    ##                              IQR (CV) : 0 (0)                                           . : : : : :                          
    ##                                                                                   . . . : : : : : : .                        
    ## 
    ## 28   sens_op                 Mean (sd) : 0.9 (0)             64 distinct values           :              90         0        
    ##      [numeric]               min < med < max:                                             : .            (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 1                                          .     : : .                              
    ##                              IQR (CV) : 0.1 (0)                                     : . : : : :   .                          
    ##                                                                                   : : : : : : : : : .                        
    ## 
    ## 29   spec_op                 Mean (sd) : 0.9 (0)             63 distinct values         :                90         0        
    ##      [numeric]               min < med < max:                                           : . :            (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 1                                              : : : .                              
    ##                              IQR (CV) : 0 (0)                                         . : : : :                              
    ##                                                                                     . : : : : :                              
    ## 
    ## 30   sp_index                Mean (sd) : 0.9 (0)             88 distinct values             . :          90         0        
    ##      [numeric]               min < med < max:                                             : : :          (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 1                                                : : : .                            
    ##                              IQR (CV) : 0 (0)                                           : : : : :                            
    ##                                                                                   . . . : : : : : .                          
    ## 
    ## 31   sens_at                 Mean (sd) : 0.9 (0)             14 distinct values   :                      90         0        
    ##      [numeric]               min < med < max:                                     :                      (100.0%)   (0.0%)   
    ##                              0.9 < 0.9 < 0.9                                      :                                          
    ##                              IQR (CV) : 0 (0)                                     : :                                        
    ##                                                                                   : : :                                      
    ## 
    ## 32   spec_at                 Mean (sd) : 0.9 (0.1)           77 distinct values         : :              90         0        
    ##      [numeric]               min < med < max:                                           : : .            (100.0%)   (0.0%)   
    ##                              0.7 < 0.9 < 1                                              : : :                                
    ##                              IQR (CV) : 0.1 (0.1)                                     . : : :                                
    ##                                                                                   .   : : : : :                              
    ## 
    ## 33   acc_at                  Mean (sd) : 0.9 (0)             75 distinct values             . :          90         0        
    ##      [numeric]               min < med < max:                                             . : :          (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 1                                                : : : .                            
    ##                              IQR (CV) : 0 (0)                                           : : : : :                            
    ##                                                                                   . . . : : : : : :                          
    ## 
    ## 34   threshold_at            Mean (sd) : 0.4 (0.1)           90 distinct values           :              90         0        
    ##      [numeric]               min < med < max:                                             :              (100.0%)   (0.0%)   
    ##                              0.2 < 0.4 < 0.7                                          .   : : :                              
    ##                              IQR (CV) : 0.1 (0.3)                                   . : : : : :                              
    ##                                                                                   . : : : : : : . . :                        
    ## 
    ## 35   sp_index_val            Mean (sd) : 0.8 (0)             69 distinct values                   :      90         0        
    ##      [numeric]               min < med < max:                                             : :     :      (100.0%)   (0.0%)   
    ##                              0.7 < 0.8 < 0.9                                              : : . . : .                        
    ##                              IQR (CV) : 0.1 (0.1)                                   :   . : : : : : :                        
    ##                                                                                   : : : : : : : : : :                        
    ## 
    ## 36   sens_at_val             Mean (sd) : 0.8 (0.1)           19 distinct values             :   :        90         0        
    ##      [numeric]               min < med < max:                                             : :   :        (100.0%)   (0.0%)   
    ##                              0.6 < 0.8 < 1                                                : : : :                            
    ##                              IQR (CV) : 0.1 (0.1)                                         : : : : .                          
    ##                                                                                     . . . : : : : :                          
    ## 
    ## 37   spec_at_val             Mean (sd) : 0.8 (0.1)           31 distinct values                   :      90         0        
    ##      [numeric]               min < med < max:                                                   . :      (100.0%)   (0.0%)   
    ##                              0.5 < 0.9 < 1                                                      : :                          
    ##                              IQR (CV) : 0.1 (0.1)                                         : . : : : :                        
    ##                                                                                   .   .   : : : : : :                        
    ## 
    ## 38   threshold_at_val        Mean (sd) : 0.4 (0.1)           90 distinct values           :              90         0        
    ##      [numeric]               min < med < max:                                             :              (100.0%)   (0.0%)   
    ##                              0.2 < 0.4 < 0.7                                          .   : : :                              
    ##                              IQR (CV) : 0.1 (0.3)                                   . : : : : :                              
    ##                                                                                   . : : : : : : . . :                        
    ## 
    ## 39   sp_index_test           Mean (sd) : 0.8 (0.1)           79 distinct values         :                90         0        
    ##      [numeric]               min < med < max:                                         . :                (100.0%)   (0.0%)   
    ##                              0.7 < 0.8 < 0.9                                          : : :                                  
    ##                              IQR (CV) : 0.1 (0.1)                                     : : : .                                
    ##                                                                                   . : : : : :                                
    ## 
    ## 40   sens_at_test            Mean (sd) : 0.8 (0.1)           23 distinct values               :          90         0        
    ##      [numeric]               min < med < max:                                                 :          (100.0%)   (0.0%)   
    ##                              0.5 < 0.8 < 1                                                    :   : .                        
    ##                              IQR (CV) : 0.1 (0.1)                                             : . : :                        
    ##                                                                                   .   . : . : : : : :                        
    ## 
    ## 41   spec_at_test            Mean (sd) : 0.8 (0.1)           39 distinct values               :          90         0        
    ##      [numeric]               min < med < max:                                               : :          (100.0%)   (0.0%)   
    ##                              0.6 < 0.8 < 1                                                : : :                              
    ##                              IQR (CV) : 0.1 (0.1)                                       . : : : :                            
    ##                                                                                     : : : : : : :                            
    ## 
    ## 42   threshold_at_test       Mean (sd) : 0.4 (0.1)           90 distinct values           :              90         0        
    ##      [numeric]               min < med < max:                                             :              (100.0%)   (0.0%)   
    ##                              0.2 < 0.4 < 0.7                                          .   : : :                              
    ##                              IQR (CV) : 0.1 (0.3)                                   . : : : : :                              
    ##                                                                                   . : : : : : : . . :                        
    ## 
    ## 43   sp_index_op             Mean (sd) : 0.9 (0)             88 distinct values               :          90         0        
    ##      [numeric]               min < med < max:                                             .   :          (100.0%)   (0.0%)   
    ##                              0.8 < 0.9 < 1                                                : : :                              
    ##                              IQR (CV) : 0 (0)                                           . : : : :                            
    ##                                                                                   .   . : : : : : .                          
    ## 
    ## 44   sens_at_op              Mean (sd) : 0.9 (0)             25 distinct values         . :              90         0        
    ##      [numeric]               min < med < max:                                           : :              (100.0%)   (0.0%)   
    ##                              0.9 < 0.9 < 0.9                                            : :                                  
    ##                              IQR (CV) : 0 (0)                                           : : :                                
    ##                                                                                     . : : : :                                
    ## 
    ## 45   spec_at_op              Mean (sd) : 0.9 (0.1)           73 distinct values           . :            90         0        
    ##      [numeric]               min < med < max:                                             : :            (100.0%)   (0.0%)   
    ##                              0.6 < 0.9 < 1                                                : : :                              
    ##                              IQR (CV) : 0.1 (0.1)                                       . : : :                              
    ##                                                                                     . . : : : : :                            
    ## 
    ## 46   threshold_at_op         Mean (sd) : 0.4 (0.1)           90 distinct values           :              90         0        
    ##      [numeric]               min < med < max:                                             :              (100.0%)   (0.0%)   
    ##                              0.2 < 0.4 < 0.7                                          .   : : :                              
    ##                              IQR (CV) : 0.1 (0.3)                                   . : : : : :                              
    ##                                                                                   . : : : : : : . . :                        
    ## 
    ## 47   inference               1. OrderedDict([('russia', O     1 ( 1.1%)                                  90         0        
    ##      [character]             2. OrderedDict([('russia', O     1 ( 1.1%)                                  (100.0%)   (0.0%)   
    ##                              3. OrderedDict([('russia', O     1 ( 1.1%)                                                      
    ##                              4. OrderedDict([('russia', O     1 ( 1.1%)                                                      
    ##                              5. OrderedDict([('russia', O     1 ( 1.1%)                                                      
    ##                              6. OrderedDict([('russia', O     1 ( 1.1%)                                                      
    ##                              7. OrderedDict([('russia', O     1 ( 1.1%)                                                      
    ##                              8. OrderedDict([('russia', O     1 ( 1.1%)                                                      
    ##                              9. OrderedDict([('russia', O     1 ( 1.1%)                                                      
    ##                              10. OrderedDict([('russia', O    1 ( 1.1%)                                                      
    ##                              [ 80 others ]                   80 (88.9%)           IIIIIIIIIIIIIIIII                          
    ## -----------------------------------------------------------------------------------------------------------------------------
