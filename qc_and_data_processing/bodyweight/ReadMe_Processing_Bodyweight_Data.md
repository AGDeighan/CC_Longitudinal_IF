# Cleaning and processing of weekly bodyweights

The speed-cleaned weekly bodyweight data (SpeedCleaned_BW) were created from the processed animal data (animal_data_processed) and weekly bodyweight data (BodyWeight) that were downloaded from the bulk export function of Climb, our laboratory information management system. The speed-cleaned data were created in two steps. In the first step the script "01_format_bw.R" performs some formatting the prepare the data for cleaning. In the second step the script "02_quick_cleaning_of_bw.R" takes the formatted data and peforms the actuall data cleaning. Both scripts are heavily commented so for further details please refer to them. The data is said to be "speed-cleaned" for two reasons. First, we have not attempted to fix records with issues, rather we have just notated and removed them. Second, we have taken a rather agressive approach to the data cleaning. A more subtle and nuanced approach may yeild more and better data.

The speed-cleaned weekly bodyweight data is a CSV file that contains 66,904 rows (one for each mouse at each weighing) and 11y columns:
 - MouseID: the name of the mouse
 - OutputRowID: the unique record identifier used by Climb
 - BWDay_Test: the day of the week on which the mouse's bodyweight should be recorded
 - Tech: the name of the animal care technician that performed the weighing (missing for many records)
 - BatchComments: any comments the animal care technician had about the test batch (missing if no comments were made)
 - DateDue: the date on which the test was scheduled to be performed
 - DateCollect: the date on which the test was performed
 - DateComplete: this is a new date field, I'm not sure what it signifies, it is missing for many records
 - AgeInDays: the age of the mouse at the time of thest, this was calculated from the date of birth recorded in the animal data, and the collection date (DateCollect)
 - BW: the measured BW (grams)
 - BW_LOESS: the loess-fitted BW (grams)

<br>
<br>

## Cleaning

The data cleaning was carried out in the following order and records with errors with errors were removed as they were identified. Thus, a record that has an error searched for in a later step would not be identified if the record was already removed due to an error identified in an earlier step. In total 1,956 records out of 68,860 (~2.8%) were removed in the data cleaning.

<br>

### 1) Missing mouse ID and bodyweight: 143 removed

143 records that were missing both their mouse ID and bodyweight were removed, they are saved in the CSV file: issues/records_missing_mouseid_and_bw.csv

<br>
<br>

### 2) Missing mouse ID only: 44 removed

44 records that were missing their mouse ID but had a recorded bodyweight were removed, they are saved in the CSV file: issues/records_missing_mouseid_not_bw.csv

<br>
<br>

### 3) Missing bodyweight only; 19 removed

19 records that were missing their bodyweight but had a mouse ID were removed, they are saved in the CSV file: issues/records_missing_bw_not_mouseid.csv

<br>
<br>

### 4) Complete duplicates: 7 removed

There were 8 records involved in complete duplications (same mouse, same date, same bodyweight). All were identical (i.e. an octuplet), 7 of the records were removed. All 8 of the records are saved in the file: issues/full_duplicates.csv

<br>
<br>

### 5) Multiple bodyweights for one mouse at one date: 178 removed

There are 178 records (89 duplications, all are pairs, no triplicates or higher level duplications) involved in duplications in which the collection date and mouse ID are identical, but the body weights are not. For many of these, the bodyweights are very similar (perhaps the mice were weighed twice?), but for some they are quite different. The due dates are also identical and for all of these duplicates that have completion dates, the completion dates are also identical, so that doesn't help. They don't all come from one date, though there is one date (2018-11-14) that has a large amount of duplicates (12 recods, 6 duplicates). I'm not sure what to do here. It seems reasonable that the pairs with two bodyweights that are not very different represent mice that were weighed twice and for those we could simply take the mean. However, there are numerous pairs for which the bodyweights differ. Furthermore, how do we define what pairs are have bodyweights that are "close" enough? Often, different mice within the same cage have similar bodyweights. Until we decide what to do, I am dropping all these records involved in duplications. These records are saved in the file: issues/same_mouse_same_date_different_bw.csv

<br>
<br>

### 6) Duplicate output row IDs: 34 removed

The output row ID should be unique to each record. However there are 17 duplications (34 records in total) of output row IDs (after removing the duplicates mentioned above). For these records, the BWs and collection dates appear to be identical, but no the mouse ID. We need to figure out what caused these duplications (row IDs should be unique), and, if possible, determine to which mice the data correctly belong. These records are saved in the file: issues/duplicated_outputrowid.csv

<br>
<br>

### 7) Bodyweights recorded after death: 50 removed

There are fifty records with collection dates after the death date. Some of these may represent mice for which the death date is wrong, or for which there is a typo in the collection date. Others may be swaps. We should try and identify what is going on for each of these records in case it is a result of some larger underlying issue.

<br>
<br>

### 8) Impossibly high bodyweights: 6 temporarily corrected

There are 6 records with impossible high bodyweights (> 200 grams, several > 1000), other than these 6 bodyweights the maximum bodyweight is 65 grams, so they must be errors. By comparing the erroneous bodyweights to the recorded bodyweights for the same mice at preceeding and following weeks, we were able to determine that they were clear fixable typos and they were all corrected in the output speed-cleaned data, but have not yet been corrected in Climb. The erroneous records are saved in the file: issues/impossibly_high_bw.csv

<br>
<br>

### 9) Impossibly low bodyweights: 4 removed, 1 temporarily corrected

There are 5 bodyweights with impossible low values (< 5 grams). By comparing the erroneous bodyweights to the recorded bodyweights for the same mice at preceeding and following weeks, we were able to determine that one of the records is an obvious and fixable typo. The typo is corrected in the speed-cleaned data but not the data in Climb. The erroneous records are saved in the file: issues/impossibly_low_bw.csv

<br>
<br>

### 10) Large fluctuations in bodyweight: 1477 removed

Bodyweights that differed from the preceeding and following weekly bodyweight for the same mouse *in the same direction* by more than 10% were flagged as large fluctuations. These could represent swaps or bad scale calibration. In the speed-cleanded data, flagged bodyweights have been removed. It may be worth investigating these flagged bodyweights to determine if they are fixable swaps; however, it may be an inefficient use of time. The flagged bodyweights are saved in the file: issues/flagged_bw.csv

<br>
<br>


## Smoothing

In addition to the measured bodyweights, the weekly bodyweight data contains LOESS-fitted bodyweights. These smoothed bodyweights may be more useful for downstream analysis and quality control of other data types (many of the other assays include a bodyweight measure so by comparing the bodyweight reported in the assay to the weekly bodyweights we can identify sample swaps) because they may provide a more stable representation of the bodyweights of the mice. One note of caution, do not use the pre-diet LOESS-fitted bodyweights believing that they are uneffected by diet. Due to the nature of LOESS modeling, the LOESS-fitted bodyweights reported before the start of the diet intervention are effected by the measured bodyweights *after* the intervention and are thus potentially affected by diet.

For details on the smoothing please see the R-script.

<br>
<br>