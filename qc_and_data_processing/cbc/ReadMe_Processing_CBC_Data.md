# Cleaning and processing of complete blood count (CBC) data

The cleaned CBC data (Cleaned_CBC) were created from the processed animal data (animal_data_processed), speed-cleaned weekly bodyweight data (SpeedCleaned_BW), and the CBC data (Blood3) that were downloaded from the bulk export function of Climb, our laboratory information management system. The cleaned data were produced in two steps. In the first step, the script "01_format_cbc.R" performs some formatting the prepare the data for cleaning. In the second step the script "02_cleaning_cbc.R" takes the formatted data and peforms the actuall data cleaning. Both scripts are heavily commented so for further details please refer to them. The data cleaning issues addressed here have not yet been pushed back to climb

The speed-cleaned weekly bodyweight data is a CSV file that contains 1071 rows and 48 columns:
 - MouseID: the name of the mouse
 - OutputRowID: the unique record identifier used by Climb
 - Timepoint: the timepoint of the assay (year 1, 2, or 3)
 - AgeInDays: the age of the mouse in days (calculated from the collection date and the date of birth)
 - DateCollect: the date the CBC was performed 
 - DaysSinceFast: the number of days between when the mouse was last fasted and the time of the assay (zeroes indicate done during time of fast or the day the food was returned [mice are generally fed between 12-2 pm and phenotypes between 8 am and 12pm], NA for ad libitum mice because they never experience a fast)
 - Tech: the technician who performed the blood draw  
 - Comments: batch comments refer to an entire assay batch, sample comments refer to an individual sample within the batch
 - BW_Test: the bodyweight (grams) of the mouse recorded during the assay 
 - BW_Weekly: the bodyweight (grams) of the mouse at the time of the assay estimated (mean of preceeding and following LOESS-fitted bodyweights) from the weekly BW data. We recommend using the BW_Weekly rather than BW_Test for analysis because they should provide the most stable representation of the animal's true weight
 - Anesthesia: type of anesthesia eye drops used during the blood draw
 - NumClumps: the number of platelet clumps counted in the sample. This is a quality control metric, platelet clumping affects eosinophil count (increased because clumps may be counted as eosinophils), platelet counts (decreased, platelets in clumps are not counted as platelets) and other platelet traits. All eosinophil and platelet traits should be adjusted for clump count.
 - NumRBC: 	RBC count (10^6/uL)
 - NumRetic:	reticulocyte count (10^6/uL)
 - PercRetic:	percent reticulocytes of total RBCs (%) = (NumRetic / NumRBC) * 100%
 - Hct:		hematocrit (%) = (NumRBC * MCV) / 10
 - Hgb:		measured blood hemoglobin concentration (g/dL)
 - CalcHgb:	calculated blood  hemoglobin concentration (g/dL) = (CHCM * NumRBC * MCV) / 1000
 - MCV:		mean corpuscular (RBC) volume (fL)
 - CHCM:		measured mean cellular hemoglobin concentration (g/dL)
 - MCHC:		calculated mean cellular hemoglobin concentration (g/dL) = (Hgb / (NumRBC * MCV)) * 1000
 - CH:		measured mean cellular hemoglobin content (pg)
 - MCH:		calculated mean cellular hemoglobin content (pg) = (Hgb / NumRBC) * 10
 - CHm:		mean cellular hemoglobin content of mature RBCs (pg) 							* Missing for the early CBC assays (missing from 297 records)
 - CHr:		mean cellular hemoglobin content of reticulocytes (pg)
 - RDWcv:		coefficient of variation of RBC volume (%)
 - RDWsd:		standard deviation of RBC volume (fL) = (RDWcv * MCV) / 100
 - HDWcv:		coefficient of variation of cellular hemoglobin concentration (%) = (HDWsd / CHCM) * 100%
 - HDWsd:		standard deviation of cellular hemoglobin concentration (g/dL)
 - NumWBC:	total WBC count (10^3/uL)
 - NumLymph:	lymphocyte count (10^3/uL)
 - NumNeut:	neutrophil count (10^3/uL)
 - NumMono:	monocyte count (10^3/uL)
 - NumEos:		eosinophil count (10^3/uL)
 - NumBaso:		basophil count (10^3/uL), generally very low or zero in peripheral blood of mice
 - NumLUC:      large unstained cell (atypical lymphocytes, blasts, and other abnormal cells) count (10^3/uL)
 - NLR:		ratio of neutrophils to lymphocytes = NumNeut / NumLymph
 - PercLymph:	percent lymphocytes (%) = NumLymph / NumWBC * 100%
 - PercNeut:	percent neutrophils (%) = NumNeut / NumWBC * 100%
 - PercMono:	percent monocytes (%) = NumMono / NumWBC * 100%
 - PercEos:	percent eosinophils (%) = NumEos / NumWBC * 100%
 - PercBaso:	percent basophils (%) = NumBaso / NumWBC * 100%
 - PercLUC:	percent large unstained cells (%) = NumLUC / NumWBC * 100%
 - NumPlt: 	platelet count (10^3/uL)
 - MPV:		mean platelet volume (fL)
 - MPM:		mean platelet mass (pg)
 - PDWcv:		coefficient of variation of platelet volume (%)
 - PDWsd:		standard deviation of platelet volume (fL) = (PDWcv * MPV) / 100

<br>
<br>

## Cleaning

The data cleaning was carried out in the following order and records with errors with errors were removed as they were identified. Thus, a record that has an error searched for in a later step would not be identified if the record was already removed due to an error identified in an earlier step. In summary, 1 record that was missing both its mouse ID and CBC data was removed, and 2 records that had mouse IDs but were missing all their CBC data were removed.

<br>

### 1) Missing mouse ID and CBC data: 1 removed

1 record that was missing both its mouse ID and CBC data was removed, they are saved in the CSV file: issues/records_missing_mouseid_and_cbc.csv

<br>
<br>

### 2) Missing mouse ID only: none

There are no records missing their mouse ID but not their CBC data

<br>
<br>

### 3) Missing CBC data only: 2 removed

2 records that were missing their CBC data but had a mouse ID were removed. Both of these records have sample comments indicating that the data are truly missing.

<br>
<br>

### 4) Complete duplicates: none

No records with duplicated mouse ID, test date, and CBC data

<br>
<br>

### 5) Multiple sets of CBC for one mouse at one date/timepoint: none

There are no mice that had more than set of CBC data for a given date or timepoint

<br>
<br>

### 6) Duplicate output row IDs: none

There are no records with identical output row IDs

<br>
<br>

### 7) Bodyweights recorded after death or before birth: none

There are no records assigned to mice that died before the test date or were not yet born

<br>
<br>

### 8) Impossible measures

There are no records with CBC values less than zero, and none with CBC values that appear implausible.

There is one record whose test bodyweight was impossibly high (3658 grams). This was a clear typo (technician forget to enter a decimal point) and was changed to 36.58 grams (which is close to the weekly bodyweight data for that mouse). The erroneous record is saved in the file: issues/impossibly_high_test_bw.csv

There is one record whose test bodyweight was impossibly low (2.51 grams). This is clearly a typo, but we cannot determine what the true value should be. The test bodyweight was replaced with NA. The erroneous record is saved in the file: issues/impossibly_low_test_bw.csv

<br>
<br>

### 9) Swaps: 2 corrected (4 records affected)

There were two sets of two-way sample swaps that were identified and corrected by comparing the bodyweight recorded at the time of the CBC assay to the weekly bodyweight data for the mice. Both swaps occured in the year 1 timepoint.  
 - The data assigned to mouse OR3609-5917 belongs to OR3609-5918 and vice versa 
 - The data assinged to mouse IL16750-5777 belongs to IL16750-5778 and vice versa  
 
The erroneous records are saved in the file: issues/sample_swaps.csv


<br>
<br>