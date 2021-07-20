# Overview

The processed animal data (animal_data_processed) was created from data output from the bulk animal export facet (AnimalBulkExport) of Climb, our laboratory information management system, and data output from the animal facet (animals). During data processing, most were changes of column names and reformatting of the names of mice, strains, diets, cohorts, etc. The only notable change was the designation of housing IDs (which indicate the pen each mouse was assigned to). Mice were housed in pems of up to four mice per pen, though some pens contained fewer than four. Four mice had the wrong housing IDs in the "animals" data, but we were able to find their correct housing ID. For full details on the processing of the animal data, please refer to the R-script "process_animal_data_20210719.R", it is heavily commented so it should be relatively read-able.

The processed animal data is a CSV file that contains 800 rows (one for each mouse) and 19 columns:
 - MouseID: the name of the mouse, the first part of the name is the same as the "StrainAKA" field and indicates the strain of the mouse
 - Diet: the diet the mouse was assigned to, AL = ad libitum (control) and IF = 2-day intermittent fast (treatment)
 - Strain: the strain of the mouse (can be searched on the jax.org for strain information)
 - StrainAKA: the strain of the mouse in a different format (can also be searched on the jax.org for strain information)
 - Sex: the sex of the mouse
 - Generation: the generation of the mouse, I'm not sure what all the generation codes mean
 - Cohort: the cohort of the mouse (wave and group)
 - JobGroup: the job group of the mouse (the cohort plus whether the mouse was part of day 1 phenotyping or day 2). Each cohort was split into two phenotyping groups, day 1 and day 2, because early on in the study there were too many mice per cohort for them all to be phenotyped on the same day. Later in the study, when there were fewer mice, the mice in both groups were often phenotyped on the same day.
 - BWDay: the day of the week the weekly bodyweights were recorded. Before the start of the diet intervention, mice were not always measured on the same day of the week, but after the start of the diet intervention each mouse only has its weekly bodyweight recorded on the indicated day of the week
 - HID: the housing ID (pen) to which the mouse was assigned
 - LHID: the last housing ID, the ID of the pen in which the mouse died. Occasionally mice are moved to a different pen due to illness or fighting with housemates. If the mouse dies before it is returned to its original pen then the LHID will be different from the HID.
 - EarNotch: the ear-notching of the mouse. Mice are given small notches in their ears to help differentiate them. Not all mice have a recorded ear-notching
 - Coat: the coat color of the mouse. Not all mice have a recorded coat color.
 - AnimalComments: any comments the animal care technicians recorded over the lifetime of the mouse
 - DOB: the birthdate of the mouse
 - DOE: the death (or study exit) date of the mouase
 - COE: the cause of death of the mouse
 - SurvDays: the number of days from birth until death/exit
 - Died: whether the mouse died (can be considered as having full lifespan data) or was censored


