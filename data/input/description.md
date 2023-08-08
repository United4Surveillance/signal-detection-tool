| Variable               | Mandatory | Description|
|------------------------|-----------|------------|
| case_id	             | YES | Unique identifier for a specific case, but nothing that can be used to identify the case or patient within the national surveillance system |
| date_report	           | YES | Date when the case was reported in ISO 8601 format YYYY-MM-DD |
| date_onset	           | If available | Date when symptoms started in ISO 8601 format YYYY-MM-DD|
| date_hospitalization	 | If available | Date when the case was admitted to the hospital in ISO 8601 format YYYY-MM-DD|
| date_death	           | If available |  Date when the case died in ISO 8601 format YYYY-MM-DD|
| date_vaccination	     |If available | Date when the case was vaccinated in ISO 8601 format YYYY-MM-DD|
| country	               |YES|Name of the country in which the case was reported|
| country_id	           |YES|Id of the country in which the case was reported, Id should be NUTS-0 or ISO-alpha3 code|
| state	                 | NO | Name of the state/major region in which the case was reported|
| state_id	             |NO | Id of the state as NUTS-1 code|
| county	               | NO | Name of the county/basic region in which the case was reported|
| county_id	             | NO | Id of the county as NUTS-2 code|
| community              | NO | Name of the community/small region in which the case was reported|
| community_id           | NO | Id of the community as NUTS-3 code|
| age	                   | NO | Age of the reported case at the time of reporting as integer between 0-125|
| age_group	             | YES | Age group of the reported case at the time of reporting as interval, e.g. 05-14 for cases with an age between 5 and 14 years|
| sex	                   | YES | Sex of the reported case, one of male, female, diverse, unknown|
| occupation	           | NO | Occupation of the reported case, one of the following options: care, kindergarten, food production, ???|
| place_of_infection	   | NO | Name of the region in which the case was likely infected if that place is different from the reporting region|
| place_of_infection_id	 | NO | Id of the region in which the case was likely infected as NUTS-code|
| pathogen	             | YES |  Name of the pathogen/diagnosis that was identified |
| pathogen_id	           | NO | Id of the pathogen/diagnosis |
| subtype	               | NO | Name of the subtype/serovar/variant that was identified|
| subtype_id	           | NO | Id of the subtype/serovar/variant |
| hospitalization	       | YES | Indication whether the case was hospitalized, YES, NO or unknown |
| death                  | YES | Indication whether the case has died, YES, NO or unknown|
| vaccination	           | YES | Indication whether the case was vaccinated before infection, YES, NO or unknown|
| symptoms	             | NO | List of symptoms that were observed ???|
| risks                  | NO | List of risks that the case had before the infection |
