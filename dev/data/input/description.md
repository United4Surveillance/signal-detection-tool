| Variable               | Mandatory | Description                                       |
|------------------------|-----------|---------------------------------------------------|
| case_id	             | YES | Unique identifier for a specific case, but nothing that can be used to identify the case or patient within the national surveillance system  |
| date_report	           | YES | Date when the case was reported in ISO 8601 format YYYY-MM-DD  |
| date_onset	           | NO  | Date when symptoms started in ISO 8601 format YYYY-MM-DD |
| date_hospitalization	 | NO  | Date when the case was admitted to the hospital in ISO 8601 format YYYY-MM-DD |
| date_death	           | NO  |  Date when the case died in ISO 8601 format YYYY-MM-DD |
| date_vaccination	     |NO  | Date when the case was vaccinated in ISO 8601 format YYYY-MM-DD |
| country	               |YES|Name of the country in which the case was reported|
| country_id	           |YES|Id of the country in which the case was reported, Id should be NUTS-0 or ISO-alpha3 code|
| state	                 | NO | Name of the state/major region in which the case was reported|
| state_id	             |NO | Id of the state as NUTS-1 code|
| county	               | NO | Name of the county/basic region in which the case was reported|
| county_id	             | NO | Id of the county as NUTS-2 code|
| community              | NO | Name of the community/small region in which the case was reported|
| community_id           | NO | Id of the community as NUTS-3 code|
| region_level1            | NO | Name of the major region in which the case was reported, alternative to the state column |
| region_level1_id             | NO | Id of the region as specified in the shapefile  |
| region_level2              | NO | Name of the basic region in which the case was reported, alternative to the county column   |
| region_level2_id               | NO | Id of the region as specified in the shapefile    |
| region_level3                | NO | Name of the small region in which the case was reported, alternative to the community column     |
| region_level3_id                 | NO | Id of the region as specified in the shapefile      |
| age	                   | NO | Age (in years) of the reported case at the time of reporting as integer between 0-125. Either age or age_group should be provided |
| age_group	             | NO | Age group (in years) of the reported case at the time of reporting as interval, e.g. 05-14 for cases with an age between 5 and 14 years. Allowed separators are -, —, _ and < for the first age group. For the last age group "digit separator digit" (i.e. 90-100 or 90_100 depending on your separator) or digit+ (i.e. 100+) is allowed. For all age groups except the first and last only one separator should be used. Either age or age_group should be provided |
| sex	                   | NO  | Sex of the reported case, one of male, female, diverse, unknown |
| occupation	           | NO | Occupation of the reported case, possible options (not limited to): care, kindergarten, food production |
| place_of_infection	   | NO | Name of the region in which the case was likely infected if that place is different from the reporting region |
| place_of_infection_id	 | NO | Id of the region in which the case was likely infected as NUTS-code |
| pathogen	             | YES |  Name of the pathogen/diagnosis that was identified |
| pathogen_id	           | NO | Id of the pathogen/diagnosis |
| subtype	               | NO | Name of the subtype/serovar/variant that was identified|
| subtype_id	           | NO | Id of the subtype/serovar/variant |
| hospitalization	       | NO | Indication whether the case was hospitalized, yes, no or unknown  |
| death                  | NO | Indication whether the case has died, yes, no or unknown |
| vaccination	           | NO | Indication whether the case was vaccinated before infection, yes, no or unknown |
