| Variable               | Mandatory | Description|
|------------------------|-----------|------------|
| date_report	           | YES | Date when the case was reported in ISO 8601 format YYYY-MM-DD |
| country	               | YES | Name of the country in which the case was reported|
| country_id	           | YES | Id of the country in which the case was reported, Id should be NUTS-0 or ISO-alpha3 code|
| state	                   | NO  | Name of the state/major region in which the case was reported|
| state_id	               | NO  | Id of the state as NUTS-1 code|
| county	               | NO  | Name of the county/basic region in which the case was reported|
| county_id	               | NO  | Id of the county as NUTS-2 code|
| community                | NO  | Name of the community/small region in which the case was reported|
| community_id             | NO  | Id of the community as NUTS-3 code|
| age	                   | NO  | Age of the reported case at the time of reporting as integer between 0-125|
| age_group	               | NO  | Age group of the reported case at the time of reporting as interval, e.g. 05-14 for cases with an age between 5 and 14 years|
| sex	                   | NO  | Sex of the reported case, one of male, female, diverse, unknown|
| pathogen	               | YES |  Name of the pathogen/diagnosis that was identified |
| pathogen_id	           | NO  | Id of the pathogen/diagnosis |
| subtype	               | NO  | Name of the subtype/serovar/variant that was identified|
| subtype_id	           | NO  | Id of the subtype/serovar/variant |
| outbreak_status          | YES | Indication whether the case is known to be part of an outbreak, YES, NO, Unknown |
| outbreak_id	           | NO  | Unique identifier for the specific outbreak, but nothing that can be used to identify the cases or outbreak within the national surveillance system |