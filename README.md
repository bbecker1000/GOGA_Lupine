# GOGA_Lupine

Claire Whicker Senior Project for CA-CESU and GGNRA

## General Information
- **Dataset Name**: GGNRA Lupin Disturbance Project
- **Description**: 
- **Source**: Golden Gate National Recreation Area, Research Team
- **Date Created**: 2010
- **Author/Owner**: National Park Service

## Data Structure -- lupineR.csv
- **File Format**: CSV
- **Size**: 25 rows, 25 columns

## Variables Information -- lupineR.csv
| Variable Name  | Description                              | Data Type | Possible Values       | Units | Missing Values |
|----------------|------------------------------------------|-----------|-----------------------|-------|----------------|
| Plot           | Location of treatment blocks             | Integer   | 1-10                  | N/A   | 4 & 5 ?        |
| Site           | Region of GGNRA                          | String    | Milagra, Marin        | N/A   | None           |
| Year           | Year of data collection                  | String    | 2010, 2011            | Years | None           |
| Treatment      | Disturbance type applied                 | String    | a-Control, Burn, Mech | N/A   | None           |
| I_LUAL_2010    | | Integer | 0 - 19 | N/A | None |
| I_LUAL_2011    | | Integer | 0 - 53 | N/A | None |
| LUAL_I         | | Integer | -6 - 43 | N/A | None |
| I_2010         | | Integer | 0 - 19 | N/A | None |
| M_2010         | | Integer | 7 - 49 | N/A | None |
| R_2010         | | Integer | 0 | N/A | None |
| A_2010         | | Integer | 7 - 49 | N/A | None |
| T_2010         | | Integer | 7 - 52 | N/A | None |
| I_2011         | | Integer | 0 - 53 | N/A | None |
| M_2011         | | Integer | 0 - 44 | N/A | None |
| R_2011         | | Integer | 0 - 44 | N/A | None |
| A_2011         | | Integer | 4 - 50 | N/A | None |
| T_2011         | | Integer | 5 - 69 | N/A | None |
| IMMATURE       | | Integer | -6 - 49 | N/A | None |
| MATURE         | | Integer | -45 - 18 | N/A | None |
| RESPROUTS      | | Integer | 0 - 44 | N/A | None |
| ADULTS         | | Integer | -14 - 19 | N/A | None |
| TOTAL          | | Integer | -4 - 44 | N/A | None |
| IMM_TRANS      | | Float | 0 - 10.000000000 | N/A | None |
| RESP_TRANS     | | Float | 0 - 10.000000000 | N/A | None |
| TOT_TRANS      | | Float | 0 - 10.000000000 | N/A | None |

## Data Structure -- NativityforR.csv
- **File Format**: CSV
- **Size**: 49 rows, 9 columns

## Variables Information -- NativityforR.csv
| Variable Name  | Description                               | Data Type | Possible Values       | Units  | Missing Values |
|----------------|-------------------------------------------|-----------|-----------------------|--------|----------------|
| PLOT           | Location of treatment blocks              | Integer   | 1-10                  | N/A    | 4 & 5 ?        |
| SITE           | Region of GGNRA                           | String    | Milagra, Marin        | N/A    | None           |
| TREATMENT      | Disturbance type applied                  | String    | a-Control, Burn, Mech | N/A    | None           |
| NON-NATIVE     | Counts of non-native species in treatment block | Integer | 31 - 232          | N/A    | None           |
| NATIVE         | Counts of native species in treatment block | Integer | 31 - 164              | N/A    | None           |
| TOTAL          | Counts of native and non-native species in treatment block | Integer | 149 - 282 | N/A | None           |
| PER_NATIVE     | Percent cover of native species           | Float     | 0.177304965 - 0.816568047 | N/A | None          |
| YEAR           | Year of data collection                   | String    | 2010, 2011            | Years | None            |
| DIFFERENCE     | 

## Data Structure -- Cover_Lifeform_All.csv
- **File Format**: CSV
- **Size**: 20,553 rows, 10 columns

## Variables Information -- Cover_Lifeform_All.csv
| Variable Name  | Description                               | Data Type | Possible Values       | Units  | Missing Values |
|----------------|-------------------------------------------|-----------|-----------------------|--------|----------------|
| Year           | Year of data collection | String | 2009 2010 2011 2012 2013 | Years | None |
| Macroplot      | Name denotes the plot #, treatment type, and site | String | 30 Possible Values | N/A | None |
| Treatment      | Disturbance type applied | String | BURN, CONTROL, MECHANICAL | N/A | None |
| Species        | Four-letter species code | String | 154 Possible Values | N/A | None |
| Lifecycle      |  | Perennial, Annual, Biennial, Not Defined | N/A | Yes (26 NA values) |
| Preferred_LF   |  | Forb/Herb, Graminoid, Shrub, Forb, Grass-like, Subshrub, Vine, Fern, Undefined | N/A | None |
| Default_LF     |  | Forb, Grass, Shrub, Undefined | N/A | None |
| Native         | The species is native to the sampling area | String | TRUE, FALSE | N/A | None |    
| Invasive       | The species is invasive to the sampling area | String | TRUE, FALSE | N/A | None | 
| Count          | How many individuals were at the sampling point | Integer | 1 | N/A | None | 


## Contact Information
- **Contact Person**: Claire Whicker
- **Email**: clairewhicker@berkeley.edu

