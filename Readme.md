# Sustainability performance of Dutch firms and the role of digitalization: The case of textile and apparel industry (Analysis pipeline).
To get started, you can use `git clone url_for_this_repostiry` to create a local copy.
The script *1-4A\_Text_mining\_single\_document_group.R* used for the text-mining analysis is part of the Publication *"An original template solution for FAIR scientific text mining"*:
http://dx.doi.org/10.1016/j.mex.2023.102145
To perform the analysis, open the script below in R-studio. R:
* *1-4A\_Text_mining\_single\_document_group.R*



Select the folder you just cloned from GitHub as your working directory (Session -> Set Working Directory -> Source File Location).

Full website  html data is not shared since website data is owned by the respective website owners. However, we are allowed to share the aggregated data, which is the word counts per website, which can be used to fully reproduce the published results. To run the analysis starting from the aggregated data, select all code starting from *Line 120* where a backup of the aggregated is loaded from file *1-4A_data.RData*. After selecting all lines 120 to the end of the document, click "Run" in R-Studio to run the selected lines.  
To see which websites data was used for this analysis, see the  file *Data set jclepro.xlsx*. 

# Data acquisition and prepossessing  
The steps described below are not needed to run the analysis since we provide the segregated data set. However, we will provide a full description for all steps we used for transparency and reproduceability. 
In this section we will describe the exact steps performed to retrieve website data and pre-preproccessing of the data. To retrieve website information, you can run:

`python 0_get_web_data.py`. 

Users on Linux might need to use *python3*. In case running the script reports any libraries are missing, these can be installed using `pip library_name`, or in case on Linux using `pip3 library_name`.


## Cleanup of HTML data
The HTML data extracted from all websites contains an unsupported characters as well as words of interest that for analysis purposes should be conjugated. Conjugation is needed since otherwise words are seen as separate without considering their context. For example "child  labour" should be conjugated to retain its meaning since it is commonly used in another context, e.g. "children fashion". The following text replacements were made to pre-process the data for further analysis:

1) Open the file *results_per_base_url.tsv* in the freely available text editor Sublime, replace "<0x0a>" with a space and save the results as *results_per_base_url2.tsv*
2) Remove non ASCII, multiple space replace with one space.
`cat results_per_base_url2.tsv | tr -d '\200-\377' |tr -s '[:space:]' > results_per_base_url3.tsv`
3) Replace conjugated words with space to truly conjugated words separated by a hyphen
`cat results_per_base_url3.tsv | sed  -e 's/fair trade/fair_trade/g' |  sed  -e 's/fair wage/fair_wage/g' |sed  -e 's/second hand/second_hand/g' | sed  -e 's/360 degree/360_degree/g' | sed  -e 's/eco friendly/eco_friendly/g' |sed  -e 's/big data/big_data>/g' | sed  -e 's/fair wages/fair_wages/g' | sed  -e 's/child labor/child_labor/g' | sed  -e 's/child labour/child_labour/g' | sed  -e 's/labor conditions/labor_conditions/g' | sed  -e 's/labour conditions/labour_conditions/g' | sed -e 's/CO2 neutraal/C02_neutral/g' | sed  -e 's/C02 neutral/C02_neutral/g' | sed  -e 's/low impact/low_impact/g'> results_per_base_url4.tsv`  
For convenience, we provide a list of conjugated words where we replaced space with a hyphen:
* second-hand
* big-data
* fair-wages
* child-labor
* labor-conditions
* C02_neutral
* CO2-neutraal

## Website translation
Website data was translated to English for further analysis using a translation table created with DeepL language translation services. We did not provide a copy of the translation table since it might infringe on the copy right of DeepL services. See their website for a subscription.

## Reference
Direct link to the article: Tolentino-Zondervan, F. & Divito, L. 2024. Sustainability performance of Dutch firms and the role of digitalization: The case of textile and apparel industry. Journal of Cleaner Production. 142573. doi: https://doi.org/10.1016/j.jclepro.2024.142573
