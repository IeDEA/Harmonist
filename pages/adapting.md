# Adapting the Harmonist Data Toolkit to a Research Network (with no Hub)

- Obtain access to [REDCap](https://projectredcap.org/partners/){:target="_blank"}.  
- Install the REDCap Data Model Browser external module.  
- Define the approved code lists for your data model in a copy of the Harmonist Code Lists (0B) REDCap project.
- Describe your common data model in a copy of the Harmonist Data Model (0A) REDCap project.
- Create a small sample fake dataset and save to a zip file.
- Create a logo image that has a resolution of 100 x 40 pixels, and another with a resolution of 50 x 20 pixels.
- Add the network-specific details, logo files, sample dataset zip file, etc. to the Harmonist Toolkit Metadata (0C) REDCap project.
- Use the Harmonist JSON copy generator feature of the Data Model Browser external module to create JSON files summarizing those three projects. Those JSONs will be stored in records in the Harmonist JSON Files REDCap project.
- Download the Harmonist Data Toolkit code from [Github](https://github.com/IeDEA/Harmonist){:target="_blank"}, install R and the required packages (Shiny, etc.). This application can be run on your local machine or installed on a server for web access.
- In REDCap, add API rights to a user (application) account for the relevant Harmonist REDCap projects. Edit the file named redcapTokens.R.example file, replace "your token here" with the API token for the indicated REDCap project. Rename the file redcapTokens.R and save. 
- Edit definitions.R and specificDefinitions.R files and customize the variable values for your research group.
- Edit globalDateBeforeChecks.json and globalDateAfterChecks.json to include details of dates that should be before or after all other dates (and exceptions to those).
- Edit numericLimits.json to describe upper and lower plausible limits for relevant numeric variable values.
- Edit withinTableDateOrder.json to list date pairs that should be in a specific order but don’t follow variable extension rules. Both date variables of each pair must be in the same table.
- Edit datasetSummary.json to describe information to include in the main Toolkit report summary of patient characteristics.
- Add custom data quality checks to the file customChecks.R if desired. See [Guidance for Custom Data Quality Checks](pages/customchecks.md) for details.

*Note: If your organization does not have access to REDCap, it is possible to manually edit Harmonist0A, Harmonist0B, and Harmonist 0C JSON files to desribe your data model and research group* 
