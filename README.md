PROJECT TITLE
FCCS - PREDICTED ROS COMPARISON FOR EGLIN AIR FORCE BASE FUELBEDS
This is a component of the Fuelbed Dynamics Model (FDM) development.

Purpose
Create tool for the Detailed Fuelbed Characterization, mapping, and Future Fire Hazard Assessment for Eglin Air Force Base, FL project that can visualize differences in rate of spread (ROS) amonng custom Fuelbed Characterization Classification System (FCCS) fuelbeds created for Eglin. This must be done because predicted ROS will be used to drive fire spread in the Fuelbed Dynamics Model simulations. We will attempt to use various fuel moisture scenarios and tranformations to create realistic predicted ROS.


Internal Project Reference.
Project Name: Detailed Fuelbed Characterization, mapping, and Future Fire Hazard Assessment for Eglin Air Force Base, FL
Project Short Name: Fuelbed Characterization & Mapping
Project Number: 2012_FCM_c

Author(s)(number indicates affiliation of organization listed below)
Jim Cronan(1)
Phone: 206-732-7873
Email: jcronan@fs.fed.us

Organization(s)
(1)
Pacific Wildland Fire Sciences Laboratory
400 North 34th Street, Suite 201
Seattle, WA 98122
Phone: 206-732-7800

Operation
Use FCCS to generate fire behavior potentials for fuelbeds
Use R code to visualize differences in ROS among fuelbeds
Adjust predicted ROS by grouping fuelbeds into fuel moisture scenario groups and transforming outputs to appropriately exagerate or subdue differences.

File architecture for FCCS-FirePotentialComparison repository
Directories:
	spreadsheet2fccs3_fuelbeds_CronanEglinFBs_2016-8-9 -- contains .xml Fuelbed Characteristic Classification System (FCCS) fuelbed files created by Anne Andreu on August 9, 2016. This was the set of fuelbeds submitted to JFSP and Eglin Air Firce Base (Jackson Gaurd, Brett Williams).
		Files: 
			181 .xml files. Not listed here.
	inputs -- contains files used to summarize and analyze rate of spread values use to parameterize FDM probability of ignition values.
		Files:
			fuelbed_synchronicity.csv -- contains information from .xml fccs fuelbeds (C:\Users\jcronan\Documents\Projects_Incomplete_Files\2012_Fuelbed_Characterization_and_Mapping\4_Project_Data\Final_Datasets_Submitted_to_JFSP\Eglin_FCCS_Fuelbeds\*.xml), FDM fuelbed inputs (C:\Users\jcronan\Documents\GitHub\EglinAirForceBase\inputs\sef_lut_all.csv) and how they synchronize with fuelbeds detailed in state and transition models (C:\Users\jcronan\Documents\Projects_Incomplete_Files\2012_Fuelbed_Characterization_and_Mapping\4_Project_Data\Final_Datasets_Submitted_to_JFSP\Eglin_FCCS_STM\Eglin_FCCS_STM\FCCS_Fuelbeds_StateAndTransitionModels.pdf)
				column descriptions
					andreu_fuelbed_no: Andreu's .xml FCCS fuelbed that matches or best corresponds to fuelbed in FDM input (sef_lut_all.csv)
					benchmark_ros: Benchmark rate of spread value from FCCS outputs for the .xml fccs fuelbed. See fccs_ros_moistScenario_1_tansformation_none.csv description for more details on how these values were obtained.
					stm_added: is the fuelbed referenced in the "fuelbed" column listed in the state and transition model (Yes -- it is listed; No -- it is not listed; Add -- it is not listed, but should be listed because it is part of the FDM state and transition model).
					
					... the column headings and values below are copied directly from sef_lut_all.csv to help classify .xml and STM fuelbeds. In scripts these values will be referenenced directly from the EglinAirForceBase repo on GitHub in case any values change.
					fuelbed: fuelbed number in the FDM input parameters
					thin: new fuelbed if thinning (i.e., sand pine removal) treatment is implemented.
					herb: new fuelbed if herbicide (purpose to remove oak species) treatment is implemented.
					rxfire: new fuelbed if prescribed fire treatment is implemented.
					eligibility_thinning: is fuelbed tallied when identifying area eligible for thinning treatment? 1 = No 2 = Yes.
					eligibility_herbicide: is fuelbed tallied when identifying area eligible for herbicide treatment? 1 = No 2 = Yes.
					eligibility_rxfire: is fuelbed tallied when identifying area eligible for prescribed fire treatment? 1 = No 2 = Yes.
					treatability_thinning: can fuelbed be thinned if unit is selected for thinning treatment? 1 = No 2 = Yes.
					treatability_herbicide: can herbicide be applied to fuelbed if unit is selected for thinning treatment? 1 = No 2 = Yes.
					treatability_rxfire: can fuelbed be burned if unit is selected for thinning treatment? 1 = No 2 = Yes.
					post_1: new fuelbed if max time-since-last-treatment (TSLT) is exceeded (see "max_tslt" column).
					max_tslt: max number of years fuelbed can go without additional treatment before natural succession causes it to transition to fuelbed listed in "post_1" column.
					mfri_shortens: new fuelbed if the mean (I believe this is dynamic) fire return interval (mfri) shortens below threshold (in years) listed in the "mfri_lower" column.
					mfri_lengthens_1: new fuelbed (option 1 of 2)if the mfri lengthens above threshold (in years) listed in the "mfri_upper" column.
					mfri_lengthens_2: new fuelbed (option 2 of 2)if the mfri lengthens above threshold (in years) listed in the "mfri_upper" column.
					crown_fire: new fuelbed if FDM model determines that a fire caused overstory mortalility (assumes this is crown fire, not root damage from smoldering).
					mfri_lower: shortest mfri (in years) before fuelbed will transition (see "mfri_shortens" column).
					mfri_upper: longest mfri (in years) before fuelbed will transition (see "mfri_lengthens_1" and "mfri_lengthens_2" column).
					succession_post_1: new fuelbed (option 1) if stand age increases above threshold (in years) listed in "succession_max" column. A -1 value should correspond with an infinite upper stand age limit. This occurs for "climax" forests and landcover types that do not change over the temporal scale this model is built for such as water or rangeland. A -2 value means this fuelbed should not exist because it doesn't fall within the STMs designed for this model. If this fuelbed occurs it is due to an error in the STM lookup table (sef_lut_all.csv) or initial generation of the FCCS fuelbed map. In these instances the max_tslt value should be set to zero and transition the erroneous fuelbed to something within the STM architecture. If the succession sub-model encounters a negative value when evaluating pixels for succession the model should crash.
					succession_post_2: new fuelbed (option 2) if stand age increases above threshold (in years) listed in "succession_max" column. A -1 value means there is no option 2. A -2 value has the same meaning as -2 in "succession_post_1" column (see above). 
					succession_max: maximum stand age before natural succession causes fuelbed to transition.
					probability_of_ignition: expert assignment of probability of ignition under environmental conditions specified in prescribed burn plans. This is based on my knowledge of southeastern fuels and discussions with fire management staff at Eglin Air Force Base.
			fccs_ros_moistScenario_1_tansformation_none.csv -- contains all output from FCCS (Fuel and Fire Tools) > Selected Fuelbeds (load all .xml files in "spreadsheet2fccs3_fuelbeds_CronanEglinFBs_2016-8-9" folder > Click "Next specify environmental inputs" button > Select "FCCS Benchmark Inputs" under "Select Environmental Scenario for 'NewUnit' (an FCCS-only unit):" window; then Click "Run FCCS" button > Go to C:\...\FuelFireTools\00_results\fccs\fccs_summary.csv. The fourth column (Column D; Benchmark_ROS) contains values are pasted into fuelbed_synchronicity.csv$benchmark_ros
			fuelbeds.xlsx -- DELETE THIS FILE. This is the original fuelbed_synchronicity file and many of the tasks implemented in R script were being done manually before I realized this was to slow and difficult to repeat. DELETE THIS FILE.
	FCCS_viaFFT -- contains version of FFT used to generate rate of spread values. These are used to evaluate suitability of benchmark rate of spread values and generate a more realistic set of rate of spread values that can be scaled to replace expert opinion-derviced probability of ignition values originally used as inputs in FDM.
		Files and sub-directories:
			Not listed here.
Files:
	.gitignore -- list files that should be ignored by Git.
	README.md -- describes contents and purpose of repository
	fccs_fire_potential_comparision.R -- script to compare predicted FCCS Benchmark ROS as probability of ignition (PIG) values against expert opintion-derived PIG values and to identify new environmental scenarios and transformations to apply to FCCS ROS predictions to create a realistic and objectively derived set of PIGS.
	20171201_fuelbed_synchronicity_notes.pdf -- hand written notes on print out of STMs describing the degree of synchronicity among the three fuelbed datasets (STM, FDM inputs, and .xml files).



 
