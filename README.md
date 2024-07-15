# Introduction

This repository manages code and data for a value of information assessment by researchers in the VALUABLES Consortium. The VALUABLES Consortium is a cooperative agreement between Resources for the Future and NASA. See [Bernknopf et al. (2021)](https://www.rff.org/publications/working-papers/earth-observations-can-enable-cost-effective-conservation-of-eastern-north-pacific-blue-whales/) for further information. Contact Yusuke Kuwayama (kuwayama@umbc.edu) with any questions on this assessment and related work.

# Contents

This repository includes two sets of scripts written to be run in sequence. The first set of scripts (/scripts) conducts an analysis for the US EEZ off California, Oregon, and Washington. The second set of scripts (/scripts_california) conducts a similar analysis for state waters off California. Each set of scripts runs in sequence following prefixes to filenames, from "1_" to "6f_."

# Dependencies

Altogether, scripts for this project require a handful of R packages and several datasets. Data for this project are detailed in the working paper linked above (Bernknopf et al. 2021) and in a follow-on manuscript in preparation for peer review. Critically, scripts require a large dataset of Automatic Identification System (AIS) reports, retrieved through marinecadastre.gov.

# Other Notes

Preserving intermediate data products, e.g. processed AIS reports, allows for passing over the time-intensive scripts 3a_ and 3b_. References to Natural Earth through rnaturalearth in script 5_ are fragile with respect to server uptime and nomenclature for geodata. Applications of this codebase to other case studies should require only lightweight changes: outside of spatial visualizations, there are few or no hard-coded references to the particulars of this case study.
