# Multivariate Event Modeller

The Multivariate Event Modeller (MEM) is a tool designed for undertaking joint probability analysis
of extremes for up to 10 variables.

The joint probability of two or more variables being “extreme” is relevant in Flood and Coastal Risk
Management (FCRM) in various contexts, including:

- Assessing the likelihood of extreme peak flow events on multiple tributaries of a river to help in
  developing scenarios for a whole-catchment model
- Placing recent or historical floods in context by estimating the combined likelihood of extreme
  flows, water levels, rainfall, wave or wind observations at one or more locations
- Assessing the likelihood of combinations of extreme river flows, storm surge and possibly other
  relevant variables
- Modelling the chance of combinations of extreme conditions occurring together in related variables
  such as soil moisture content, rainfall accumulations and river flows

Previous [Defra and Environment Agency
guidance](http://www.estuary-guide.net/pdfs/FD2308_3429_TRP.pdf) describes methods for joint
probability calculations for certain pairs of variables, based on statistical models supported by
[specialist software](https://eprints.hrwallingford.com/404/).

More recently, [new statistical
methods](http://onlinelibrary.wiley.com/doi/10.1111/j.1467-9868.2004.02050.x/abstract) have been
developed that allow for a more general, data-driven analysis of the joint probability of extreme
events in combinations of multiple variables. Applications include regional and national scale
assessments of the [probability of widespread flooding in
rivers](http://onlinelibrary.wiley.com/doi/10.1111/j.1753-318X.2010.01081.x/abstract), and joint
probability analysis of [extreme surge and waves at the
coast](http://www.sciencedirect.com/science/article/pii/S0378383914000210).

These methods have been documented and tested through a series of [Environment Agency research
reports](https://www.gov.uk/flood-and-coastal-erosion-risk-management-research-reports/spatial-coherence-risk-of-widespread-flooding)
and were applied in the development of the [2017 National Risk Assessment
scenarios](https://www.gov.uk/flood-and-coastal-erosion-risk-management-research-reports/planning-for-the-risk-of-widespread-flooding).

The Multivariate Event Modeller (MEM) implements the new methods with user-supplied data sets to
estimate the joint probability of extreme events in combinations of up to 10 variables.

The MEM is designed for joint probability analysis of extremes in time series data for up to 10
variables. Additionally, the MEM handles a special case where data have been sampled for extreme
surge events to illustrate the extension of coastal joint probability methods to incorporate spatial
dependence.

## Acknowledgements

The Multivariate Event Modeller was initially developed as part of Environment Agency project
SC140002 (Spatial Joint Probability for FCRM and National Risk Assessment), within the
Defra/Environment Agency/Natural Resources Wales Joint Flood and Coastal Risk Management R&D
programme. The tool was updated in 2023, funded by the Environment Agency.

## Documentation

The overview tab within the MEM contains information about how to use the tool. There is also
guidance within each tab. Further details can be found in the [user
guide](https://assets.publishing.service.gov.uk/media/60364280d3bf7f0aaf64f109/Planning_for_the_risk_of_widespread_flooding_-_user_guide__1_.pdf)
published in 2018. Changes since 2018 are recorded in the release notes.

## Licences

The code was written by Jeremy Benn Associates Limited.

The code in the ‘Functions.R’ file is released under the [GNU General Public License
v2.0](https://choosealicense.com/licenses/gpl-2.0/) and the code in the ‘server.R’ and ‘ui.R’ files
is released under the [Open Government
Licence](https://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/).

## Support

Please send enquiries to FloodHydrology@environment-agency.gov.uk
