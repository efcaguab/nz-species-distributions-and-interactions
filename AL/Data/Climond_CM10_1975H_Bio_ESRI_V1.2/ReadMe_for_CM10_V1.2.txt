* This data is part of the CliMond V1.2 dataset, released on 9 September 2014.

-------------------------------------------------------------------------
Citing this material
-------------------------------------------------------------------------

Please cite this material as:  Kriticos, D.J., Webber, B.L., Leriche, A., Ota, N., Macadam, I., Bathols, J. & Scott, J.K. (2012) CliMond: global high resolution historical and future scenario climate surfaces for bioclimatic modelling. Methods in Ecology & Evolution 3: 53-64. DOI: 10.1111/j.2041-210X.2011.00134.x

In first making reference to the data obtained from the CliMond archive, please include the phrase "the CLiMond dataset (Kriticos et al. 2012)".  If you use the Bio36-Bio40 variables, please also cite Kriticos et al. (2014).


-------------------------------------------------------------------------
To contact the authors:
-------------------------------------------------------------------------

Please use the feedback form at http://www.climond.org/contactus.aspx


-------------------------------------------------------------------------
Data derivation summary of the CliMond V1.2 datasets:
-------------------------------------------------------------------------
Any URL’s referenced below were correct at the time of data download.  The CliMond team accepts no responsibility for the upkeep of these web addresses.  If you find that the links are stale, we suggest you search for the data on the primary domain address for each file.

* 10’ Historical Data (10’ raw source format)
The historical data for daily minimum temperature, daily maximum temperature, monthly precipitation, and elevation were downloaded from the WorldClim website <http://worldclim.org/> (Version 1.4, release 3; Hijmans et al. 2005):

      • Daily minimum temperature: http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/tmin_10m_esri.zip
      • Daily maximum temperature: http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/tmax_10m_esri.zip
      • Monthly precipitation: http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/prec_10m_esri.zip
      • Elevation: http://biogeo.ucdavis.edu/data/climate/worldclim/1_4/grid/cur/alt_10m_esri.zip

Relative humidity data was downloaded from the website <http://www.cru.uea.ac.uk/cru/data/hrg/tmc/> of the Climatic Research Unit (CRU) at the University of East Anglia (New et al. 2002).

      • Relative humidity: http://www.cru.uea.ac.uk/cru/data/hrg/tmc/grid_10min_reh.dat.gz


* 10’ Future Climate Scenario Data (30’ raw source format)
Future climate change data were pattern-scaled against historical data using the results of individual global climate models.  The data for daily minimum temperature, daily maximum temperature, monthly precipitation, and specific humidity were downloaded from the IPCC Data Distribution Centre website <https://esg.llnl.gov:8443/index.jsp> (Meehl et al. 2007).  The algorithm adopted for creating future surfaces from the downloaded files follows that of Kriticos et al. (2012).

After registering for an account with the WCRP CMIP3 multi-model database <https://esg.llnl.gov:8443/security/accountRequestData.do> the data can be downloaded by ftp using the following URL template:  "ftp://ftp-esg.ucllnl.org/ipcc/scenario/submodel/frequency/variable/model/run1"

Where each of the above URL parameters can take the following values:
    scenario = sresa1b or sresa2
    submodel = land
    frequency = mo (monthly)
    variable =
      • tasmax (daily minimum air temperature)
      • tasmin (daily maximum air temperature)
      • prc (monthly total precipitation)
      • huss (surface specific humidity)
      • ps (surface air pressure)
      • psl (seal level air pressure)
    model = CSIRO-Mk3.0 or MIROC-H
    run = run1

For example, if you want to download monthly Maximum Air Temperature data for the CSIRO Mk 3 GCM for the SRES A1B scenario you would use the follwing URL:  "ftp://ftp-esg.ucllnl.org/ipcc/sresa1b/land/mo/tasmax/CSIRO-Mk3.0/run1"

Alternatively, you can browse for the data at https://esg.llnl.gov:8443/index.jsp

The historical data for mean sea level pressure were downloaded from the European Centre for Medium-Range Weather Forecasts 40 Year Re-analysis (ERA-40) Data Archive:

      • Mean sea level pressure: http://www.ecmwf.int/products/data/archive/descriptions/e4/index.html


*BioClim variables
The algorithms adopted for calculating the BioClim variables follows that of Kriticos et al. (2012) and Kriticos et al. (2014).


*Köppen-Geiger climate classification 
The algorithm adopted for calculating the Köppen-Geiger climate classification follows that of Kriticos et al. (2012).

-------------------------------------------------------------------------
References:
-------------------------------------------------------------------------

Hijmans, R.J., Cameron, S.E., Parra, J.L., Jones, P.G. & Jarvis, A. (2005) Very high resolution interpolated climate surfaces for global land areas. International Journal of Climatology, 25: 1965-1978.

Kriticos, D.J., Jarošik, V., Ota, N. (2014) Extending the suite of Bioclim variables: A proposed registry system and case study using principal components analysis". Methods in Ecology and Evolution, Online Early (DOI: 10.1111/2041-210X.12244).

Kriticos, D.J., Webber, B.L., Leriche, A., Ota, N., Macadam, I., Bathols, J. & Scott, J.K. (2012) CliMond: global high resolution historical and future scenario climate surfaces for bioclimatic modelling. Methods in Ecology & Evolution, 3: 53-64.

Meehl, G.A., Covey, C., Delworth, T., Latif, M., McAvaney, B., Mitchell, J.F.B., Stouffer, R.J. & Taylor, K.E. (2007) The WCRP CMIP3 multimodel dataset: a new era in climate change research. Bulletin of the American Meteorological Society, 88: 1383–1394.

New, M., Lister, D., Hulme, M. & Makin, I. (2002) A high-resolution data set of surface climate over global land areas. Climate Research, 21: 1-25.


-------------------------------------------------------------------------
CliMond: global climatologies for bioclimatic modelling
www.climond.org
-------------------------------------------------------------------------
