These are the data and files for the 4 basic sources 
for the Sumatra tsunami.

The earthquake rupture is split into four sections.  

All files are numbered sequentially from south to north.

The number "1" always indicates the southern most section.    

You will need to regrid the source data in Lat/Long, or
calculate the source directly in Lat/Long using TOPICS.

I assumed a central longitude of 85 degrees, which may 
be wrong, when converting back from UTM to Lat/Long.  


FILES
-----

TOPICS12 -> Source generation software for UTM or Lat/Long

GEO2UTM -> Conversion software to go either way (FRENCH)

out#.txt -> Gives the tsunami source data

surface#.grd -> SUrfer ASCII grid file in UTM

xyz#.dat -> ASCII file of XYZ triplets in UTM

GEOxyz#.dat -> ASCII file of xyz triplets in Lat/Long

Centroids -> Tsunami source centroid data in UTM

GEOCentroids -> Tsunami source centroid data in Lat/Long