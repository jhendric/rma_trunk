[]{#TOP}

DART observations and MODIS products.
=====================================

+-----------------------------------+-----------------------------------+
| ![DART project                    | Jump to [DART Documentation Main  |
| logo](../../../documentation/imag | Index](../../../documentation/ind |
| es/Dartboard7.png){height="70"}   | ex.html)\                         |
|                                   | version information for this      |
|                                   | file:\                            |
|                                   | \$Id\$                            |
+-----------------------------------+-----------------------------------+

[PROGRAMS](#Programs) / [PLANS](#Plans) / [TERMS OF USE](#Legalese)

There are many MODIS products, in many formats. This document will list
all of the data products and formats that have DART programs to convert
them to observation sequence files. []{#Programs}

------------------------------------------------------------------------

PROGRAMS
--------

  ----------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
  [MOD15A2\_to\_obs](MOD15A2_to_obs.html)   Converts [MODIS Land Product Subsets](http://daac.ornl.gov/MODIS/modis.shtml) Leaf Area Index (**LAI**) and Fraction of Photosynthetically Active Radiation (**FPAR**) 8 day composite [\[MOD15A2\]](https://lpdaac.usgs.gov/products/modis_products_table/mod15a2)
  ----------------------------------------- ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

[]{#Plans}

------------------------------------------------------------------------

PLANS
-----

1.  Support MOD15A2 'Global Tool' records.
2.  The work that remains is to get the IGBP landcover code for the site
    and incorporate that into the observation metadata. I *almost* have
    everything I need. Once that happens, the forward observation
    operator can be made to be much more accurate by only using model
    landunits that have the right landcover class.
3.  Support more products. Put in a request to help me prioritize.

[]{#Legalese}

------------------------------------------------------------------------

Terms of Use
------------

DART software - Copyright UCAR. This open source software is provided by
UCAR, "as is", without charge, subject to all terms of use at
<http://www.image.ucar.edu/DAReS/DART/DART_download>

  ------------------ -----------------------------
  Contact:           Tim Hoar
  Revision:          \$Revision\$
  Source:            \$URL\$
  Change Date:       \$Date\$
  Change history:    try "svn log" or "svn diff"
  ------------------ -----------------------------


