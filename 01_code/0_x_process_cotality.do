* Handle a difficult .dta.

*  Read

use "../02_data/0_0_0_Cotality/OR41_PB_080524.dta"

*  Reduce

keep ///
clip previousclip ///
fips fipscode stfips ctyfips ///
apnparcelnumberunformatted onlineformattedparcelid alternateparcelid ///
previousparcelnumber previousparcelsequencenumber previousparcelnumberformatted ///
censusid ///
landusecode countyusedescription stateusedescription ///
zoningcode zoningcodedescription ///
blocklevellatitude blocklevellongitude ///
parcellevellatitude parcellevellongitude ///
legaldescription ///
owner*

*  Write

export delimited owners.csv
