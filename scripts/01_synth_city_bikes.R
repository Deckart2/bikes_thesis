#Accessing, cleaning, synthesizing city bike lane data
#Gabriel Morrison
#January 8, 2019

#First Task:
# write a function that, for city folder:
  #Reads the .shp/geojson/other file
  #Converts it to a crs: 2163
  #Potentially filters out proposed bike lanes
  #Selects only type of lane column
  #Joins to another column

#Packages to Use:
library(tidyverse)
library(tidylog)
library(sf)
library(tmap)

#Function for reading "standardized" files ====

#Note: missing() argument/style inspired from LyzandeR's response here:
        #https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions
#Use crs 2163 because it relatively good equal area representation of entire US: https://source.opennews.org/articles/choosing-right-map-projection/

#Function Below:
  #folder_name= (string) name of folder in bike files, usually name of city or name of city followed by _#
  #column_with_type= (string) name a column in data that indicates type of bike lane
  #shared, painted_lane, protected_lane_or_bvld, separated_path= (lst of strings) 
    #indicating variables in column_with_type
  #column_indicating_existing= (string) column name that indicates if the data is only proposed
  #var_indicating_existing= (string) variable name that indicates in column_indicating_existing that data is existing (ie not just proposed)

#NOTE: LAS VEGAS, DC_1, AND COLUMBUS LACK GOOD META-DATA, WILL EMAIL THEIR GIS PEOPLE


read_bike_lanes<- function(folder_name, 
                           column_with_type, shared_lane, painted_lane,
                           protected_lane, bvld, separated_path, 
                           column_indicating_existing, var_indicating_existing) {
  location <- paste("data_raw/city_bike_lanes/", folder_name, sep = "")
  
  if(missing(column_with_type) & missing(column_indicating_existing)){
    city_bike <- st_read(location) %>%
      select() %>%
      mutate(type="unknown") %>%
      st_zm(drop = TRUE) %>%
      st_transform(2163) %>%
      mutate(city = folder_name)
  }
  
  else if(missing(column_indicating_existing)) {
    city_bike <- st_read(location) %>%
      select(column_with_type) %>%
      rename(type=column_with_type) %>%
      mutate(type = case_when(
        type %in% shared_lane  ~"shared_lane",
        type %in% painted_lane  ~"painted_lane",
        type %in% protected_lane  ~"protected_lane",
        type %in% separated_path  ~"separated_path",
        type %in% bvld ~ "bvld")) %>%
      st_zm(drop = TRUE) %>%
      st_transform(2163) %>%
      mutate(city = folder_name)
    
  } else if(missing(column_with_type)){
      city_bike <- st_read(location) 
      city_bike <- city_bike[city_bike[[column_indicating_existing]] == var_indicating_existing, ]
      city_bike <- city_bike %>%    
        select() %>%
        mutate(type="unknown") %>%
        st_zm(drop = TRUE) %>%
        st_transform(2163) %>%
        mutate(city = folder_name)
    
  } else {
    city_bike <- st_read(location) 
    city_bike <- city_bike[city_bike[[column_indicating_existing]] == var_indicating_existing, ]
    city_bike <- city_bike %>%    
      select(column_with_type) %>%
      rename(type=column_with_type) %>%
      mutate(type = case_when(
        type %in% shared_lane  ~"shared_lane",
        type %in% painted_lane  ~"painted_lane",
        type %in% protected_lane  ~"protected_lane",
        type %in% separated_path  ~"separated_path",
        type %in% bvld ~ "bvld")) %>%
      st_zm(drop = TRUE) %>%
      st_transform(2163) %>%
      mutate(city = folder_name)
  }
}
  
#Call read_bike_lanes_function:
atlanta <- read_bike_lanes("atlanta", column_with_type = "facil", 
                           shared_lane = c("Sharrows"), 
                           painted_lane = c("Bike Lane"),
                           protected_lane = c("Protected Bike Lane"),
                           separated_path = c("Mixed Use Path"),
                           bvld = c(""))

baltimore <- read_bike_lanes("baltimore", column_with_type = "category", 
                             shared_lane = c("Shared Lane Markings or Signage", "Shared Bus/Bike Lane"), 
                             painted_lane= c("Bike Lane"), 
                             protected_lane = c("Separated Bike Lane"),
                             separated_path = c("Gravel Path", "Path or Sidepath"),
                             bvld = c("Bike Boulevard"))

boston <- read_bike_lanes("boston", column_with_type = "ExisFacil", 
                             shared_lane = c("BSBL", "NW-U", "SLM", "SRd", "SUB", "SUP", "TC"), 
                             painted_lane= c("BFBL", "BL", "BLSL", "CFBL", "NW"), 
                             protected_lane =c("SBL", "SBLBL", "WALK"), 
                             separated_path=c("NSUP"),
                             bvld = c(""))

buffalo <- read_bike_lanes("buffalo", column_with_type = "facility", 
                             shared_lane = c("On Road - Shared-Lane Markings"), 
                             painted_lane= c("On Road - Dedicated Bike Lanes", "On Road - Dedicated Lanes with Contraflow"), 
                             protected_lane=c(""), 
                             separated_path=c("Off Street - Multi-Use Path"),
                             bvld = c(""))

charlotte_1 <- read_bike_lanes("charlotte_1") %>%
                mutate(type = "shared_lane")

charlotte_2 <- read_bike_lanes("charlotte_2", column_with_type = "Bike_Lanes", 
                               shared_lane = c("Shoulder"), 
                               painted_lane= c("Standard", "Buffered"), 
                               protected_lane=c("Separated"), 
                               separated_path=c(""),
                               bvld = c(""))

chicago <- read_bike_lanes("chicago", column_with_type = "displayrou", 
                            shared_lane = c("SHARED-LANE"), 
                            painted_lane= c("BIKE LANE", "BUFFERED BIKE LANE"), 
                            protected_lane=c("PROTECTED BIKE LANE"), 
                            bvld = c(""),
                            separated_path=c("OFF-STREET TRAIL", "ACCESS PATH", "NEIGHBORHOOD GREENWAY"))

cleveland <- read_bike_lanes("cleveland", column_with_type = "SUB_TYPE", 
                            shared_lane = c("SIGNED", "SHARROWS", "PAVED SHOULDER", "SHARROWS"), 
                            painted_lane= c("LANE", "BUFFERED"), 
                            protected_lane=c("SEPARATED"), 
                            separated_path=c("APT"), 
                            bvld = c("Bike Boulevard", "Bicycle Boulevard"), 
                            column_indicating_existing="PHASE", var_indicating_existing="EXISTING") %>%
                            filter(!is.na(type))



#COLUMBUS METADATA UNCLEAR RETURN HERE
columbus <- read_bike_lanes("columbus", column_with_type = "BBP_CLASS", 
                            shared_lane = c("Route", "Paved Shoulder",
                                       "Shared Lane Markings", "Route"), 
                            painted_lane= c("Lane", "Lane Road Widening", 
                                            "Lane Road Diet"), 
                            protected_lane=c(""), 
                            separated_path=c("Path", 'PATH'), 
                            bvld = c("Bike Boulevard", "Bicycle Boulevard"), 
                            column_indicating_existing="FACILITYST", var_indicating_existing="EX")

dallas_1 <- read_bike_lanes("dallas_1", column_with_type = "fac_type", 
                            shared_lane = c("Sharrow"), 
                            painted_lane= c("Bike Lanes", "Buffered Bike Lane"), 
                            protected_lane=c("Cycle Track"), 
                            separated_path=c("OffStreet"), 
                            bvld = c(""))

dallas_2 <- read_bike_lanes("dallas_2", column_with_type = "fac_type", 
                            shared_lane = c("Sharrow", "Sharrows"), 
                            painted_lane= c("Bike Lanes", "Buffered Bike Lane", "Bike Lane", "Buf BL/CT"), 
                            protected_lane=c("Cycle Track"), 
                            separated_path=c("OffStreet"), 
                            bvld = c(""))

dc_2 <- read_bike_lanes("dc_2") %>%
  mutate(type = "separated_path")

denver <- read_bike_lanes("denver", column_with_type = "EXISTING_F", 
                          shared_lane = c("Shared Roadway"), 
                             painted_lane= c("Shared Use Path", "Protected Bike Lane"), 
                             protected_lane=c("Protected Bike Lane"), 
                             separated_path=c("Shared Use Path", "Trail"), 
                             bvld = c("Neighborhood Bikeway"))

detroit_1 <- read_bike_lanes("detroit_1", column_with_type = "TYPE", 
                             shared_lane = c("SHARROW"), 
                             painted_lane= c("BIKE LANE"), 
                             protected_lane=c(""), 
                             separated_path=c("GREENWAY", "INNER CIRCLE GREENWAY"), 
                             bvld = c(""), 
                             column_indicating_existing= "STATUS", var_indicating_existing="COMPLETED")



detroit_2 <- read_bike_lanes("detroit_2", column_with_type = "TRAIL_TYPE", 
                               shared_lane = c(""), 
                               painted_lane= c(""), 
                               protected_lane=c(""), 
                               separated_path=c("8 ft Fines Shared-use Path", "10ft Fines Shared-use Path", 
                                                       "10 ft Shared-use Path", "8 ft Shared-use Path"), 
                               bvld = c("")) %>%
                               filter(!is.na(type))

ft_worth <- read_bike_lanes("ft_worth", column_with_type = "atp_bike_f", 
                             shared_lane = c("Existing Signed Route"), 
                             painted_lane= c("Existing Bicycle Facility"), 
                             protected_lane=c("Existing Sidepath"), 
                             separated_path=c("Existing Trail", "Existing Natural Surface Trail"), 
                             bvld = c(""), 
                             column_indicating_existing="future", var_indicating_existing='0') %>%
                             filter(!is.na(type))

houston <- read_bike_lanes("houston", column_with_type = "B_Type_Exi", 
                             shared_lane = c("Signed Bike Route", "Signed Shared Roadway"), 
                             painted_lane= c("Bike Lane"), 
                             protected_lane=c("Cycle Track"), 
                             separated_path=c("Shared-Use Path"), 
                             bvld = c(""), 
                             column_indicating_existing="Bicycle", var_indicating_existing="Existing On-Street")

indianapolis_1 <- read_bike_lanes("indianapolis_1",
                             shared_lane = c(""), 
                             painted_lane= c(" "), 
                             protected_lane=c(""), 
                             separated_path=c(""), 
                             bvld = c(""), 
                             column_indicating_existing="OPER_STATU",
                             var_indicating_existing="BUILT") %>%
                             mutate(type = "painted_lane")

indianapolis_2 <- read_bike_lanes("indianapolis_2", column_with_type = "TYPE",
                             shared_lane = c(""), 
                             painted_lane= c("BIKELANE"), 
                             protected_lane=c(""), 
                             separated_path=c("MULTIUSE", "GREENWAY"), 
                             bvld = c("NEIGHBORWAY"), 
                             column_indicating_existing="STATUS",
                             var_indicating_existing=c("Proposed-MDCAdopted", "Proposed-Other")) 

jacksonville_1 <- read_bike_lanes("jacksonville_1", column_with_type = "DESCR", 
                             shared_lane = c("UNDESIGNATED (OBSOLETE)", "SHARROW"), 
                             painted_lane= c("DESIGNATED", "BUFFERED", "COLORED", "BOTH 2 AND 3"), 
                             protected_lane=c(""), 
                             separated_path=c(""), 
                             bvld = c(""))

jacksonville_2 <- read_bike_lanes("jacksonville_2") %>%
  mutate(type = "separated_path")

las_vegas <- read_bike_lanes("las_vegas", column_with_type = "TYPE", 
                             shared_lane = c(""), 
                             painted_lane= c("1", "6"), 
                             protected_lane=c(""), 
                             separated_path=c("0"), 
                             bvld = c(""))


los_angeles <- read_bike_lanes("los_angeles", column_with_type = "Bikeway", 
                             shared_lane = c("Sharrowed Route", "Detour Sharrowed Route", "Temp Removal Sharrowed Route", "Route"), 
                             painted_lane= c("Buffer Bike Lane", "Lane"), 
                             protected_lane=c("Protected Bike Lane"), 
                             separated_path=c("Path"), 
                             bvld = c("Bicycle Friendly Street"))


louisville <- read_bike_lanes("louisville", column_with_type = "PLAN_TYPE", 
                             shared_lane = c("Sharrows"), 
                             painted_lane= c("Bike Lanes"), 
                             protected_lane=c(""), 
                             separated_path=c("Shared Use Path", "Offroad Trails"), 
                             bvld = c(""))


miami_1 <- read_bike_lanes("miami_1") %>%
  mutate(type = "painted_lane")

miami_2 <- read_bike_lanes("miami_2") %>%
  mutate(type = "separated_path")

milwaukee_1 <- read_bike_lanes("milwaukee_1",
                               column_with_type = "Y_2020_FAC",
                               shared_lane = c("SHARROWS", "BIKE ROUTE"), 
                               painted_lane= c("BUFFERED BIKE LANE", "BIKE LANE"), 
                               protected_lane=c("PROTECTED BIKE LANE"), 
                               separated_path=c("Shared Use Path", "Offroad Trails"), 
                               bvld = c("BIKE BOULEVARD"))


new_orleans <- read_bike_lanes("new_orleans", column_with_type = "FacilityTy", 
                             shared_lane = c("Shared Lane", "Bus/Bike Lane", "Signed Route"), 
                             painted_lane= c("Bike Lane", "Buffered Bike Lane"), 
                             protected_lane=c("Protected Bike Lane"), 
                             separated_path=c("Shared Use Path"), 
                             bvld = c("Bike Boulevard"))

oklahoma_city <- read_bike_lanes("oklahoma_city", column_with_type = "Facility_T",
                                 shared_lane = c("Shared Route"),
                                 painted_lane = c("Bike Lane"),
                                 separated_path = c("Multi-use Trail"),
                                 protected_lane = c(), 
                                 bvld = c()) %>%
  filter(!is.na(type))


orlando <- read_bike_lanes("orlando", column_with_type = "Type", 
                             shared_lane = c("Signed Route"), 
                             painted_lane= c("Bike Lane"), 
                             protected_lane=c("Off Street"), 
                             separated_path=c(""), 
                             bvld = c(""))


philadelphia_1 <- read_bike_lanes("philadelphia_1", column_with_type = "TYPE", 
                                shared_lane = c("Sharrow", "Bus Bike Lane"), 
                                painted_lane= c("Conventional", "Paint Buffered", 
                                                "Two Way Unprotected Bicycle Lane",
                                                "Conventional w Sharrows",
                                                "Contraflow w Conventional, same",
                                                "Paint Buffered w Conventional"), 
                                protected_lane=c("Two Way Protected Bicycle Lane",
                                                 "One Way Protected Bicycle Lane"), 
                                separated_path=c(""), 
                                bvld = c(""))

phoenix <- read_bike_lanes("phoenix", column_with_type = "Bikeway_Ty", 
                             shared_lane = c("BIKE ROUTE", "BUS BIKE LANE"), 
                             painted_lane= c("BIKE LANE"), 
                             protected_lane=c(""), 
                             separated_path=c("BIKE PATH - PAVED", "SHARED USE PATH", "BIKE PATH - UNPAVED", "UNPAVED SHARED USE"), 
                             bvld = c("BIKE BLVD"))

portland <- read_bike_lanes("portland", column_with_type = "Facility", 
                             shared_lane = c("ABL"), 
                             painted_lane= c("BL", "BBL"), 
                             protected_lane=c("PBL", "SIR"), 
                             separated_path=c("NG", "TRL"), 
                             bvld = c("ESR", "LSB"), 
                             column_indicating_existing="Status",
                             var_indicating_existing= "ACTIVE")


raleigh <- read_bike_lanes("raleigh", column_with_type = "Type", 
                             shared_lane = c("Shared Lane Markings"), 
                             painted_lane= c("Bike Lane", "Buffered Bike Lane", 
                                             "Bike Lane/Shared Lane Markings", 
                                             "Buffered Bike Lane/Shared Lane Markings",
                                             "Buffered Bike Lane/Bike Lane"), 
                             protected_lane=c("Separated Bike Lane"), 
                             separated_path=c("Multi-Use Path"), 
                             bvld = c(""))

sacramento <- read_bike_lanes("sacramento", column_with_type = "Type", 
                             shared_lane = c("Route"), 
                             painted_lane= c("Lane"), 
                             protected_lane=c("Protected"), 
                             separated_path=c("Path"), 
                             bvld = c(""))

san_antonio <- read_bike_lanes("san_antonio", column_with_type = "BikeFacili", 
                             shared_lane = c("Route", "Shoulder"), 
                             painted_lane= c("Bike Lane", "Buffered Lane"), 
                             protected_lane=c("Separated Lane", "Cycletrack"), 
                             separated_path=c("Multi Use Path"), 
                             bvld = c(""))

san_diego <- read_bike_lanes("san_diego", column_with_type = "class", 
                             shared_lane = c("Bike Route"), 
                             painted_lane= c("Bike Lane"), 
                             protected_lane=c(""), 
                             separated_path=c("Multi-Use Path", "Muliti-Use Path"), 
                             bvld = c("")) %>%
                          filter(!is.na(type))
              
san_francisco <- read_bike_lanes("san_francisco", column_with_type = "symbology", 
                             shared_lane = c("BIKE ROUTE"), 
                             painted_lane= c("BIKE LANE"), 
                             protected_lane=c("SEPARATED BIKEWAY"), 
                             separated_path=c("BIKE PATH"), 
                             bvld = c("NEIGHBORWAY"))

san_jose <- read_bike_lanes("san_jose", column_with_type = "EXISTINGBI", 
                             shared = c("Class 3 (Sharrow)"), 
                             painted_lane= c("Class 2 (Basic)", "Class 2 (Buffered)"), 
                             protected_lane=c("Class 4"), 
                             separated_path=c("Class 1"), 
                             bvld = c("Class 3 (Bike Blvd)"))

seattle <- read_bike_lanes("seattle", column_with_type = "EXISTING_F", 
                             shared_lane = c("Sharrow", "Sharow"), 
                             painted_lane= c("In Street, Major Separation", "In Street, Minor Separation"), 
                             protected_lane=c(""), 
                             separated_path=c("Multi-use Trail"), 
                             bvld = c("Neighborhood Greenway"))


st_louis_1 <- read_bike_lanes("st_louis_1", column_with_type = "External_S", 
                            shared_lane = c(""), 
                            painted_lane= c(""), 
                            protected_lane=c(""), 
                            separated_path=c("Existing"), 
                            bvld = c(""))

st_louis_2 <- read_bike_lanes("st_louis_2") %>%
  mutate(type = "separated_path") 

st_louis_3 <- read_bike_lanes("st_louis_3", column_with_type = "FAC_TYPE", 
                         shared_lane = c("Paved Shoulder", "Shared Lane Markings", 
                                    "Wide Outside Lane", "Share the Road sign",
                                    "Designated Bike Route", "Paved Shoulder/Bike Lane",
                                    "Shared Lane"), 
                         painted_lane= c("Bike Lane", "Buffered Bike Lane", 
                                         "Climbing Lane"), 
                         protected_lane=c("Separated Bike Lane"), 
                         separated_path=c("Multi-Use Path"), 
                         bvld = c("Bike Blvd"), 
                         column_indicating_existing="F_STATU",
                         var_indicating_existing="Existing")



st_paul <- read_bike_lanes("st_paul", column_with_type = "type", 
                             shared_lane = c("Shared Lane", "Shoulder"), 
                             painted_lane= c("Bike Lane"), 
                             protected_lane=c(""), 
                             separated_path=c("Trail"), 
                             bvld = c("Bike Blvd"))

tampa <- read_bike_lanes("tampa", column_with_type = "TYPE", 
                             shared_lane = c("Shared Lane Markings", "Paved Shoulder", "Shared Lane Signs"), 
                             painted_lane= c("Buffered Bicycle Lane", 'Bicycle Lane'), 
                             protected_lane=c("Cycle Track"), 
                             separated_path=c("Multi-Use Path"), 
                             bvld = c(""), 
                             column_indicating_existing="BIKELANEST",
                             var_indicating_existing="Existing")


virginia_beach <- read_bike_lanes("virginia_beach", column_with_type = "TYPE", 
                             shared_lane = c("SRS", "SRW"), 
                             painted_lane= c("WSW"), 
                             protected_lane=c(""), 
                             separated_path=c("MUP"), 
                             bvld = c(""))


#Reading/Processing the less standardized files =====

#dc_1 
dc_1 <- st_read("data_raw/city_bike_lanes/dc_1/") %>%
  mutate(SLOWSTREET = ifelse(is.na(SLOWSTREET), "", "bvld")) %>%
  mutate(type = case_when(
    SLOWSTREET == "bvld" ~ "bvld", 
    BIKELANE_C %in% c("OB", "IB", "BD")  ~"painted_lane",
    BIKELANE_2 %in% c("OB", "IB", "BD")   ~"painted_lane",
    BIKELANE_D %in% c("OB", "IB", "BD")   ~"protected_lane",
    BIEKLANE_D %in% c("OB", "IB", "BD")   ~"painted_lane",
    BIKELANE_3 %in% c("OB", "IB", "BD")  ~ "protected_lane",
    BIKELANE_B %in% c("OB", "IB", "BD")  ~ "protected_lane")) %>%
  select(type) %>%
  filter(!is.na(type)) %>%
  mutate(city = "dc_1") %>%
  st_transform(2163)%>%
  st_zm(drop = TRUE) 
  
#Kansas City
kansas_city <- st_read("data_raw/city_bike_lanes/kansas_city/a00000009.gdbtable") %>%
  st_zm(drop = TRUE) %>%
  st_transform(2163) %>%
  mutate(type = case_when(
    Facility_Type %in% c("SBR")  ~"shared_lane",
    Facility_Type %in% c("BL")  ~"painted_lane",
    Facility_Type %in% c("Separated-Use Trail", "On-Street Protected Bike Lane", "Cycle Track")  ~"protected_lane",
    Facility_Type %in% c("SUP")  ~"separated_path",
    Facility_Type %in% c("Bike Boulevard")  ~"bvld")) %>%
  select(type) %>%
  mutate(city = "Kansas City") %>%
  rename(geometry = Shape)

#Memphis:
#Note: Used command line to unzip the .lpk file
#Help on doing that here: https://gis.stackexchange.com/questions/34310/opening-lyr-file-via-rgdal-ogr

memphis <- st_read("data_raw/city_bike_lanes/memphis/v105/bike_ped__facility_data.gdb") %>%
  st_zm(drop = TRUE) %>%
  st_transform(2163) %>%
  mutate(type = case_when(
    FACILITY_C %in% c("Marked Shared Roadway", "Signed Shared Roadway", 
                      "Paved Shoulder", "Marked Shared Lane")  ~"shared_lane",
    FACILITY_C %in% c("Bike Lane", "Buffered Bike Lane")  ~"painted_lane",
    FACILITY_C %in% c("Cycle Track")  ~"protected_lane",
    FACILITY_C %in% c("Shared Use Path", "Dirt Trail")  ~"separated_path")) %>%
  select(type) %>%
  mutate(city = "memphis") %>%
  rename(geometry = Shape)


milwaukee_2 <- st_read("data_raw/city_bike_lanes/milwaukee_2") %>%
  st_transform(2163) %>%
  filter(Status == "E") %>%
  select() %>%
  mutate(type = "separated_path") %>%
  mutate(city = "memphis")


#Minneapolis
minneapolis <- st_read("data_raw/city_bike_lanes/minneapolis") %>%
  st_zm(drop = TRUE) %>%
  st_transform(2163) %>%
  filter(FACILITY != "Sidewalk") %>%
  filter(TRLSTATUS == "Open") %>%
  mutate(type = case_when(
    SUMMER_USE %in% c("Bikeable Shoulder", "Advisory Bike Lane", "Sharrow/Shared Lane")  ~"shared_lane",
    SUMMER_USE %in% c("Standard Bike Lane", "Buffered Bike Lane", "Contraflow Bike Lane")  ~"painted_lane",
    SUMMER_USE %in% c("Separated-Use Trail", "On-Street Protected Bike Lane", "Cycle Track")  ~"protected_lane",
    SUMMER_USE %in% c("Multi-Use Trail", "Bike/Pedestrian Tunnel", 'Bike/Pedestrian Bridge')  ~"separated_path",
    SUMMER_USE %in% c("Bike Boulevard")  ~"bvld")) %>%
  select(type) %>%
  mutate(city = "minneapolis")

#nashville lanes:
nashville_1 <-st_read("data_raw/city_bike_lanes/nashville/BikewayExport011121.shp") %>%
  st_transform(2163) %>%
  st_zm(drop = TRUE) %>%
  mutate(type = case_when(
    BikewayTyp %in% c("WOL", "SSR") ~"shared_lane",
    BikewayTyp =="BL" ~"painted_lane",
    BikewayTyp == "BBL" ~"protected_lane")) %>%
  select(type) %>%
  mutate(city = "nashville_1")

#nashville trails:
nashville_2 <-st_read('data_raw/city_bike_lanes/nashville/ExistingTrails_Export011121.shp') %>%
  st_transform(2163) %>%
  st_zm(drop = TRUE) %>%
  filter(STATUS == "Existing") %>%
  filter(TYPE == "Multi-use" | (TYPE == "Park Trail" & SURFACE %in% c("PAVED, ASPHAULT"))) %>%
  select() %>%
  mutate(type = "separated_path") %>%
  mutate(city = "nashville_2")

#New York
new_york <- st_read(dsn = "data_raw/city_bike_lanes/new_york/a00000009.gdbtable") %>%
  st_transform(2163) %>%
  filter(BIKING == "Y") %>%
  mutate(type = case_when(
    TRAIL_HWY_ %in% c("Shared Roadway", "Shoulder Lane")  ~"shared_lane",
    TRAIL_HWY_ == "Dedicated Lane"  ~"painted_lane",
    TRAIL_HWY_ == "Off Road" ~ "separated_path")) %>%
  select(type) %>%
  mutate(city = "new_york") %>%
  rename(geometry = Shape)

#Philadelphia Connector streets
philadelphia_2 <- st_read("data_raw/city_bike_lanes/philadelphia_2/PhiladelphiaBikeConnectorStreets201204.shp") %>%
  st_transform(2163) %>%
  mutate(type = case_when(
    ExisBikFac %in% c("FRIENDLY", "Sharrow")  ~"shared_lane",
    ExisBikFac == "Bike Lane(s)"  ~"painted_lane")) %>%
  select(type) %>%
  filter(!is.na(type)) %>%
  mutate(city = "philadelphia_2")

#Philadelphia Regional Connectors:
philadelphia_3 <- st_read("data_raw/city_bike_lanes/philadelphia_2/PhiladelphiaBikeRegionalConnections201204.shp") %>%
  st_transform(2163) %>%
  mutate(type = case_when(
    STATUS == "Not Signed, Designated"  ~"shared_lane",
    STATUS %in% c("Completed", "Signed", "Completed, Dirt") ~ "separated_path")) %>%
  select(type) %>%
  mutate(city = "philadelphia_3")
  
#Philadelphia Trails Paths:
philadelphia_4 <- st_read("data_raw/city_bike_lanes/philadelphia_2/PhiladelphiaBikeTrailsSidepaths201209.shp") %>%
  st_transform(2163) %>%
  select() %>%
  mutate(type = "separated_path") %>%
  mutate(city = "philadelphia_4")
  
#Pittsburgh Lanes:
pittsburgh_1 <- st_read("data_raw/city_bike_lanes/pittsburgh/Bike\ Lanes.shp") %>%
  st_transform(2163) %>%
  select() %>%
  mutate(type = "painted_lane")%>%
  mutate(city = "pittsburgh_1")

#Pittsburgh Cautionary Routes:
pittsburgh_2 <- st_read("data_raw/city_bike_lanes/pittsburgh/Cautionary Bike Route.shp") %>%
  st_transform(2163) %>%
  select() %>%
  mutate(type = "shared_lane")%>%
  mutate(city = "pittsburgh_2")
  
#Pittsburgh On street Routes
pittsburgh_3 <- st_read("data_raw/city_bike_lanes/pittsburgh/On Street Bike Route.shp") %>%
  st_transform(2163) %>%
  select() %>%
  mutate(type = "shared_lane")%>%
  mutate(city = "pittsburgh_3")

#Protected Bike Lane:
pittsburgh_4 <- st_read("data_raw/city_bike_lanes/pittsburgh/Protected Bike Lane.shp") %>%
  st_transform(2163) %>%
  select() %>%
  mutate(type = "protected_lane")%>%
  mutate(city = "pittsburgh_4")

#Sharrow:
pittsburgh_5 <- st_read("data_raw/city_bike_lanes/pittsburgh/Sharrows.shp") %>%
  st_transform(2163) %>%
  select() %>%
  mutate(type = "shared_lane")%>%
  mutate(city = "pittsburgh_5")

#Path
pittsburgh_6 <- st_read("data_raw/city_bike_lanes/pittsburgh/trails.shp") %>%
  st_transform(2163) %>%
  select() %>%
  mutate(type = "separated_path")%>%
  mutate(city = "pittsburgh_6")

#Providence:
providence <- st_read("data_raw/city_bike_lanes/providence") %>%
  st_transform(2163) %>%
  filter(Status %in% c("Existing", "Planned")) %>%
  mutate(type = case_when(
    Type == "Separated Urban Trail" ~ "separated_path",
    Type == "Striped Bike Lane" ~ "painted_lane",
    Type == "Striped Bike Lane" ~ "shared_lane"
  )) %>%
  select(type) %>%
  mutate(city = "providence")


#Salt Lake City Roads:
salt_lake_1 <- st_read("data_raw/city_bike_lanes/salt_lake_city_1")
salt_lake_1 <- salt_lake_1 %>%
  st_transform(2163) %>%
  filter(!is.na(BIKE_L) | !is.na(BIKE_R)) %>%
  mutate(bike = coalesce(BIKE_L, BIKE_R)) %>% 
  mutate(type = case_when(
    bike %in% c("3A", "3B", "3C", "3")  ~"shared_lane",
    bike %in% c("2A", "2B", "2")  ~"painted_lane",
    bike %in% c("1A", "1B", "1C", "1")  ~"protected_lane",
    bike %in% c("PP", "PU")  ~"separated_path",
    bike %in% c("")  ~"bvld")) %>%
  mutate(city = "salt_lake_1") %>%
  select(city, type) 

#Salt Lake City Paths:
salt_lake_2 <- st_read("data_raw/city_bike_lanes/salt_lake_city_2") %>%
  st_transform(2163) %>%
  filter(Class == "Path") %>%
  filter(Status == "EXISTING") %>%
  filter(CartoCode == "4 - Road-concurrent" | 
           CartoCode == "3 - Paved Shared Use" |
           CartoCode == "3 - Paved Shared Use" ) %>%
  filter(County == "SALT LAKE") %>%
  select() %>%
  mutate(type = "separated_path") %>%
  mutate(city = "salt_lake_2")
  
  
  
#Combine all the city dataframes ======
city_bikes <- rbind(atlanta, baltimore, boston, buffalo, charlotte_1, 
                    charlotte_2, chicago, cleveland, columbus, dallas_1, dallas_2,
                    dc_1, dc_2, denver, detroit_1, detroit_2, ft_worth, 
                    houston, indianapolis_1, indianapolis_2, 
                    jacksonville_1, jacksonville_2, kansas_city, las_vegas, 
                    los_angeles, louisville, memphis, miami_1, miami_2, milwaukee_1, milwaukee_2, 
                    minneapolis, nashville_1, nashville_2,new_orleans, 
                    new_york, oklahoma_city, orlando, philadelphia_1, philadelphia_2, philadelphia_3, 
                    philadelphia_4, phoenix, pittsburgh_1, pittsburgh_2,
                    pittsburgh_3, pittsburgh_4, pittsburgh_5, pittsburgh_6,
                    portland, providence, raleigh, sacramento, salt_lake_1, 
                    salt_lake_2, san_antonio, san_diego, san_francisco, san_jose, 
                    seattle, st_louis_1, st_louis_2, st_louis_3, st_paul, tampa, 
                    virginia_beach)
city_bikes <- city_bikes %>%
  mutate(type = ifelse(is.na(type), "unknown", type)) 


st_write(city_bikes, "data_in_progress/city_bikes.geojson")
  
