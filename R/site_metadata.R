# ---------------------------------------------------------------------------
# NEON AQUATIC site metadata (the 34 sites in the macroinvertebrate bundle).
# Lookup of site code -> human-readable name, NEON domain, state, coordinates,
# aquatic type (lake / river / stream), and a one-line bio so the picker teaches
# as you choose. Source: NEON aquatic field-site descriptions
# (https://www.neonscience.org/field-sites). Coordinates come from the bundle
# (meta$lat / meta$lng), so the picker map uses the real reach location.
# ---------------------------------------------------------------------------

neon_sites <- tibble::tribble(
  ~site,  ~domain, ~name,                                   ~state, ~type,    ~bio,
  "ARIK", "D10",  "Arikaree River",                         "CO",   "river",  "A shallow High Plains river on the Colorado eastern plains that runs warm and often goes dry in late summer.",
  "BARC", "D03",  "Lake Barco",                             "FL",   "lake",   "A clear, acidic sandhill lake on the Ordway-Swisher reserve in north-central Florida.",
  "BIGC", "D17",  "Upper Big Creek",                        "CA",   "stream", "A cold, steep granite headwater stream in the southern Sierra Nevada of California.",
  "BLDE", "D12",  "Blacktail Deer Creek",                   "WY",   "stream", "A clear mountain stream in the sagebrush and forest of northern Yellowstone, Wyoming.",
  "BLUE", "D11",  "Blue River",                             "OK",   "stream", "A spring-fed stream over limestone in the Arbuckle Mountains of south-central Oklahoma.",
  "BLWA", "D08",  "Black Warrior River (Lower Flint)",      "AL",   "river",  "A warm, slow lowland river in the coastal plain of west-central Alabama.",
  "CARI", "D19",  "Caribou Creek",                          "AK",   "stream", "A boreal stream draining permafrost-influenced black-spruce country near Fairbanks, Alaska.",
  "COMO", "D13",  "Como Creek",                             "CO",   "stream", "A cold, high-elevation snowmelt stream below Niwot Ridge in the Colorado Front Range.",
  "CRAM", "D05",  "Crampton Lake",                          "WI",   "lake",   "A small, clear north-temperate forest lake in the Northwoods of Wisconsin.",
  "CUPE", "D04",  "Rio Cupeyes",                            "PR",   "stream", "A warm, fast tropical-forest stream in the mountains of western Puerto Rico.",
  "FLNT", "D03",  "Flint River",                            "GA",   "river",  "A warm coastal-plain river over limestone in southwestern Georgia.",
  "GUIL", "D04",  "Rio Yahuecas (Guilarte)",               "PR",   "stream", "A cool, steep tropical headwater stream in the central mountains of Puerto Rico.",
  "HOPB", "D01",  "Lower Hop Brook",                        "MA",   "stream", "A shaded New England forest stream in central Massachusetts.",
  "KING", "D06",  "Kings Creek",                            "KS",   "stream", "A tallgrass-prairie stream on the Konza Prairie of the Kansas Flint Hills.",
  "LECO", "D07",  "LeConte Creek",                          "TN",   "stream", "A cold, clear mountain stream in Great Smoky Mountains National Park, Tennessee.",
  "LEWI", "D02",  "Lewis Run",                              "VA",   "stream", "A small Blue Ridge foothill stream in the Virginia Piedmont.",
  "LIRO", "D05",  "Little Rock Lake",                       "WI",   "lake",   "A clear, low-nutrient seepage lake in the forests of northern Wisconsin.",
  "MART", "D16",  "Martha Creek",                           "WA",   "stream", "A cold, forested Cascade-foothill stream in southern Washington.",
  "MAYF", "D08",  "Mayfield Creek",                         "AL",   "stream", "A warm coastal-plain stream draining loblolly-pine and hardwood country in Alabama.",
  "MCDI", "D06",  "McDiffett Creek",                        "KS",   "stream", "A small tallgrass-prairie stream in the Kansas Flint Hills.",
  "MCRA", "D16",  "McRae Creek",                            "OR",   "stream", "A cold, old-growth-forest stream in the H.J. Andrews Experimental Forest, Oregon.",
  "OKSR", "D18",  "Oksrukuyik Creek",                       "AK",   "stream", "A clear tundra stream on the Arctic foothills of Alaska's North Slope.",
  "POSE", "D02",  "Posey Creek",                            "VA",   "stream", "A small, clear stream in the Blue Ridge foothills of northern Virginia.",
  "PRIN", "D11",  "Pringle Creek",                          "TX",   "stream", "A warm Cross Timbers prairie stream in north-central Texas.",
  "PRLA", "D09",  "Prairie Lake",                           "ND",   "lake",   "A shallow prairie-pothole lake on the Northern Great Plains of North Dakota.",
  "PRPO", "D09",  "Prairie Pothole",                        "ND",   "lake",   "A shallow, productive prairie-pothole wetland lake in central North Dakota.",
  "REDB", "D15",  "Red Butte Creek",                        "UT",   "stream", "A montane stream draining the Wasatch Range above Salt Lake City, Utah.",
  "SUGG", "D03",  "Lake Suggs",                             "FL",   "lake",   "A clear, acidic sandhill lake on the Ordway-Swisher reserve in north-central Florida.",
  "SYCA", "D14",  "Sycamore Creek",                         "AZ",   "stream", "A Sonoran Desert stream northeast of Phoenix that floods in the monsoon and dries to pools between rains.",
  "TECR", "D17",  "Teakettle Creek",                        "CA",   "stream", "A cold, high-elevation conifer-forest stream in the central Sierra Nevada, California.",
  "TOMB", "D08",  "Lower Tombigbee River",                  "AL",   "river",  "A large, warm, slow river in the coastal plain of southwestern Alabama.",
  "TOOK", "D18",  "Toolik Lake",                            "AK",   "lake",   "A clear, cold arctic lake on the foothills of Alaska's North Slope.",
  "WALK", "D07",  "Walker Branch",                          "TN",   "stream", "A small, forested ridge-and-valley stream at Oak Ridge, Tennessee.",
  "WLOU", "D13",  "West St Louis Creek",                    "CO",   "stream", "A cold, high-elevation snowmelt stream in the Colorado Rockies near Fraser."
)

# full state name for grouping the picker
state_names <- c(
  AK = "Alaska", AL = "Alabama", AZ = "Arizona", CA = "California", CO = "Colorado",
  FL = "Florida", GA = "Georgia", KS = "Kansas", MA = "Massachusetts", ND = "North Dakota",
  OK = "Oklahoma", OR = "Oregon", PR = "Puerto Rico", TN = "Tennessee", TX = "Texas",
  UT = "Utah", VA = "Virginia", WA = "Washington", WI = "Wisconsin", WY = "Wyoming"
)

site_bio <- function(code) {
  row <- neon_sites[neon_sites$site == code, ]
  if (nrow(row) == 0) return(NULL)
  row$bio[1]
}
site_name <- function(code) {
  row <- neon_sites[neon_sites$site == code, ]
  if (nrow(row) == 0) return(code)
  row$name[1]
}
# vectorized name lookup (for the Search results table over many site codes)
site_name_vec <- function(codes) {
  nm <- neon_sites$name[match(codes, neon_sites$site)]
  ifelse(is.na(nm), codes, nm)
}
site_label <- function(code) {
  row <- neon_sites[neon_sites$site == code, ]
  if (nrow(row) == 0) return(code)
  sprintf("%s · %s, %s · NEON %s", row$name[1], row$site[1], row$state[1], row$domain[1])
}
