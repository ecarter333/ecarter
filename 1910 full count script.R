data <- read.csv("2010_ipums_fullct.gz")
data
View(data)

install.packages(c("sf", "dplyr", "leaflet", "tigris"))
library(sf)
library(dplyr)
library(leaflet)

write.csv(head(data, 50), "sample.csv", row.names = FALSE)

# ============================================================
# Portland 1910 Census — Data Processing & Leaflet Map
# IPUMS microdata → ED-level summary → interactive map
# (From Claude)
# ============================================================
# Prerequisites (run once):
# install.packages(c("tidyverse", "leaflet", "sf", "jsonlite",
#                    "htmlwidgets", "RColorBrewer", "scales"))

library(tidyverse)
library(leaflet)
library(sf)
library(jsonlite)
library(htmlwidgets)
library(scales)


library(sf)


# Check what we got
nrow(eds_1910)
names(eds_1910)
print(eds_1910)

library(sf)
library(tidyverse)


eds_1910 <- st_read("portland_eds_1910.geojson")
eds_1910 <- eds_1910 |>
  filter(ed_num >= 124 & ed_num <= 327)
# Load both layers
eds_1910 <- st_read("portland_eds_1910.geojson")
neighborhoods <- st_read("Neighborhoods_(regions).geojson")  # ← replace with actual filename
eds_1910 <- eds_1910 |>
  filter(ed_num != 43)

# Verify it's gone
eds_1910 |> 
  st_drop_geometry() |>
  filter(ed_num < 124 | ed_num > 327)
# Check neighborhood fields
names(neighborhoods)

# Make sure CRS matches
eds_1910 <- st_transform(eds_1910, crs = 4326)
neighborhoods <- st_transform(neighborhoods, crs = 4326)

# Fix any geometry issues
eds_1910 <- st_make_valid(eds_1910)
neighborhoods <- st_make_valid(neighborhoods)

# Rename neighborhood name field
neighborhoods <- neighborhoods |>
  rename(neighborhood = NAME)

# Intersect
ed_neighborhood_intersect <- st_intersection(eds_1910, neighborhoods) |>
  mutate(overlap_area = st_area(geometry)) |>
  st_drop_geometry()

# Assign each ED to its largest-overlap neighborhood
ed_to_neighborhood <- ed_neighborhood_intersect |>
  group_by(ed_num) |>
  slice_max(overlap_area, n = 1) |>
  ungroup() |>
  select(ed_num, neighborhood, overlap_area)

print(ed_to_neighborhood)

# Assign each ED to its largest-overlap neighborhood
ed_to_neighborhood <- ed_neighborhood_intersect |>
  group_by(ed_num) |>
  slice_max(overlap_area, n = 1) |>
  ungroup() |>
  select(ed_num, neighborhood, overlap_area)

print(ed_to_neighborhood)

# Which ED is missing?
all_eds <- eds_1910$ed_num
matched_eds <- ed_to_neighborhood$ed_num

missing_ed <- setdiff(all_eds, matched_eds)
cat("Missing ED:", missing_ed, "\n")

eds_1910 |> 
  st_drop_geometry() |>
  filter(ed_num < 124 | ed_num > 327) |>
  arrange(ed_num)

st_crs(eds_1910)
st_crs(neighborhoods)

print(ed_to_neighborhood)
nrow(ed_to_neighborhood)

# Add total area and calculate overlap percentage
ed_total_area <- eds_1910 |>
  mutate(total_area = st_area(geometry)) |>
  st_drop_geometry() |>
  select(ed_num, total_area)

ed_to_neighborhood <- ed_to_neighborhood |>
  left_join(ed_total_area, by = "ed_num") |>
  mutate(pct_in_neighborhood = as.numeric(overlap_area / total_area) * 100) |>
  mutate(across(where(is.numeric), \(x) round(x, 1)))

# Flag ambiguous ones
ambiguous_eds <- ed_to_neighborhood |>
  filter(pct_in_neighborhood < 75)

print(ambiguous_eds)

ed_to_neighborhood |>
  arrange(pct_in_neighborhood) |>
  print(n = 22)

library(tidyverse)

census_1910 <- read_csv("2010_ipums_fullct.gz")   # ← replace with your actual filename

census_1910 <- census_1910 |>
  mutate(
    year   = 1910,
    ed_num = as.integer(ENUMDIST %% 1000),
    
    race_label = case_when(
      RACE == 1 ~ "White",
      RACE == 2 ~ "Black",
      RACE == 3 ~ "American Indian",
      RACE == 4 ~ "Chinese",
      RACE == 5 ~ "Japanese",
      RACE == 6 ~ "Other Asian",
      RACE == 8 ~ "Two or more races",
      TRUE      ~ "Other/Unknown"
    ),
    
    nativity = case_when(
      MBPL >= 100 | FBPL >= 100 ~ "Immigrant stock",
      MBPL <  100 & FBPL <  100 ~ "Native stock",
      TRUE                       ~ "Unknown"
    ),
    
    occ_sector = case_when(
      OCC1950 %in% 0:099   ~ "Professional",
      OCC1950 %in% 100:123 ~ "Farmers (owners)",
      OCC1950 %in% 200:290 ~ "Managers & Proprietors",
      OCC1950 %in% 300:390 ~ "Clerical",
      OCC1950 %in% 400:490 ~ "Sales",
      OCC1950 %in% 500:595 ~ "Craftsmen & Foremen",
      OCC1950 %in% 600:690 ~ "Operatives",
      OCC1950 %in% 700:720 ~ "Private Household",
      OCC1950 %in% 730:790 ~ "Service Workers",
      OCC1950 %in% 810:840 ~ "Farm Laborers",
      OCC1950 %in% 910:970 ~ "Laborers",
      OCC1950 %in% 979:995 ~ "Non-occupational response",
      OCC1950 == 999       ~ "Not in labor force",
      TRUE                 ~ "Other"
    )
  )

# Attach neighborhood assignments
census_1910 <- census_1910 |>
  left_join(ed_to_neighborhood |> select(ed_num, neighborhood, pct_in_neighborhood),
            by = "ed_num")

# Check match rate
cat("Matched:", sum(!is.na(census_1910$neighborhood)), "\n")
cat("Unmatched:", sum(is.na(census_1910$neighborhood)), "\n")

neighborhood_1910 <- census_1910 |>
  filter(!is.na(neighborhood)) |>
  group_by(neighborhood) |>
  summarise(
    total_pop        = n(),
    pct_white        = mean(race_label == "White") * 100,
    pct_black        = mean(race_label == "Black") * 100,
    pct_chinese      = mean(race_label == "Chinese") * 100,
    pct_japanese     = mean(race_label == "Japanese") * 100,
    pct_imm_stock    = mean(nativity == "Immigrant stock") * 100,
    pct_native       = mean(nativity == "Native stock") * 100,
    pct_professional = mean(occ_sector == "Professional") * 100,
    pct_laborer      = mean(occ_sector == "Laborers") * 100,
    pct_service      = mean(occ_sector == "Service Workers") * 100,
    pct_craftsmen    = mean(occ_sector == "Craftsmen & Foremen") * 100,
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 1)))

print(neighborhood_1910)

neighborhood_1910_occ <- census_1910 |>
  filter(!is.na(neighborhood)) |>
  filter(OCC1950 != 999) |>        # labor force only
  group_by(neighborhood) |>
  summarise(
    labor_force_pop  = n(),
    pct_professional = mean(occ_sector == "Professional") * 100,
    pct_laborer      = mean(occ_sector == "Laborers") * 100,
    pct_service      = mean(occ_sector == "Service Workers") * 100,
    pct_craftsmen    = mean(occ_sector == "Craftsmen & Foremen") * 100,
    pct_managers     = mean(occ_sector == "Managers & Proprietors") * 100,
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 1)))

print(neighborhood_1910_occ)

census_1910 |>
  filter(!is.na(neighborhood)) |>
  filter(OCC1950 != 999) |>
  count(occ_sector) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  arrange(desc(pct))

census_1910 |>
  filter(!is.na(neighborhood)) |>
  filter(OCC1950 != 999) |>
  filter(occ_sector == "Other") |>
  count(OCC1950) |>
  arrange(desc(n))




# ============================================================
# 1. BIRTHPLACE → ANCESTRY LOOKUP TABLE
# ============================================================
# Maps IPUMS MBPL/FBPL codes to standardized ancestry labels
# Handles historical regions, empires, and pre-unification territories

bpl_to_ancestry <- function(code) {
  case_when(
    # --- United States (all states + territories) ---
    code %in% c(1:56, 90, 99, 100:120)   ~ "United States",
    
    # --- Canada ---
    code %in% c(150, 155, 160)            ~ "Canada",
    
    # --- Mexico / Central America / Caribbean ---
    code == 200                            ~ "Mexico",
    code %in% c(210, 250, 260)            ~ "Central America & Caribbean",
    
    # --- South America ---
    code == 300                            ~ "South America",
    
    # --- Scandinavia ---
    code %in% c(400, 404, 405)            ~ "Scandinavia (Norway/Sweden/Denmark)",
    code %in% c(401, 402, 403)            ~ "Finland/Iceland",
    
    # --- British Isles ---
    code %in% c(410, 411, 412, 413)       ~ "England/Scotland/Wales",
    code == 414                            ~ "Ireland",
    code == 419                            ~ "British Isles, n.s.",
    
    # --- Western Europe ---
    code == 421                            ~ "France",
    code %in% c(420, 423, 424)            ~ "Belgium/Luxembourg",
    code == 425                            ~ "Netherlands",
    code == 426                            ~ "Switzerland",
    code == 429                            ~ "Western Europe, n.s.",
    
    # --- Southern Europe ---
    code == 433                            ~ "Greece",
    code == 434                            ~ "Italy",
    code == 436                            ~ "Portugal",
    code == 438                            ~ "Spain",
    code %in% c(430, 432, 435, 437, 440)  ~ "Southern Europe, n.s.",
    
    # --- German-speaking (Germany + Austria + Switzerland already above) ---
    # Note: many "Germans" from Russia appear under Russia codes below
    code == 453                            ~ "Germany",
    code == 450                            ~ "Austria",
    
    # --- Central/Eastern Europe ---
    code == 452                            ~ "Czechoslovakia/Bohemia",
    code == 454                            ~ "Hungary",
    code == 455                            ~ "Poland",
    code == 456                            ~ "Romania",
    code == 457                            ~ "Yugoslavia/Serbia/Croatia",
    code == 451                            ~ "Bulgaria",
    code %in% c(458, 459)                 ~ "Central/Eastern Europe, n.s.",
    
    # --- Russian Empire / USSR ---
    # Includes many Jewish immigrants coded here in 1910
    code %in% c(460, 461, 462, 463)       ~ "Baltic States",
    code == 465                            ~ "Russia/Russian Empire",
    code == 499                            ~ "Europe, n.s.",
    
    # --- East Asia ---
    code == 500                            ~ "China",
    code == 501                            ~ "Japan",
    code == 502                            ~ "Korea",
    
    # --- Southeast Asia ---
    code == 515                            ~ "Philippines",
    code %in% c(510:519)                  ~ "Southeast Asia",
    
    # --- South Asia ---
    code == 521                            ~ "India",
    code %in% c(520:550)                  ~ "South/Southwest Asia",
    
    # --- Middle East ---
    code %in% c(534, 537, 541)            ~ "Syria/Lebanon/Palestine",
    code == 542                            ~ "Turkey/Asia Minor",
    code %in% c(530:549)                  ~ "Middle East, n.s.",
    
    # --- Africa ---
    code == 600                            ~ "Africa",
    
    # --- Oceania ---
    code %in% c(700, 710)                 ~ "Australia/Pacific",
    
    # --- Unknown/missing ---
    code %in% c(900, 950, 997, 999) |
      is.na(code)                          ~ NA_character_,
    
    TRUE                                   ~ "Other/Unknown"
  )
}


# ============================================================
# 2. CALCULATE FRACTIONAL ANCESTRY WEIGHTS PER PERSON
# ============================================================
# Assumes census_1910 is already loaded with MBPL, FBPL columns
# and neighborhood assignments from the main analysis script

compute_ancestry <- function(df) {
  df |>
    mutate(
      # Apply lookup to both parents
      manc = bpl_to_ancestry(MBPL),   # mother's ancestry
      fanc = bpl_to_ancestry(FBPL),   # father's ancestry
      
      # Classify US-born parents
      m_us = MBPL %in% c(1:56, 90, 99),
      f_us = FBPL %in% c(1:56, 90, 99),
      m_known = !is.na(manc),
      f_known = !is.na(fanc),
      
      # Determine weights
      # Both parents known and same ancestry → 1.0
      # Both parents known and different → 0.5 each (handled in pivot below)
      # One parent known, one missing → 0.5
      # Both unknown → exclude
      m_weight = case_when(
        m_known & f_known  ~ 0.5,
        m_known & !f_known ~ 0.5,
        TRUE               ~ 0
      ),
      f_weight = case_when(
        f_known & m_known  ~ 0.5,
        f_known & !m_known ~ 0.5,
        TRUE               ~ 0
      ),
      
      # Total known weight (for normalizing later)
      total_weight = m_weight + f_weight
    )
}

census_1910 <- compute_ancestry(census_1910)


# ============================================================
# 3. AGGREGATE ANCESTRY WEIGHTS TO NEIGHBORHOOD LEVEL
# ============================================================

ancestry_by_neighborhood <- function(df) {
  
  # Mother's side contribution
  mother_contrib <- df |>
    filter(!is.na(neighborhood), m_weight > 0, !is.na(manc)) |>
    group_by(neighborhood, ancestry = manc) |>
    summarise(weight = sum(m_weight), .groups = "drop")
  
  # Father's side contribution
  father_contrib <- df |>
    filter(!is.na(neighborhood), f_weight > 0, !is.na(fanc)) |>
    group_by(neighborhood, ancestry = fanc) |>
    summarise(weight = sum(f_weight), .groups = "drop")
  
  # Combine both parents
  combined <- bind_rows(mother_contrib, father_contrib) |>
    group_by(neighborhood, ancestry) |>
    summarise(weight = sum(weight), .groups = "drop")
  
  # Calculate total weights per neighborhood (denominator)
  totals <- df |>
    filter(!is.na(neighborhood)) |>
    group_by(neighborhood) |>
    summarise(total_weight = sum(total_weight), .groups = "drop")
  
  # Calculate percentages
  combined |>
    left_join(totals, by = "neighborhood") |>
    mutate(pct_ancestry = round(weight / total_weight * 100, 1)) |>
    select(neighborhood, ancestry, weight, pct_ancestry) |>
    arrange(neighborhood, desc(pct_ancestry))
}

ancestry_1910 <- ancestry_by_neighborhood(census_1910)

cat("Ancestry summary (1910):\n")
print(ancestry_1910, n = 50)

# Save
write_csv(ancestry_1910, "neighborhood_ancestry_1910.csv")


# ============================================================
# 4. WIDE FORMAT: ONE ROW PER NEIGHBORHOOD
# ============================================================
# Useful for joining back to spatial data or comparing decades

ancestry_1910_wide <- ancestry_1910 |>
  select(neighborhood, ancestry, pct_ancestry) |>
  pivot_wider(
    names_from  = ancestry,
    values_from = pct_ancestry,
    values_fill = 0
  ) |>
  # Clean up column names for R compatibility
  rename_with(\(x) str_replace_all(x, "[^a-zA-Z0-9]", "_") |> tolower(),
              -neighborhood)

print(ancestry_1910_wide)
write_csv(ancestry_1910_wide, "neighborhood_ancestry_1910_wide.csv")


# ============================================================
# 5. TOP ANCESTRIES PER NEIGHBORHOOD
# ============================================================
# Handy summary: what are the top 5 ancestries in each neighborhood?

top_ancestries <- ancestry_1910 |>
  filter(ancestry != "United States") |>   # exclude native stock for this view
  group_by(neighborhood) |>
  slice_max(pct_ancestry, n = 5) |>
  ungroup()

cat("\nTop 5 foreign ancestries per neighborhood:\n")
print(top_ancestries, n = 100)


# ============================================================
# 6. NATIVE STOCK SUMMARY
# ============================================================
# Separate view of native stock (both parents US-born) vs immigrant stock

native_stock <- census_1910 |>
  filter(!is.na(neighborhood)) |>
  mutate(
    stock = case_when(
      m_us & f_us              ~ "Native stock (both parents US-born)",
      !m_us & !f_us            ~ "Immigrant stock (both parents foreign)",
      (m_us & !f_us) |
        (!m_us & f_us)         ~ "Mixed stock (one parent foreign)",
      TRUE                     ~ "Unknown"
    )
  ) |>
  group_by(neighborhood, stock) |>
  summarise(n = n(), .groups = "drop") |>
  group_by(neighborhood) |>
  mutate(pct = round(n / sum(n) * 100, 1)) |>
  ungroup()

cat("\nNative vs immigrant stock by neighborhood:\n")
print(native_stock, n = 50)

write_csv(native_stock, "neighborhood_stock_1910.csv")


ancestry_1910 |>
  filter(ancestry != "United States") |>
  group_by(neighborhood) |>
  slice_max(pct_ancestry, n = 3) |>
  ungroup() |>
  arrange(neighborhood, desc(pct_ancestry)) |>
  print(n = 50)

ancestry_1910 |>
  filter(ancestry == "Russia/Russian Empire") |>
  arrange(desc(pct_ancestry))

census_1910 |>
  filter(neighborhood %in% c("Irvington", "Eliot")) |>
  count(LANGUAGE) |>
  arrange(desc(n))

ancestry_1910 |>
  group_by(neighborhood) |>
  summarise(total_pct = sum(pct_ancestry)) |>
  print()














#Below is the original Leaflet project code:

# ============================================================
# 1. LOAD DATA
# ============================================================

census <- read_csv("your_file.csv")   # ← replace with your filename

# Quick sanity check
glimpse(census)
cat("Total rows:", nrow(census), "\n")
cat("Unique EDs:", n_distinct(census$ENUMDIST), "\n")


# ============================================================
# 2. DECODE CODED VARIABLES
# ============================================================
# IPUMS uses numeric codes; we attach human-readable labels.

census <- census |>
  mutate(
    # --- Extract plain ED number from the 8-digit IPUMS code ---
    # Format is SSCCCEEE: state(2) + county(3) + ED(3)
    ed_num = as.integer(ENUMDIST %% 1000),
    
    # --- Race (RACE variable) ---
    race_label = case_match(RACE,
                            1 ~ "White",
                            2 ~ "Black",
                            3 ~ "American Indian",
                            4 ~ "Chinese",
                            5 ~ "Japanese",
                            6 ~ "Other Asian",
                            8 ~ "Two or more races",
                            .default ~ "Other/Unknown"
    ),
    
    # --- Nativity: classify by parental birthplace ---
    # Both parents born in US = native stock
    # At least one parent foreign-born = immigrant stock
    # (Use MBPL/FBPL: 1–99 = US states/territories; 100+ = foreign)
    mbpl_foreign = MBPL >= 100 & !is.na(MBPL),
    fbpl_foreign = FBPL >= 100 & !is.na(FBPL),
    nativity = case_when(
      mbpl_foreign | fbpl_foreign ~ "Immigrant stock",
      MBPL < 100 & FBPL < 100    ~ "Native stock",
      TRUE                        ~ "Unknown"
    ),
    
    # --- Mother's region of origin (broad groupings) ---
    mbpl_region = case_when(
      MBPL < 100                     ~ "United States",
      MBPL %in% 400:429              ~ "Germany/Austria",
      MBPL %in% 430:439              ~ "Scandinavia",
      MBPL %in% 440:449              ~ "UK / Ireland",
      MBPL %in% 450:469              ~ "Russia / E. Europe",
      MBPL %in% 470:499              ~ "S. / W. Europe",
      MBPL %in% 500:599              ~ "Asia",
      MBPL %in% 600:699              ~ "Canada",
      TRUE                           ~ "Other/Unknown"
    ),
    
    # --- Occupation broad sector (OCC1950 codes) ---
    occ_sector = case_when(
      OCC1950 %in% 100:123           ~ "Professional",
      OCC1950 %in% 200:290           ~ "Farmers & Farm Labor",
      OCC1950 %in% 300:390           ~ "Managers & Proprietors",
      OCC1950 %in% 400:490           ~ "Clerical",
      OCC1950 %in% 500:595           ~ "Sales",
      OCC1950 %in% 600:690           ~ "Craftsmen & Foremen",
      OCC1950 %in% 700:720           ~ "Operatives",
      OCC1950 %in% 730:840           ~ "Service Workers",
      OCC1950 %in% 900:970           ~ "Laborers",
      OCC1950 == 999                 ~ "Not in labor force / N/A",
      TRUE                           ~ "Other"
    ),
    
    # --- Citizenship ---
    citizen_label = case_match(CITIZEN,
                               0 ~ "N/A (born in US)",
                               1 ~ "Naturalized",
                               2 ~ "First papers filed",
                               3 ~ "Not yet filed",
                               4 ~ "Not a citizen",
                               .default ~ "Unknown"
    )
  )


# ============================================================
# 3. AGGREGATE TO ENUMERATION DISTRICT LEVEL
# ============================================================

ed_summary <- census |>
  group_by(ed_num) |>
  summarise(
    total_pop      = n(),
    
    # Race
    pct_white      = mean(race_label == "White") * 100,
    pct_black      = mean(race_label == "Black") * 100,
    pct_chinese    = mean(race_label == "Chinese") * 100,
    pct_japanese   = mean(race_label == "Japanese") * 100,
    
    # Nativity
    pct_imm_stock  = mean(nativity == "Immigrant stock") * 100,
    pct_native     = mean(nativity == "Native stock") * 100,
    
    # Top origin regions
    pct_us_born_parents    = mean(mbpl_region == "United States") * 100,
    pct_german_austrian    = mean(mbpl_region == "Germany/Austria") * 100,
    pct_scandinavian       = mean(mbpl_region == "Scandinavia") * 100,
    pct_russian_eeurope    = mean(mbpl_region == "Russia / E. Europe") * 100,
    pct_uk_irish           = mean(mbpl_region == "UK / Ireland") * 100,
    pct_asian_born         = mean(mbpl_region == "Asia") * 100,
    
    # Occupation
    pct_professional  = mean(occ_sector == "Professional") * 100,
    pct_laborer       = mean(occ_sector == "Laborers") * 100,
    pct_service       = mean(occ_sector == "Service Workers") * 100,
    pct_craftsmen     = mean(occ_sector == "Craftsmen & Foremen") * 100,
    pct_nilf          = mean(occ_sector == "Not in labor force / N/A") * 100,
    
    # Top occupation by count (mode)
    top_occ_sector    = names(sort(table(occ_sector[occ_sector != "Not in labor force / N/A"]),
                                   decreasing = TRUE))[1],
    
    .groups = "drop"
  ) |>
  mutate(across(where(is.numeric), \(x) round(x, 1)))

cat("ED summary rows:", nrow(ed_summary), "\n")
print(head(ed_summary))


# ============================================================
# 4. LOAD BOUNDARY FILE
# ============================================================
# You need a GeoJSON or shapefile of 1910 Portland ED boundaries.
#
# OPTIONS:
#   A) NHGIS shapefile → convert via sf:
#        boundaries <- st_read("path/to/nhgis_ed_1910.shp")
#
#   B) GeoJSON file:
#        boundaries <- st_read("portland_eds_1910.geojson")
#
#   C) If using NHGIS shapefile, the join key is usually "ENUMDIST"
#      or a "GISJOIN" field — inspect with: names(boundaries)

boundaries <- st_read("YOUR_BOUNDARY_FILE.geojson")   # ← replace

# Make sure CRS is WGS84 (required for Leaflet)
boundaries <- st_transform(boundaries, crs = 4326)

# Inspect the boundary attribute names to find the ED join key
cat("Boundary columns:", names(boundaries), "\n")

# Join census summary to boundaries
# Adjust "ED_KEY_COLUMN" to whatever the ED field is called in your boundary file
map_data <- boundaries |>
  left_join(ed_summary, by = c("ED_KEY_COLUMN" = "ed_num"))

cat("Matched EDs:", sum(!is.na(map_data$total_pop)), "of", nrow(map_data), "\n")


# ============================================================
# 5. BUILD LEAFLET MAP
# ============================================================

# --- Color palettes for different variables ---
pal_pop      <- colorNumeric("YlOrRd",  domain = map_data$total_pop,     na.color = "#cccccc")
pal_imm      <- colorNumeric("PuBuGn",  domain = map_data$pct_imm_stock,  na.color = "#cccccc")
pal_black    <- colorNumeric("Oranges", domain = map_data$pct_black,      na.color = "#cccccc")
pal_chinese  <- colorNumeric("Reds",    domain = map_data$pct_chinese,    na.color = "#cccccc")
pal_labor    <- colorNumeric("Blues",   domain = map_data$pct_laborer,    na.color = "#cccccc")
pal_prof     <- colorNumeric("Greens",  domain = map_data$pct_professional,na.color = "#cccccc")

# --- Popup HTML: rich info card for each ED ---
make_popup <- function(d) {
  paste0(
    "<div style='font-family:sans-serif; min-width:220px'>",
    "<h3 style='margin:0 0 6px 0; color:#333'>ED ", d$ed_num, "</h3>",
    "<b>Total population:</b> ", comma(d$total_pop), "<br>",
    "<hr style='margin:6px 0'>",
    "<b>Race</b><br>",
    "White: ", d$pct_white, "%<br>",
    "Black: ", d$pct_black, "%<br>",
    "Chinese: ", d$pct_chinese, "%<br>",
    "Japanese: ", d$pct_japanese, "%<br>",
    "<hr style='margin:6px 0'>",
    "<b>Nativity</b><br>",
    "Immigrant stock: ", d$pct_imm_stock, "%<br>",
    "Native stock: ", d$pct_native, "%<br>",
    "Top ancestry: Germany/Austria ", d$pct_german_austrian, "%, ",
    "Scandinavia ", d$pct_scandinavian, "%, ",
    "Russia/E.Europe ", d$pct_russian_eeurope, "%<br>",
    "<hr style='margin:6px 0'>",
    "<b>Occupations</b><br>",
    "Laborers: ", d$pct_laborer, "%<br>",
    "Professional: ", d$pct_professional, "%<br>",
    "Service: ", d$pct_service, "%<br>",
    "Craftsmen: ", d$pct_craftsmen, "%<br>",
    "Top sector: <i>", d$top_occ_sector, "</i>",
    "</div>"
  )
}

popups <- pmap_chr(map_data |> st_drop_geometry() |> as.list() |> transpose(),
                   \(...) make_popup(list(...)))

# --- Build the map with layer controls ---
m <- leaflet(map_data) |>
  addProviderTiles("CartoDB.Positron", group = "Light basemap") |>
  addProviderTiles("Esri.WorldImagery",  group = "Satellite") |>
  
  # Layer 1: Total population
  addPolygons(
    group       = "Total population",
    fillColor   = ~pal_pop(total_pop),
    fillOpacity = 0.7,
    color       = "white", weight = 1,
    popup       = popups,
    label       = ~paste0("ED ", ed_num, ": ", comma(total_pop), " people")
  ) |>
  addLegend("bottomright", pal = pal_pop, values = ~total_pop,
            title = "Total population", group = "Total population") |>
  
  # Layer 2: % immigrant stock
  addPolygons(
    group       = "% Immigrant stock",
    fillColor   = ~pal_imm(pct_imm_stock),
    fillOpacity = 0.7,
    color       = "white", weight = 1,
    popup       = popups,
    label       = ~paste0("ED ", ed_num, ": ", pct_imm_stock, "% immigrant stock")
  ) |>
  addLegend("bottomright", pal = pal_imm, values = ~pct_imm_stock,
            title = "% Immigrant stock", group = "% Immigrant stock") |>
  
  # Layer 3: % Black
  addPolygons(
    group       = "% Black",
    fillColor   = ~pal_black(pct_black),
    fillOpacity = 0.7,
    color       = "white", weight = 1,
    popup       = popups,
    label       = ~paste0("ED ", ed_num, ": ", pct_black, "% Black")
  ) |>
  addLegend("bottomright", pal = pal_black, values = ~pct_black,
            title = "% Black", group = "% Black") |>
  
  # Layer 4: % Chinese
  addPolygons(
    group       = "% Chinese",
    fillColor   = ~pal_chinese(pct_chinese),
    fillOpacity = 0.7,
    color       = "white", weight = 1,
    popup       = popups,
    label       = ~paste0("ED ", ed_num, ": ", pct_chinese, "% Chinese")
  ) |>
  addLegend("bottomright", pal = pal_chinese, values = ~pct_chinese,
            title = "% Chinese", group = "% Chinese") |>
  
  # Layer 5: % Laborers
  addPolygons(
    group       = "% Laborers",
    fillColor   = ~pal_labor(pct_laborer),
    fillOpacity = 0.7,
    color       = "white", weight = 1,
    popup       = popups,
    label       = ~paste0("ED ", ed_num, ": ", pct_laborer, "% laborers")
  ) |>
  addLegend("bottomright", pal = pal_labor, values = ~pct_laborer,
            title = "% Laborers", group = "% Laborers") |>
  
  # Layer 6: % Professional
  addPolygons(
    group       = "% Professional",
    fillColor   = ~pal_prof(pct_professional),
    fillOpacity = 0.7,
    color       = "white", weight = 1,
    popup       = popups,
    label       = ~paste0("ED ", ed_num, ": ", pct_professional, "% professional")
  ) |>
  addLegend("bottomright", pal = pal_prof, values = ~pct_professional,
            title = "% Professional", group = "% Professional") |>
  
  # Layer controls
  addLayersControl(
    baseGroups   = c("Light basemap", "Satellite"),
    overlayGroups = c("Total population", "% Immigrant stock", "% Black",
                      "% Chinese", "% Laborers", "% Professional"),
    options = layersControlOptions(collapsed = FALSE)
  ) |>
  # Show only one layer at a time by default
  hideGroup(c("% Immigrant stock", "% Black", "% Chinese",
              "% Laborers", "% Professional")) |>
  
  addScaleBar(position = "bottomleft") |>
  setView(lng = -122.676, lat = 45.523, zoom = 13)

# Display in RStudio / Posit viewer
m

# Save as standalone HTML file
saveWidget(m, "portland_1910_map.html", selfcontained = TRUE)
cat("Map saved to portland_1910_map.html\n")


# ============================================================
# 6. OPTIONAL: EXPORT ED SUMMARY AS CSV (for reference)
# ============================================================
write_csv(ed_summary, "ed_summary_1910.csv")
cat("ED summary saved to ed_summary_1910.csv\n")