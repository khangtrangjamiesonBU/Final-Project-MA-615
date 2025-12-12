library(shiny)
library(dplyr)
library(stringr)
library(readr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(leaflet)
library(tibble)
library(RColorBrewer)
library(viridisLite)

## ---------- DATA PREP (runs once when the app starts) ----------

# 1. Read lemur data
lem_raw <- read.csv("supplementary-table-s1-3.csv", skip = 1)

lemurs <- lem_raw %>%
  filter(
    Kingdom == "Animalia",
    Class == "Mammalia",
    Order == "Primates"
  ) %>%
  filter(!is.na(Latitude), !is.na(Longitude))

# Body-mass ranges for species with all NAs
mass_ranges <- tribble(
  ~Genus_Species,              ~min_mass, ~max_mass,
  "Avahi laniger",             0.6,       1.3,
  "Cheirogaleus grovesi",      0.4,       0.41,
  "Cheirogaleus medius",       0.12,      0.27,
  "Eulemur rufifrons",         2.2,       2.3,
  "Hapalemur griseus",         0.7,       1.0,
  "Hapalemur meridionalis",    0.965,     1.179,
  "Indri indri",               6.0,       9.5,
  "Lemur catta",               2.2,       3.5,
  "Lepilemur petteri",         0.5,       0.9,
  "Lepilemur ruficaudatus",    0.6,       0.9,
  "Microcebus",                0.03,      0.12,
  "Microcebus cf. murinus",    0.058,     0.067,
  "Mirza coquereli",           0.29,      0.32,
  "Phaner pallescens",         0.3,       0.5,
  "Propithecus diadema",       5.0,       7.0,
  "Propithecus verreauxi",     2.5,       4.0,
  "Varecia rubra",             3.3,       4.1
)

set.seed(123)

# Impute & clean
lemurs_imputed <- lemurs %>%
  mutate(Body_Mass = as.numeric(Body_Mass)) %>%
  group_by(Genus_Species) %>%
  mutate(
    Body_Mass = if_else(
      is.na(Body_Mass),
      median(Body_Mass, na.rm = TRUE),
      Body_Mass
    )
  ) %>%
  ungroup() %>%
  left_join(mass_ranges, by = "Genus_Species") %>%
  mutate(
    Body_Mass = if_else(
      is.na(Body_Mass) & !is.na(min_mass),
      runif(n(), min_mass, max_mass),
      Body_Mass
    )
  ) %>%
  select(-min_mass, -max_mass)

# Fix encoding for character columns
lemurs_imputed <- lemurs_imputed %>%
  mutate(across(where(is.character), ~ iconv(.x, from = "", to = "UTF-8")))

# Aggregate for map
lemurs_counts <- lemurs_imputed %>%
  group_by(
    Genus_Species,
    Site_Name,
    Locality,
    Latitude,
    Longitude
  ) %>%
  summarise(
    n_individuals = n(),
    Elevation     = first(Elevation),
    .groups = "drop"
  )

# sf object
lemurs_counts_sf <- st_as_sf(
  lemurs_counts,
  coords = c("Longitude", "Latitude"),
  crs = 4326
)

# Madagascar polygon
madagascar <- ne_countries(
  scale = "medium",
  country = "Madagascar",
  returnclass = "sf"
)

# Color palette by species
species_levels <- sort(unique(lemurs_counts_sf$Genus_Species))
n_species <- length(species_levels)
pal_vec <- colorRampPalette(brewer.pal(12, "Paired"))(n_species)

pal <- colorFactor(
  palette = pal_vec,
  domain  = species_levels
)


# ---- Gallery metadata (from data.xlsx) ----
gallery_info <- tribble(
  ~Common_Name, ~img, ~lifespan, ~diet, ~status,
  "eastern woolly lemur", "lemur_gallery/Eastern-Woolly-Lemur.jpg", "unknown", "young leaves, buds, and fruits", "Vulnerable",
  "red-fronted brown lemur", "lemur_gallery/redfrontedlemur.jpg", "20-25 years in the wild", "fruit, leaves", "Vulnerable",
  "Verreaux's sifaka", "lemur_gallery/Verreauxs-Sifaka.jpg", "18-23 years but potentially longer (up to 31 years)", "leaves, flowers, fruit, and bark", "Critically Endangered",
  "golden-brown mouse lemur", "lemur_gallery/golden-brown-mouse-lemur.jpg", "6-8 years wild, onger in captivity (up to 18+ years)", "insects, fruits, flowers, and plants", "Unknown",
  "furry-eared dwarf lemur", "lemur_gallery/furry-eared-dwarf-lemur.jpg", "relatives suggest up to 15-19 years in captivity", "fruits, insects, nectar, and tree sap", "Endangered",
  "ring-tailed lemur", "lemur_gallery/ring-tailed-lemur.jpg", "16-20 years in the wild, longer in captivity", "eat fruits, leaves, flowers, and insects (opportunistic omnivores)", "Endangered",
  "mouse lemur", "lemur_gallery/mouse-lemur.jpg", "typically live 6-8 years in the wild, longer in captivity (up to 14-15 yrs)", "omnivorous, focusing on insects, fruit, flowers, nectar, and small vertebrates", "Least Concern",
  "Coquerel's giant mouse lemur", "lemur_gallery/Coquerels-giant-mouse-lemur.jpg", "around 5-8 years but longer in captivity (up to ~25 years)", "omnivore eating insects, fruit, flowers, and small vertebrates", "Endangered",
  "red-fronted lemur", "lemur_gallery/red-fronted-lemur.jpg", "20-25 years in the wild", "fruit, leaves, flowers, and insects", "Vulnerable",
  "Groves' dwarf lemur", "lemur_gallery/Groves-dwarf-lemur.jpg", "20+ years in captivity", "fruit,nectar,insects", "Undetermined",
  "indri", "lemur_gallery/indri.jpg", "15-20 years", "leaves, flowers, and fruit", "Critically Endangered",
  "diademed sifaka", "lemur_gallery/diademed-sifaka.jpg", "20-27 years", "leaves, fruits, flowers, and seeds", "Critically Endangered",
  "Simmons' mouse lemur", "lemur_gallery/Simmons-mouse-lemur.jpg", "6-8 years ", "omnivorous, including insects (especially beetles), fruits, flowers, gum, and even small vertebrates like frogs or chameleons", "Endangered",
  "Sibree's dwarf lemur", "lemur_gallery/Sibrees-dwarf-lemur.jpg", "unknown", "presumed to be omnivorous, including fruits, flowers, insects, and tree gum", "Critically Endangered",
  "Petter's sportive lemur", "lemur_gallery/Petters-sportive-lemur.jpg", "15+ years in captivity", "leaves and occasionally fruit", "Vulnerable",
  "gray bamboo lemur", "lemur_gallery/gray-bamboo-lemur.jpg", "23 years in captivity", "bamboo but supplements with leaves/fruit", "Vulnerable",
  "fat-tailed dwarf lemur", "lemur_gallery/fat-tailed-dwarf-lemur.jpg", "4-11 years", "fruits, flowers, nectar, insects, and small vertebrates", "Vulnerable",
  "pale fork-marked lemur", "lemur_gallery/pale-fork-marked-lemur.jpg", "12 years in captivity", "tree gums/saps, insects, and fruits", "Endangered",
  "brown mouse lemur", "lemur_gallery/brown-mouse-lemur.jpg", "6-8 years", "fruit, insects, gum, and nectar", "Endangered",
  "reddish-gray mouse lemur", "lemur_gallery/reddish-gray-mouse-lemur.jpg", "unknown", "eating insects (especially beetles), fruits, flowers, nectar, and tree gum", "Least Concern",
  "gray mouse lemur", "lemur_gallery/gray-mouse-lemur.jpg", "unknown", "Insects (especially beetles), fruits, flowers, nectar, and tree gum", "Least Concern",
  "Goodman's mouse lemur", "lemur_gallery/Goodmans-mouse-lemur.jpg", "unknown", "linsects (like beetles) and fruits, but also nectar, flowers, and small vertebrates", "Vulnerable",
  "black-and-white ruffed lemur", "lemur_gallery/black-and-white-ruffed-lemur.jpg", "unknown", "fruits, leaves, flowers, and nectar, occasionally insects", "Critically Endangered",
  "southern lesser bamboo lemur", "lemur_gallery/southern-lesser-bamboo-lemur.jpg", "23 years", "leaves, shoots, and culms (stems) of the bamboo plant", "Vulnerable",
  "Madame Berthe's mouse lemur", "lemur_gallery/Madame-Berthes-mouse-lemur.jpg", "6-8 years ", "sugary 'honeydew' (insect secretions) produced by the larvae of the Flatida coccinea bug", "Critically Endangered",
  "red-tailed sportive lemur", "lemur_gallery/red-tailed-sportive-lemur.jpg", "8 years in the wild (up to 15+ in captivity)", "leaves (folivore) with fruit/flowers", "Critically Endangered",
  "red ruffed lemur", "lemur_gallery/red-ruffed-lemur.jpg", "15-20 years", "fruits, but also nectar, leaves, and seeds", "Critically Endangered"
)


## ---------- UI ----------

ui <- fluidPage(
  # Global styles
  tags$head(
    tags$style(HTML("
      /* Make tab titles bigger and bold */
      .nav-tabs > li > a {
        font-size: 18px;
        font-weight: bold;
      }

      /* Main headings (e.g., 'Lemurs of Madagascar') */
      h2 {
        color: #8B0000;        /* dark red */
      }

      /* Subheadings (e.g., 'Evolutionary Background') */
      h3 {
        color: #003366;        /* dark blue */
        text-align: center;    /* centered */
      }
      
       /* Source section (blue + hyperlink styling) */
      .source-heading a {
        color: #003366;
        text-decoration: underline;
        font-weight: bold;
      }
      .source-heading a:hover {
        text-decoration: none;
      }
      
            /* ---- Gallery grid ---- */
      .gallery-grid {
        display: grid;
        grid-template-columns: repeat(auto-fill, minmax(220px, 1fr));
        gap: 16px;
        align-items: start;
      }

      .gallery-item {
        position: relative;
        border-radius: 14px;
        overflow: hidden;
        box-shadow: 0 2px 8px rgba(0,0,0,0.12);
        cursor: default;
        background: #fff;
      }

      .gallery-item img {
        width: 100%;
        height: 180px;
        object-fit: cover;
        display: block;
      }

      .gallery-caption {
        padding: 10px 12px;
        font-weight: 600;
        font-size: 14px;
      }

      .gallery-tooltip {
        position: absolute;
        inset: 0;
        background: rgba(0,0,0,0.72);
        color: #fff;
        opacity: 0;
        transition: opacity 0.18s ease-in-out;
        padding: 12px;
        font-size: 13px;
        line-height: 1.35;
      }

      .gallery-item:hover .gallery-tooltip {
        opacity: 1;
      }

      .gallery-tooltip .title {
        font-weight: 800;
        font-size: 15px;
        margin-bottom: 6px;
      }

      .gallery-tooltip b {
        font-weight: 800;
      }

    "))
  ),
  # Banner with image + title on top of it
  tags$div(
    style = "position: relative; margin-bottom: 20px;",
    
    # banner image
    tags$img(
      src = "lemur_banner.jpg",  # file inside www/
      style = "width: 100%; max-height: 300px; object-fit: cover;"
    ),
    
    # title overlaid on the image
    tags$h1(
      "Endangered Species - Lemurs in Madagascar",
      style = paste(
        "position: absolute;",
        "bottom: 20px;",
        "left: 30px;",
        "color: white;",
        "font-size: 42px;",
        "font-weight: bold;",
        "text-shadow: 0 0 8px rgba(0,0,0,0.8);",
        "margin: 0;"
      )
    )
  ),
  
  # ---- Tabs ----
  tabsetPanel(
    tabPanel(
      "Introduction",
      br(),
      
      # --- YouTube video ---
      tags$div(
        style = "text-align: center;",
        tags$iframe(
          width = "800",
          height = "450",
          src = "https://www.youtube.com/embed/6eaTBoqpNEg",
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = TRUE
        )
      ),
      
      br(), br(),
      
      # --- Introduction Text ---
      tags$h2("Lemurs of Madagascar"),
      tags$p("Madagascar is renowned for its lemurs—unique primates that look like a blend of cat, squirrel, and dog. 
              Found nowhere else on Earth, these animals display remarkable behaviors, from the whale-like calls of the 
              indri to the elegant, dance-like leaps of the sifaka. The sections below introduce their origins, 
              behavior, and conservation."),
      
      tags$h3("Evolutionary Background"),
      tags$p("Unlike other parts of the world, Madagascar lacks the dominant primate group known as the Haplorhini, 
              which includes monkeys, apes, and humans. Instead, this ecological space is occupied by lemurs, members 
              of the more ancient Strepsirhini lineage, which also includes lorises, bush babies, and pottos. These early 
              primates were generally small, nocturnal, insect-eating animals with large eyes and long snouts. Madagascar’s 
              long geographic isolation allowed lemurs to survive and evolve relatively undisturbed."),
      
      tags$p("About 160 million years ago, Madagascar was still attached to Africa as part of the supercontinent 
              Gondwanaland. When the landmass split, Madagascar drifted eastward and became isolated. The first lemur-like 
              primates arose in Africa around 60 million years ago and eventually reached Madagascar. Later, when monkeys 
              emerged roughly 17–23 million years ago, the island was already cut off, preventing monkeys from ever arriving. 
              Because monkeys were more adaptable and social, they outcompeted lemur relatives almost everywhere 
              else—except on Madagascar."),
      
      tags$p("Freed from such competition, lemurs diversified extensively, adapting to a wide range of habitats with few 
              large predators. They eventually developed many traits similar to monkeys, including social living, daytime 
              activity, and plant-based diets."),
      
      tags$p("Humans arrived much later, about 2,000 years ago, bringing dramatic ecological change. Their presence led to 
              the extinction of at least 15 lemur species, including giants as large as modern gorillas. Today, the largest 
              living lemur—the indri—is tiny in comparison. Nearly all species now face extinction, primarily due to 
              deforestation and hunting."),
      
      tags$h3("Social Behavior"),
      tags$p("Lemur societies vary greatly across species. Ring-tailed lemurs (Lemur catta) form large, female-led groups 
              of up to 30 members, where females have priority access to resources. In contrast, species like the aye-aye 
              (Daubentonia madagascariensis) are mostly solitary and meet only to reproduce. Generally, diurnal species form 
              more complex social groups, while nocturnal species tend to live alone or in small units. Grooming plays a 
              critical role in strengthening social bonds and maintaining hygiene."),
      
      tags$h3("Diet and Feeding Strategies"),
      tags$p("Lemur diets shift according to species and seasonal food availability. Many rely heavily on fruit, but most 
              are flexible omnivores that eat leaves, flowers, seeds, nectar, and insects. Bamboo lemurs (Hapalemur spp.) 
              specialize in eating bamboo that contains cyanide levels lethal to most animals. The aye-aye, using its 
              elongated middle finger, extracts insect larvae from wood, filling a niche similar to a woodpecker."),
      
      tags$h3("Predators and Threats"),
      tags$p("Natural predators include the fossa (Cryptoprocta ferox), a tree-climbing carnivore, and several large birds 
              of prey like the Madagascar harrier-hawk. However, the most significant threats come from humans. Widespread 
              habitat loss, fragmentation, and hunting have caused severe population declines. Today, over 95% of lemur species 
              are listed as endangered or critically endangered."),
      
      tags$h3("Conservation Efforts"),
      tags$p("Because lemurs are found only in Madagascar and play key ecological roles, they are central to conservation 
              initiatives. Protected areas such as Ranomafana, Andasibe-Mantadia, and Masoala National Parks provide essential 
              habitat. Conservation groups are also working to restore forests, reduce poaching, support sustainable community 
              practices, and expand ecotourism. Zoos worldwide maintain breeding programs for select species, though long-term 
              survival still depends on protecting Madagascar’s forests."),
      
      tags$h3("Why Madagascar’s Lemurs Matter Globally"),
      tags$p("Despite being just one of many countries with native primates, Madagascar contains an exceptionally large share 
              of primate diversity. As noted by primatologist Russell Mittermeier, the island shelters 21% of all primate genera 
              (14 of 65) and 36% of primate families (5 of 14)—the highest concentration worldwide."),
      
      tags$p("Because of this, Madagascar is recognized as a distinct fourth biogeographic region for primate study, separate 
              from the usual three: the Americas, Asia, and mainland Africa. Lemurs represent an ancient lineage that has 
              endured environmental change and human pressure for millions of years."),
      
      tags$p("Protecting lemurs is essential not only for preserving Madagascar’s natural heritage but also for advancing 
              scientific knowledge about primate behavior, evolution, and ecology. Lemurs play vital roles in seed dispersal, 
              pollination, and forest renewal, making their conservation a global priority."),
      br(),
      hr(),
      tags$h3(
        class = "source-heading",
        tags$a(
          "Source",
          href = "https://www.wildmadagascar.org/wildlife/lemurs.php",
          target = "_blank"
        )
      )
    ),
    
    tabPanel(
      "Endangered Species",
      br(),
      
      tags$h2("Threats to Madagascar’s Lemurs"),
      
      tags$p("Madagascar, located off the southeastern coast of Africa, contains extraordinary biological richness, 
          with an estimated 12,000 plant species and 700 vertebrate species, of which 80–90% occur nowhere else on Earth. 
          After being geographically isolated for approximately 88 million years, and spanning an area comparable to the 
          northeastern United States, the island has become one of the most significant biodiversity hotspots globally. 
          While species diversity is high across the island, the biodiversity of its tropical forests is particularly remarkable."),
      
      tags$p("Despite this ecological importance, Madagascar’s forests—much like tropical forests worldwide—are experiencing 
          rapid degradation. Fragmentation, unsustainable harvesting of timber and forest products, over-hunting, invasive 
          species, pollution, and climate change have all contributed to declines in native species. Although climate change 
          is often emphasized because of its widespread impacts, recent research indicates that, in Madagascar, it is not the 
          primary factor driving current species losses. Instead, human-induced habitat destruction plays the dominant role."),
      
      tags$h3("Forest Loss and Its Consequences"),
      
      tags$p("Nearly 44% of Madagascar’s forests have disappeared in the past six decades, largely due to slash-and-burn 
          agriculture (locally known as tavy) and charcoal production. The pace and extent of habitat loss and fragmentation 
          have been severe. Almost half of all remaining forest now lies within 100 meters (approximately 300 feet) of a 
          non-forested edge, exposing wildlife to ongoing disturbance and reducing ecosystem integrity. Illegal hunting and 
          the capture of wildlife for the pet trade further exacerbate biodiversity decline."),
      
      tags$p("As a result, Madagascar’s fauna faces extreme conservation pressure. According to the International Union for 
          Conservation of Nature (IUCN), 95% of lemur species—including some of the most iconic primates—are currently 
          threatened with extinction. Madagascar’s biodiversity crisis has intensified over the last decade."),
      
      # ---- Video BEFORE the Impact on Ruffed Lemurs section ----
      tags$div(
        style = "text-align: center; margin-top: 25px; margin-bottom: 25px;",
        tags$iframe(
          width = "800",
          height = "450",
          src = "https://www.youtube.com/embed/TxUPMzI14WY?start=19",
          frameborder = "0",
          allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
          allowfullscreen = TRUE
        )
      ),
      
      tags$h3("Impact on Ruffed Lemurs"),
      
      tags$p("Two recent scientific studies have examined how human activities influence ruffed lemurs, a group of critically 
          endangered primates that play a key ecological role as major seed dispersers and indicators of rainforest health. 
          Modeling that incorporated a range of climate and deforestation scenarios suggests that suitable habitat for the 
          two ruffed lemur species could decline by up to 93% over the next century. Under unchecked deforestation, nearly 
          all remaining eastern rainforest habitat could disappear, effectively eliminating ruffed lemur populations. The 
          analyses demonstrate that habitat loss is likely to have more immediate and severe impacts on these species than 
          climate change."),
      
      tags$p("However, projections also indicate that if protected areas maintain their current forest cover, the decline in 
          suitable habitat could be reduced to approximately 62%, highlighting the importance of safeguarding and improving 
          the integrity of existing reserves."),
      
      tags$p("A separate genetic study published in late 2019 examined how landscape features affect ruffed lemur gene flow—a 
          crucial process that maintains genetic diversity and supports population resilience. The study assessed both 
          natural barriers and human-caused constraints on movement, including rivers, elevation changes, roads, habitat 
          quality, and human population density. Human activity emerged as the strongest predictor of reduced genetic 
          exchange, with deforestation near human settlements forming significant barriers to movement and dispersal."),
      
      tags$p("Collectively, the evidence demonstrates that deforestation represents an urgent and direct threat to ruffed lemur 
          survival, surpassing the short-term impacts of climate change. This threat extends beyond lemurs; many 
          forest-dependent plants and animals face similar risks. Globally, over one-third (approximately 36.5%) of plant 
          species are extremely rare and disproportionately vulnerable to human land use. Regions with the highest 
          concentration of rare species often coincide with areas experiencing the greatest human disturbance."),
      
      tags$h3("Opportunities for Conservation Action"),
      
      tags$p("Madagascar’s natural heritage stands at a critical juncture. Strengthening protected areas, expanding 
          reforestation initiatives, and improving forest management remain essential strategies for mitigating habitat 
          loss while broader efforts address global climate change. Several nonprofit organizations are actively engaged in 
          these efforts. For example, the Madagascar Biodiversity Partnership, in collaboration with the Arbor Day 
          Foundation’s Plant Madagascar project, has planted nearly three million trees in the Kianjavato region—identified 
          as a priority zone for ruffed lemur conservation. The Centre ValBio, based near Ranomafana National Park, is also 
          conducting extensive reforestation work to support local ecosystems."),
      
      tags$p("At the policy level, Madagascar’s president, Andry Rajoelina, announced an initiative to reforest 40,000 hectares 
          (99,000 acres) annually for five consecutive years, a scale equivalent to approximately 75,000 football fields. 
          While the pledge is promising, the plan currently lacks detailed implementation strategies."),
      
      tags$p("Spatial projections from recent studies identify areas where habitat is likely to persist, as well as regions 
          facing high risk of near-complete habitat loss or genetic isolation for ruffed lemurs. Because lemur presence 
          strongly correlates with overall biodiversity, protecting lemur habitats will also safeguard broader ecological 
          communities. These research findings provide a roadmap for prioritizing conservation interventions to preserve 
          Madagascar’s exceptionally rich but increasingly threatened biodiversity."),
      br(),
      hr(),
      tags$h3(
        class = "source-heading",
        tags$a(
          "Source",
          href = "https://leakeyfoundation.org/lemurs-are-the-worlds-most-endangered-mammals-but-planting-trees-can-help-save-them/",
          target = "_blank"
        )
      )
    ),
    
    tabPanel(
      "Characteristics",
      br(),
      
      # ---- Section 1: Dictionary ----
      tags$h2("Dictionary"),
      tags$p(
        "Use this table to look up the genus and family for any lemur common name.",
        style = "font-size: 18px; margin-top: 5px; margin-bottom: 10px;"
      ),
      
      tags$label(
        "Choose a common name:",
        style = "font-size: 14px; font-weight: bold;"
      ),
      br(),
      
      selectInput(
        inputId = "dict_common",
        label   = NULL,
        choices = sort(unique(lemurs_imputed$Common_Name)),
        selected = sort(unique(lemurs_imputed$Common_Name))[1]
      ),
      
      tableOutput("dict_table"),
      
      hr(),
      
      # ---- Section 2: Gallery ----
      tags$h2("Gallery"),
      tags$p(
        "Hover over a photo to see the lemur’s common name, lifespan, diet, and IUCN conservation status.",
        style = "font-size: 18px; margin-top: 5px; margin-bottom: 10px;"
      ),
      
      uiOutput("gallery_grid"),
      
      hr(),
      
      # ---- Section 3: Distribution of Body Mass by Species ----
      tags$h2("Distribution of Body Mass by Species"),
      tags$p(
        "To explore the distribution of body mass for a particular species, please choose a species from the dropdown menu below.",
        style = "font-size: 18px; margin-top: 5px; margin-bottom: 10px;"
      ),
      
      tags$label(
        "Choose a species:",
        style = "font-size: 14px; font-weight: bold;"
      ),
      br(),
      
      selectInput(
        inputId = "bm_species",
        label   = NULL,
        choices = sort(unique(lemurs_imputed$Genus_Species)),
        selected = sort(unique(lemurs_imputed$Genus_Species))[1]
      ),
      
      plotOutput("bm_hist", height = "400px")
    ),
    
    tabPanel(
      "Map of Distribution",
      br(),
      
      tags$h2("Map of Lemur Distribution"),
      tags$p(
        "This map illustrates the geographic distribution of lemur species across Madagascar. 
     Each bubble represents a specific observation site, and the size of the bubble reflects 
     the number of individuals recorded at that location. By clicking on a bubble, you can 
     identify where each species resides, compare population sizes across regions, and view 
     additional ecological information such as locality names and elevation. This visualization 
     highlights Madagascar’s remarkable biodiversity and the unique habitats that support 
     different lemur species.",
        style = "font-size: 18px; margin-bottom: 15px;"
      ),
      
      leafletOutput("lemur_map", height = "650px")
    )
  )
)

    


## ---------- SERVER ----------

server <- function(input, output, session) {
  
  output$lemur_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      addPolygons(
        data   = madagascar,
        weight = 1,
        color  = "grey40",
        fillColor = "transparent"
      ) %>%
      addCircleMarkers(
        data = lemurs_counts_sf,
        radius = ~2 + n_individuals,
        stroke = FALSE,
        fillOpacity = 0.8,
        color = ~pal(Genus_Species),
        popup = ~paste0(
          "<b>Species:</b> ", Genus_Species, "<br>",
          "<b>Individuals at this site:</b> ", n_individuals, "<br>",
          "<b>Site:</b> ", Site_Name, "<br>",
          "<b>Locality:</b> ", Locality, "<br>",
          "<b>Elevation:</b> ", Elevation
        ),
        label = ~paste0(Genus_Species, " (n = ", n_individuals, ")")
      ) %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = lemurs_counts_sf$Genus_Species,
        title = "Species",
        opacity = 1
      )
  })
  
# ---- Dictionary table ----
  output$dict_table <- renderTable({
    req(input$dict_common)
    
    lemurs_imputed %>%
      filter(Common_Name == input$dict_common) %>%
      select(Common_Name, Genus_Species, Family) %>%
      distinct() %>%
      arrange(Genus_Species)
  })
  
# ---- Gallery grid ----
  output$gallery_grid <- renderUI({
    
    gal <- gallery_info %>%
      mutate(
        Common_Name_norm = Common_Name %>%
          as.character() %>%
          str_squish() %>%
          str_to_lower() %>%
          str_replace_all("’", "'")
      ) %>%
      arrange(Common_Name_norm)
    
    tags$div(
      class = "gallery-grid",
      lapply(seq_len(nrow(gal)), function(i) {
        tags$div(
          class = "gallery-item",
          tags$img(src = gal$img[i], alt = gal$Common_Name[i]),
          tags$div(
            class = "gallery-tooltip",
            tags$div(class = "title", gal$Common_Name[i]),
            HTML(paste0(
              "<b>Lifespan:</b> ", gal$lifespan[i], "<br>",
              "<b>Diet:</b> ", gal$diet[i], "<br>",
              "<b>IUCN status:</b> ", gal$status[i]
            ))
          ),
          tags$div(class = "gallery-caption", gal$Common_Name[i])
        )
      })
    )
  })

  # ---- Body-mass histogram ----
  output$bm_hist <- renderPlot({
    req(input$bm_species)
    
    df <- lemurs_imputed %>%
      filter(Genus_Species == input$bm_species, !is.na(Body_Mass))
    
    hist(
      df$Body_Mass,
      main = paste("Body Mass Distribution for", input$bm_species),
      xlab = "Body mass (kg)",
      col  = "lightblue",
      border = "white"
    )
  })
  
}

## ---------- RUN APP ----------

shinyApp(ui = ui, server = server)
