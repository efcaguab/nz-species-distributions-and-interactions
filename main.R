# Prepare workspace -------------------------------------------------------

pkgconfig::set_config("strings_in_dots" = "literals")

library(magrittr)
library(drake)

# load functions
f <- lapply(list.files("code", full.names = T), source)


read_data_plan <- drake_plan(
  networks = read_networks(network_folder = "./data/raw/web-of-life_2018-10-23_042854"),
  metadata = read_metadata(file_in("./data/raw/web-of-life_2018-10-23_042854/references.csv"))
)

exploration_plan <- drake_plan(
  species_list = get_species_list(networks, metadata), 
  interaction_list = get_interaction_list(networks, metadata),
  # the thresholds are specificied on the number of locations a species is
  focal_species_options = get_focal_species_options(species_list, interaction_list, categ = "loc"), 
  sp_list_extra_info = get_sp_extra_info(focal_species_options, 10),
  backbones = get_sp_backbones(sp_list_extra_info),
  n_occurrences = get_n_ocurrences(backbones),
  maps = fetch_maps(backbones),
  turnover_exploration = my_render(knitr_in("./paper/data-exploration/turnover.Rmd"), 
            file_out("./paper/data-exploration/turnover.md"))
)


reporting_plan <- drake_plan(
  bibliography = target(
    command = get_bibliography("https://raw.githubusercontent.com/efcaguab/phd-bibliography/master/interactions%2Bsdm.bib", 
                               file_out("paper/bibliography.bib"))
  ),
  abstract = readLines(file_in("./paper/abstract.md")),
  keywords = process_keywords(file_in("./paper/keywords.md")),
  acknowledgements = readLines(file_in("./paper/acknowledgements.md")),
  intro_line_number = get_line_number(file_in("paper/manuscript.Rmd"), "# Introduction"),
  abs_wordcount = count_words(file_in("paper/abstract.md")),
  msc_wordcount = count_words(file_in('paper/manuscript.Rmd'), lines_to_ignore = 1:intro_line_number),
  n_references = count_references(file_in('paper/manuscript.Rmd'), lines_to_ignore = 1:intro_line_number, refs_to_exclude = "@ref"),
  n_displays = count_displays(file_in('paper/manuscript.Rmd'), lines_to_ignore = 1:intro_line_number),
  msc_title = get_yaml_title(file_in('paper/manuscript.Rmd')),
  supp_info = render_pdf(knitr_in('paper/supp-info.Rmd'), file_out('paper/supp-info.pdf'), clean_md = FALSE),
  draft_info = render_pdf(file_in('paper/draft-info.Rmd'), file_out('paper/draft-info.pdf'), clean_md = FALSE),
  manuscript = render_pdf(knitr_in('paper/manuscript.Rmd'), file_out('paper/manuscript.pdf'), clean_md = FALSE),
  cover_letter = knitr::knit2pdf(knitr_in("paper/cover-letter.Rnw"), output = file_out("paper/cover-letter.tex"))
)

full_plan <- rbind(
  read_data_plan, 
  exploration_plan, 
  reporting_plan
)

full_config <- drake_config(full_plan)
make(full_plan)
