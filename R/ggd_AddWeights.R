# Zoek op "@@" om de feedback te vinden


# Deze functie voorziet een GM dataset van gewichten
# op basis van de GGD  richtlijn wegen voor epidemiologen.

#' Title
#'
#' @param data
#' @param reference
#' @param by.x
#' @param by.y
#' @param by.y.filters
#' @param respondent.id
#' @param weight.limits
#' @param force.cast.to.character
#' @param append
#' @param do.rescale
#'
#' @return
#' @export
#'
#' @examples
ggd_AddWeights <- function(
    data, # dataset van GM, tibble df afkomstig uit een haven inladen van een .sav bestand
    reference, # een referentietabel, df
    by.x = c("GEMEENTECODE", "OnderwijsSoort", "GESLACHT", "MBOKK331"), # welke kolommen in de data bevatten stratum informatie? @@ Wat betekent by.x/by.y? -> variabele naam voor mij niet heel intuitief
    # by.x.impute = NA, # TODO inbouwen @@Wat wil je hiermee doen?
    by.y = c("Gemeente", "OnderwijsSoort", "Geslacht", "Leerjaar"), # welke kolommen in de referentietabel bevatten stratum informatie?
    by.y.filters = list( # of de dataset gefilterd moet worden op kolommen uit by.y
      c(153,141,164,1742,163,173,189,158,183,1735,1700,1774,168,147), # standaard alleen cbs gemeentecodes van Twentse gemeenten
      NA, # standaard: geen filter op onderwijssoort @@Wat zegt "standaard:"?
      c(1, 2), # standaard: filteren op man/vrouw
      c(2, 4) # standaard: filteren op klas 2 en 4
    ),
    respondent.id = "Respondentnummer", # unieke kolom waarmee output mergeable is @@Wat is het voordeel van respondent.id tov. respondent_id?
    weight.limits = c(.2, 5), # w_klein moet hier tussenin zitten @@Wat is w_klein?
    force.cast.to.character = "Gemeente", # welke kolommen moeten een string worden TODO: beter maken @@wat betekent force.cast.to.character? Wat wil je hier precies doen?
    append = T, # als T, dan return(data+w), als F dan return(respondent.id+w)
    do.rescale = T # of w_klein en w_groot moet worden herschaald als gewogen_aantallen!=daadwerkelijke aantallen
){
  cat("Adding weights ...\n")

  # summarize dataset on reference vars per stratum
  weight_table <- data  %>%
    group_by(across(all_of(by.x))) %>%
    rename_with(~ by.y, .cols = all_of(by.x)) %>%
    summarise(n_vragenlijst = length(!!sym(respondent.id)))

  # apply filters based on by.y.filters
  for(i in 1:length(by.y.filters)){
    current_filter <- by.y.filters[[i]]  #@@Variabele niet echt nodig?
    if(!is.na(current_filter[1])){
      weight_table <- weight_table %>% filter(!!sym(by.y[i]) %in% current_filter)
    }
  }

  # convert some columns to characters if desired
  if(!is.na(force.cast.to.character[1])){
    force.cast.to.character <- paste(force.cast.to.character, collapse = "|")
    weight_table <- weight_table %>%
      mutate_at(vars(matches(force.cast.to.character)), function(x){as.character(as_factor(x))})
  }

  # join data summary data with reference table
  # based on matchcolumns, then calculate weights per stratum
  # remove rows for which no cbs counts are available
  weight_table <- weight_table %>%
    left_join(reference %>% select(-Jaar)) %>%
    mutate(w_klein = NA, w_groot = NA, proportie_cbs = NA, proportie_vragenlijst = NA) %>%
    filter(!is.na(n_cbs))

  # for each gemeente calculate weighting factors
  # TODO: deze groepering flexibel maken met argument @@
  for(g in weight_table$Gemeente){
    r <- weight_table$Gemeente == g
    weight_table$proportie_cbs[r] <- weight_table$n_cbs[r] / sum(weight_table$n_cbs[r])
    weight_table$proportie_vragenlijst[r] <-  weight_table$n_vragenlijst[r] / sum(weight_table$n_vragenlijst[r])
    weight_table$w_klein[r] <- weight_table$proportie_cbs[r] / weight_table$proportie_vragenlijst[r]
    weight_table$w_groot[r] <- weight_table$n_cbs[r] / weight_table$n_vragenlijst[r]
  }

  # kleine gewichten worden afgekapt op weight.limits
  weight_table <- weight_table %>%
    mutate(
      w_klein = pmax(pmin(w_klein, weight.limits[2]), weight.limits[1])
    )

  # herschalen
  if(do.rescale){
    for(gemeente in weight_table$Gemeente){
      r <- weight_table$Gemeente == gemeente
      rescaling_factor <- sum(weight_table$n_vragenlijst[r]) / sum(weight_table$n_vragenlijst[r] * weight_table$w_klein[r])
      weight_table$w_klein[r] <- weight_table$w_klein[r] * rescaling_factor
      weight_table$w_groot[r] <- weight_table$w_groot[r] * rescaling_factor
    }
  }

  # join weights to original dataset based on stratum
  weights <- merge(
    x = data %>% mutate(
      GEMEENTECODE = as.character(as_factor(GEMEENTECODE)) # TODO make flexible with function arguments @@
    ),
    y = weight_table,
    by.x = by.x,
    by.y = by.y,
    all.x = T
  ) %>% select(-n_cbs, -n_vragenlijst, -proportie_cbs, -proportie_vragenlijst)

  # return statement
  if(!append){
    weights <- weights %>% select(!!sym(respondent.id), w_klein, w_groot)
  }
  return(weights)
}
