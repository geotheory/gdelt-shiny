# A Shiny interface to GDELT 'doc 2.0' and 'geo 2.0' APIs
# 2017, Robin Edwards, @geotheory
# Shared under GNU General Public License v3.0

require(shiny)
require(shinyBS)
require(shinyjs)
require(shinythemes)
require(V8)
require(dplyr)
require(stringr)
require(lubridate)

# setwd('~/Documents/shinyapps.io/gdelt/')
tab = 'CONTENT'

gt_code = "<body>
<div id='google_translate_element'></div><script type='text/javascript'>
function googleTranslateElementInit() {
new google.translate.TranslateElement({pageLanguage: 'en', layout: google.translate.TranslateElement.InlineLayout.SIMPLE, multilanguagePage: true}, 'google_translate_element');
}
</script><script type='text/javascript' src='//translate.google.com/translate_a/element.js?cb=googleTranslateElementInit'></script>"

process_iframe = function(url){
  d = readLines(url) %>% paste(collapse='\n')
  tbl_ran = str_locate_all(d, 'TABLE') %>% range()
  header = substr(d, 1, tbl_ran[1]-2) %>% str_replace('<body>', gt_code) %>% 
    str_replace(fixed('html, body {'), 'html, body { max-width: 100%; overflow-x: hidden; ') # disable horiz scroll
  main   = substr(d, start = tbl_ran[1]-1, stop = tbl_ran[2]+1)
  # footer = substr(d, start = tbl_ran[2]+2, stop = nchar(d))
  footer = '</body></html>'
  main_head = substr(main, 1, str_locate(main, '<TR')[1]-1)
  main = substr(main, str_locate(main, '<TR')[1], nchar(main)) %>% str_replace("</TABLE>", '')
  main_foot = '</TABLE>'
  l = main %>% str_split('</TR>') %>% .[[1]] %>% subset(., . != '') %>% paste('</TR>')
  gt_root = 'https://translate.google.com/translate?hl=en&sl=auto&tl=en&u='
  gt_tail = '&sandbox=1'
  l2 = lapply(l, function(i){
    lnk = str_extract(i, 'javascript:window.open.*<B>') %>% str_extract('http[^\']*')
    str_replace(i, '</span>[ ]*</A>', paste0("</span></A><A href='", gt_root, lnk, gt_tail, "' target='_BLANK'>  (English)</A>")) %>% 
      str_replace(fixed("javascript:window.open('"), '') %>% str_replace(fixed("');\""), '" target="_BLANK"')
  }) %>% paste(collapse = '\n')
  paste(header, main_head, l2, main_foot, footer, collapse = '\n')
}

# some time calculation functions
now = as.Date(Sys.Date())
add_days = function(d, n){
  d <- ymd(d)
  d %m+% days(n)
}
# add_hours = function(d, n){
#   #d = as.POSIXct(d)
#   d <- ymd_hms(d)
#   d %m+% minutes(n*60)
# }
rf_date = function(x) paste0(substr(x,1,4), '-', substr(x,5,6), '-', substr(x,7,8))

# function to translate logical inputs into a search string
format_search_str = function(x, mode){
  mode = paste0(mode, ':')
  x1 = x %>% str_replace_all(., ' ', '') %>% str_replace(., '^', mode) %>% 
    str_replace(., paste0(mode, '-'), paste0('-', mode)) %>%  # move minus to prefix position
    str_replace(., paste0(mode, '%22-'), paste0('-', mode, '%22')) # for image tags which also have %22
  ifelse(any(str_detect(x1, '-')),
         paste(x1, collapse='%20'),
         ifelse(length(x1) > 1, paste0('(', paste(x1, collapse='%20OR%20'), ')'),
                paste(x1, collapse='%20OR%20'))
  )
}

# paths for GDELT's doc' and 'geo' APIs, documented at:
# https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/
# https://blog.gdeltproject.org/gdelt-geo-2-0-api-debuts/
root =     'https://api.gdeltproject.org/api/v2/doc/doc?query='
geo_root = 'https://api.gdeltproject.org/api/v2/geo/geo?query='

# Menus for the respective API modes available, with 'doc' API  modes grouped appropriately
content_modes = c('MEDIA - a simple list of news articles that matched the search' = 'ArtList',
                  'MEDIA-ART - art layout of news articles that matched the search AND that include a social sharing image' = 'ArtGallery',
                  'IMAGE COLLAGE - matching images. Most relevant when used with the image-related search terms.' = 'ImageCollage',
                  'IMAGE COLLAGE INFO - as for IMAGE COLLAGE but includes links' = 'ImageCollageInfo',
                  'IMAGE GALLERY - alternative layout to IMAGE COLLAGE INFO' = 'ImageGallery',
                  'IMAGE COLLAGE SHARE - lists social sharing images found in the matching articles, where present.' = 'ImageCollageShare',
                  'WORDCLOUD ENGLISH - wordcloud of English translations of most relevant articles.' = 'WordCloudEnglish',
                  'WORDCLOUD NATIVE - wordcloud of most relevant articles (author language).' = 'WordCloudNative',
                  'WORDCLOUD THEME - histogram of the GDELT GKG Themes assigned to each article.' = 'WordCloudTheme',
                  'WORDCLOUD IMAGE TAGS - histogram of image tags assigned by Google\'s Cloud Vision algorithms.' = 'WordCloudImageTags',
                  'WORDCLOUD IMAGE WEB-TAGS - as above, but based on taxonomy tags assigned by Google CV based on captions identified via reverse Google Images searches.' = 'WordCloudImageWebTags',
                  'TONE CHART - emotional histogram showing the tonal distribution of coverage of your query. Articles typically score within ±20 (max ±100).' = 'ToneChart')

timeline_modes = c('VOLUME - % of global news coverage' = 'TimelineVol', 
                   'VOLUME INFO - as VOLUME but includes interactive URL links to top sources' = 'TimelineVolInfo', 
                   'SENTIMENT - average "tone" of all matching coverage' = 'TimelineTone',
                   'LANGUAGE - volume breakdown by language (65 supported)' = 'TimelineLang',
                   'SOURCE COUNTRY - which countries are focusing the most on the topic' = 'TimelineSourceCountry')

geo_modes = c('POINT-DATA - displays a dot at each location mentioned in proximity to your search term. Image functionality is disabled' = 'PointData',
              'IMAGE POINT-DATA - as above, but for image searches. Search terms are disabled' = 'ImagePointData',
              'POINT HEATMAP - heatmap of the locations most closely associated with your search term (GeoJSON only)' = 'PointHeatmap',
              'IMAGE POINT HEATMAP - as above, but for image searches' = 'ImagePointHeatmap',
              'POINT ANIMATION - a series of heatmaps in 15 minute increments over the past 24 hours (GeoJSON only)' = 'PointAnimation',
              'IMAGE POINT-ANIMATION - as above, but for image searches' = 'ImagePointAnimation',
              'COUNTRY - aggregates all locations to country level. Also performs normalization, dividing number of mentions in context of your search by total mentions' = 'Country',
              'IMAGE COUNTRY - as above, but for image searches' = 'ImageCountry',
              'SOURCE COUNTRY - reflects the country or origin of your search results' = 'SourceCountry',
              'IMAGE SOURCE COUNTRY - as above, but for image searches' = 'ImageSourceCountry',
              'ADM1 - as COUNTRY but higher administrative granularity' = 'ADM1',
              'IMAGE ADM1 - as above, but for image searches' = 'ImageADM1')

sort_options = c('', 'Date: newest first' = 'DateDesc', 'Date: oldest first' = 'DateAsc', 'Tone: most positive first' = 'ToneDesc', 'Tone: most negative first' = 'ToneAsc')

# Options selection lists to read in
country_codes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT', sep='\t', stringsAsFactors = F, header = F)
country_codes = c('', setNames(as.character(country_codes$V1), country_codes$V2))
lang_codes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT', sep = '\t', stringsAsFactors = F, header = F)
lang_codes = c('', English = 'eng', setNames(as.character(lang_codes$V1), lang_codes$V2))
image_tags = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGETAGS.TXT', sep='\t', stringsAsFactors = F, header = F) %>% .[['V1']]
themes = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT', sep = '\t', stringsAsFactors = F, header = F) %>% 
  arrange(V1) %>% mutate(V1 = str_replace_all(V1, '_', ' ')) %>% .[['V1']] # underscores removed for aesthetic. Need to re-add to URL calls
adm1 = read.csv('http://data.gdeltproject.org/api/v2/guides/LOOKUP-ADM1S.TXT', header=F, sep='\t', stringsAsFactors = F)
adm1 = c('', setNames(as.character(adm1$V1), adm1$V2))

# SERVER --------------------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Tooltips
  tt_delay = list(delay = list(show=1500, hide=0))
  addTooltip(session, id = 'search_terms', title = 'Supports multiple terms separated by spaces, phrases in double quotes e.g. "cats and dogs", and OR if nested in parentheses, e.g. (cats OR dogs)', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'search_lang', title = 'Language of the search terms you are using, if not English. This lets you search for specific non-English words/phrases without any automatic translation. Matching content in this language is returned.', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'image_tags', title = 'Every image processed by GDELT is assigned one or more topical tags from a universe of more than 10,000 objects and activities recognized by Google algorithms', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'themes', title = 'Searches for any of the GDELT Global Knowledge Graph (GKG) Themes. GKG Themes offer a powerful way of searching for complex topics, since there can be numerous different phrases or names under a single heading. Key in likely relevant themes to find matching options. Words on the left denote the semantic hierarchy (NB. "TAX" seems to refer to taxonomy not taxation)', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'search_country', title = 'Country or countries where the target content has originated', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'search_domain', title = 'Internet domain of origin. Accepts multiple domains seperated by commas.', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'source_lang', title = 'Language(s) of the content you are searching for, which can be different to that of your search terms. So you can search for e.g. French content with English search terms - GDELT handles the interpretation', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'timespan', title = 'Specify recent period, e.g: "24h", "3w", "2m" (hours/weeks/months), or without code letter e.g. "30" for minutes', placement = "bottom", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'daterange', title = '(Functions when "Recent" is blank.) By default GDELT reports the most recent ~3 months, but you can specify any date range within this window', placement = "bottom", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'smooth', title = 'Line smooth option, using rolling average method', placement = "bottom", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'max_records', title = 'GDELT will return 75 by default, but this can be increased to 250', placement = "right", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'data_format', title = 'Specify format for data export', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'data_sort', title = 'By default results are sorted by relevance. You can also sort by date or article tone instead', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'geo_near', title = 'Returns all matches within a certain radius (bounding box) of a given point. You specify a particular latitude and longitude and distance in either miles (default) or kms (e.g. for 100km from Paris "48.85,2.35,100km")', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'geo_timespan', title = 'The geo portal searches the past 24 hours (1,440 mins), but this can be reduced further to a minimum timespan of 15 mins', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'geo_cc', title = 'Specify country of media mentions', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'geo_adm1', title = 'Specify ADM1 (top sub-national) geographical region of media mentions', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'geo_loc', title = 'Searches for a given word or phrase in the full formal name of the location - e.g. New York', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'geo_format', title = 'Specify format for data export', placement = "top", trigger = "hover", options = tt_delay)
  addTooltip(session, id = 'translate', title = 'Translate results in MEDIA LIST mode, and add Google Translate options for article links.', placement = "left", trigger = "hover", options = tt_delay)
  
  
  # feed large selections from server (instead of pre-loading)
  updateSelectizeInput(session = session, inputId = 'image_tags', choices = image_tags, server = TRUE)
  updateSelectizeInput(session = session, inputId = 'themes', choices = themes, server = TRUE)
  updateSelectizeInput(session = session, inputId = 'geo_adm1', choices = adm1, server = TRUE)
  
  observe({
    # logically build up the API search string. This forms part of the final URL call. Seperate items (AND logic) are seperated by space '%20'
    search = ''
    if(input$search_terms != '') search = input$search_terms
    search = str_replace_all(search, ' ', '%20')
    if(length(input$image_tags) > 0){
      img_tags = format_search_str(paste0('%22', input$image_tags, '%22'), 'imagewebtag')
      search = ifelse(search == '', paste0(search, img_tags), paste0(search, '%20', img_tags))
    }
    if(length(input$themes) > 0) for(i in input$themes) search = paste0(search, '%20theme:', str_replace_all(i,' ','_'))
    if(input$search_domain != ''){
      domains = str_split(input$search_domain, ',')[[1]]
      search = paste0(search, '%20', format_search_str(domains, 'domainis'))
    }
    if(length(input$search_country) > 0) search = paste0(search, '%20', format_search_str(input$search_country, 'sourcecountry'))
    if(length(input$source_lang) > 0) search = paste0(search, '%20', format_search_str(input$source_lang, 'sourcelang'))
    if(input$output_tab == 'GEO24'){
      if(input$geo_near != '') search = paste0(search, '%20near:', input$geo_near)
      if(input$geo_cc != '') search = paste0(search, '%20locationcc:', input$geo_cc)
      if(input$geo_adm1 != '') search = paste0(search, '%20locationadm1:', input$geo_adm1)
      if(input$geo_loc != ''){
        location = str_replace_all(input$geo_loc, '["\']', ''); 
        search = paste0(search, '%20location:%22', location, '%22')
      }
    } #else{ # params not supported by GEO API
    #  if(input$search_lang != '') search = paste0(search, '%20searchlang:', input$search_lang)
    #}
    # logically build up full URL, incorporating API search string
    if(input$output_tab == 'GEO24'){ # GEO MODE
      url = paste0(geo_root, search, '&mode=', input$geo_mode)
      if(input$geo_timespan != 1440) url = paste0(url, '&timespan=', input$geo_timespan) # timespan in mins
      if(input$geo_format != '') url = paste0(url, '&format=', input$geo_format) # output format
    } else{
      # CONTENT or TIMELINE modes
      # date params
      shinyjs::enable("daterange")
      timespan = ''
      if(input$timespan == ''){       # period defined by date range
        end_date = ifelse(input$daterange[2] == as.character(today()),
                          format(Sys.time(), '%Y%m%d%H%M%S'), 
                          format(as.Date(input$daterange[2]), paste0('%Y%m%d', '235959')))
        start_date = format(as.Date(input$daterange[1]), paste0('%Y%m%d', '000000'))
      } else{                          # period defined in past hours/days/weeks/months
        timespan = paste0('&timespan=', input$timespan)
      }
      
      # build URL
      url = paste0(root, search)
      if(input$search_lang != '') url = paste0(url, '&searchlang=', input$search_lang)
      if(input$output_tab == 'CONTENT'){
        url = paste0(url, '&mode=', input$content_mode)
        if(input$data_sort != '')  url = paste0(url, '&sort=', input$data_sort) # sort argument
      } else if(input$output_tab == 'TIMELINE'){
        url = paste0(url, '&mode=', input$timeline_mode, '&timelinesmooth=', input$smooth)
      }
      url = paste0(url, '&maxrecords=', input$max_records)
      if(input$data_format != '') url = paste0(url, '&format=', input$data_format)
      if(timespan != ''){
        # default hours
        url = paste0(url, timespan)
      } else{ 
        if(!all(is.na(input$daterange))){
          url = paste0(url, '&startdatetime=', start_date, '&enddatetime=', end_date)
        }
      }
    }
    
    # append args to URL in browser
    url_args = paste0('?', str_extract(url, '[a-z]+.[a-z]+[?]query=.*'))
    js$pageURL(url_args)

    # render to interface for reference and external use
    link_url = ifelse(input$content_mode %in% c('ArtList', 'ArtGallery'), url %>% paste0(ifelse(input$translate, '&trans=googtrans', '')), url) # translate
    output$gdelt_url = renderUI({ HTML(paste0("<a id='gdelt_url' href='", link_url), "' target='_blank'>", 
                                              str_replace(link_url, '&times', '&amp;times'), "</a>")})
    
    # Manage widget fuctionality
    if(input$output_tab == 'GEO24'){
      for(i in c('daterange', 'timespan', 'search_lang')) shinyjs::disable(i)
      for(i in c('max_records', 'data_format', 'data_sort')) shinyjs::hide(i)
    } else{
      for(i in c('daterange', 'timespan', 'search_lang')) shinyjs::enable(i)
      for(i in c('max_records', 'data_format', 'data_sort')) shinyjs::show(i)
      if(input$timespan == '') shinyjs::enable("daterange") else shinyjs::disable("daterange")
      if(input$output_tab == 'CONTENT'){
        shinyjs::enable("data_sort")
        if(input$content_mode %in% c('ArtList', 'ArtGallery')) shinyjs::enable("translate") else shinyjs::disable("translate")
        } else{
          shinyjs::disable("data_sort")
          shinyjs::disable("translate")
        }
    }
    
    # Title to detail search parameters
    search_title = ''
    if(input$search_terms != '') search_title = input$search_terms
    if(length(input$image_tags) > 0){
      if(search_title != '') search_title = paste(search_title, '+')
      search_title = paste(search_title, paste('image tags:', paste(input$image_tags, collapse=', ')))
    }
    if(length(input$themes) > 0){
      if(search_title != '') search_title = paste(search_title, '+')
      search_title = paste(search_title, paste('themes:', paste(input$themes, collapse=', ')))
    }
    output$plot_title = renderUI({ HTML(paste0('<h3>', search_title, '</h3>')) })
    
    # iframe container for API response
    # responsiveness. ~2/3 screen width for desktops; full-width for mobile devices
    w = ifelse(input$dimension[1] > 767, -15 + 2 * input$dimension[1]/3, input$dimension[1])
    h = ifelse(input$dimension[1] > 767, input$dimension[2]-70, input$dimension[2])
    
    if(input$translate & input$output_tab == 'CONTENT' & input$content_mode == 'ArtList'){
      edited_iframe = process_iframe(url)
      sink('www/iframe.html'); cat(edited_iframe); sink() # export to html
      output$frame <- renderUI({
        tags$iframe(src = './iframe.html', width = w, height = h)
        # HTML(edited_iframe)
      })
    } else {
      output$frame <- renderUI({
        tags$iframe(src = url, width = w, height = h)
      })
    }
  })
  
  # hash --------------------------------------------------------------------------------
  
  # initialise widgets according to hash arguments if provided
  # This whole horrible code section can be removed if necessary
  observeEvent(session$clientData$url_hash, {
    hash = gsub(pattern = "#", replacement = "", x = session$clientData$url_hash)
    if(hash == '') return() # no hash provided
    hash_args = parseQueryString(hash)
    
    # interpret query param (i.e. disaggregate query, imagetag and theme args)
    if(!str_detect(names(hash_args)[1], 'query')) warning('1st hash param is not query')
    if(hash_args[[1]] != ''){
      if(str_detect(hash_args[[1]], 'location:')){ # fix problem of spaces in location entries
        location_arg = str_extract(hash_args[[1]], 'location:"[a-zA-Z ]+"')
        hash_args[[1]] = str_replace(hash_args[[1]], 'location:"[a-zA-Z ]+"', str_replace_all(location_arg,' ','_'))
      }
      if(str_detect(hash_args[[1]], '[()]')){ # fix problem of OR login in brackets
        or_search_arg = str_extract(hash_args[[1]], '[(][a-zA-Z ]+[)]')
        hash_args[[1]] = str_replace(hash_args[[1]], '[(][a-zA-Z ]+[)]', str_replace_all(or_search_arg,' ','_'))
      }
      # parse search string for its arguments
      full_qry = hash_args[[1]] %>% str_replace_all(' OR ', ' ') %>% 
        str_replace_all('[ ]+', ' ') %>% str_split(' ') %>% .[[1]] %>% as_data_frame()
      # search
      search_qry = full_qry %>% filter(!str_detect(value, ':')) %>%   
        .[['value']] %>% paste(collapse = ' ') %>% str_replace_all('_', ' ')
      updateTextInput(session = session, inputId = 'search_terms', value = search_qry)
      # image
      image_qry = full_qry %>% filter(str_detect(value, 'imagewebtag:')) %>% 
        mutate(value = str_replace_all(value, '(imagewebtag:)|(")', '')) %>% .[['value']]
      updateSelectizeInput(session = session, inputId = 'image_tags', selected = image_qry[1]) # no effect
      # theme
      # theme_qry = full_qry %>% filter(str_detect(value, 'theme')) %>% .[['value']]
      # domain
      domain_qry = full_qry %>% filter(str_detect(value, 'domainis:')) %>% 
        mutate(value = str_replace(value, 'domainis:', '')) %>% .[['value']] %>% paste(collapse = ',')
      updateTextInput(session = session, inputId = 'search_domain', value = domain_qry)
      # sourcecountry
      sourcecountry_qry = full_qry %>% filter(str_detect(value, 'sourcecountry:')) %>% 
        mutate(value = str_replace(value, 'sourcecountry:', '')) %>% .[['value']]
      updateSelectInput(session = session, inputId = 'search_country', selected = sourcecountry_qry)
      # searchlang
      searchlang_qry = full_qry %>% filter(str_detect(value, 'searchlang:')) %>% 
        mutate(value = str_replace(value, 'searchlang:', '')) %>% .[['value']]
      updateSelectInput(session = session, inputId = 'search_lang', selected = searchlang_qry)
      # sourcelang
      sourcelang_qry = full_qry %>% filter(str_detect(value, 'sourcelang:')) %>% 
        mutate(value = str_replace(value, 'sourcelang:', '')) %>% .[['value']]
      updateSelectInput(session = session, inputId = 'source_lang', selected = sourcelang_qry)
      # locationcc
      locationcc_qry = full_qry %>% filter(str_detect(value, 'locationcc:')) %>% 
        mutate(value = str_replace(value, 'locationcc:', '')) %>% .[['value']]
      updateSelectInput(session = session, inputId = 'geo_cc', selected = locationcc_qry)
      # adm1
      locationadm1_qry = full_qry %>% filter(str_detect(value, 'locationadm1:')) %>% 
        mutate(value = str_replace(value, 'locationadm1:', '')) %>% .[['value']]
      updateSelectizeInput(session = session, inputId = 'geo_adm1', selected = locationadm1_qry) # no effect
      # location
      location_qry = full_qry %>% filter(str_detect(value, 'location:')) %>% 
        mutate(value = str_replace(value, 'location:', '')) %>% .[['value']] %>% 
        str_replace_all('"','') %>% str_replace_all('_', ' ')
      updateTextInput(session = session, inputId = 'geo_loc', value = location_qry)
      # location
      geo_near_qry = full_qry %>% filter(str_detect(value, 'near:')) %>% 
        mutate(value = str_replace(value, 'near:', '')) %>% .[['value']]
      updateTextInput(session = session, inputId = 'geo_near', value = geo_near_qry)
    }
    # non-query arguments
    if(!is.null(hash_args$startdatetime)){
      updateDateRangeInput(session=session, inputId = 'daterange', start = rf_date(hash_args$startdatetime), end = rf_date(hash_args$enddatetime))
    }
    if(!is.null(hash_args$maxrecords)) updateSliderInput(session=session, inputId = 'max_records', value = hash_args$maxrecords)
    if(!is.null(hash_args$timelinesmooth)) updateSliderInput(session=session, inputId = 'smooth', value = hash_args$timelinesmooth)
    if(!is.null(hash_args$format)){ # widget depends on API used
      if(str_detect(names(hash_args)[1], 'geo/geo')){
        updateSelectInput(session=session, inputId = 'geo_format', selected = hash_args$format)
      } else updateSelectInput(session=session, inputId = 'data_format', selected = hash_args$format)
    }
    if(!is.null(hash_args$sort)) updateSelectInput(session = session, inputId = 'data_sort', selected = hash_args$sort)
    # output tab and mode
    if(!is.null(hash_args$mode)){ # although else should not happen
      
      if(hash_args$mode %in% geo_modes){
        tab = 'GEO24'
        updateSelectInput(session = session, inputId = 'geo_mode', selected = hash_args$mode)
        # geo timespan
        if(!is.null(hash_args$timespan)) updateNumericInput(session=session, inputId = 'geo_timespan', value = hash_args$timespan)
      } else{ # content or timeline mode
        if(hash_args$mode %in% content_modes){
          tab = 'CONTENT' 
          updateSelectInput(session = session, inputId = 'content_mode', selected = hash_args$mode)
        } else {
          tab = 'TIMELINE'
          updateSelectInput(session = session, inputId = 'timeline_mode', selected = hash_args$mode)
        }
        # content/timeline timespan
        if(!is.null(hash_args$timespan)) updateTextInput(session=session, inputId = 'timespan', value = hash_args$timespan)
      }
      updateTabsetPanel(session = session, inputId = "output_tab", selected = tab)
    }
  }, once = T)
  
  
  # Help dialogue
  observeEvent(input$help, {
    showModal(modalDialog(
      h1("How to use GDELT"),
      hr(),
      p("GDELT is a web search tool for media, official and typically non-commercial content that offers some powerful alternative ways to find what you want (", a('GDELT home', href = 'https://www.gdeltproject.org/', target="_blank"), '/', a('reference', href = 'https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/', target="_blank"), ').' ," GDELT monitors global news media in multiple languages and processes it with advanced machine learning and deep learning algorithms to offer more intelligent search. Some usage examples include:"),
      tags$ul(
        tags$li('searching globally in English for content in local languages on a particular topic;'),
        tags$li('searching for all content posted in the past hour from Japan, or in Japanese, or from a particular website;'),
        tags$li('finding content based on the features and/or text found in its accompanying imagery;'),
        tags$li('finding content based on the themes it relates to; or'),
        tags$li('searching with a combination of these methods')
      ),
      p("The panel on the left structures the interface into two main groups - ", strong('inputs'), " and ", strong('outputs'), ". Inputs (top) are the search terms and other criteria that define the query. Outputs (below) are the CONTENT, TIMELINE and GEO24 tabs that group the various modes you can use to visualise and understand the results."),
      h3('INPUTS'),
      p('The service offers 3 ways to search for content:'),
      tags$ul(
        tags$li(strong('Search term'), " - any work or phrase. Phrases should be nested in double quotes - e.g. ", code('"the Ides of March"'),". Unquoted words seperated by spaces are interpretted as 'x and y' You can search for 'x or y' by enclosing terms in brackets and seperating with 'OR', e.g.", code('(cats OR dogs)'), ". Terms prefixed by the minus/hyphen symbol are interpretted as NOT, e.g.", code('-dogs')), 
        tags$li(strong('Image tags'), " - images within content are processed using deep learning algorithms to identify features and text they contain. Search for available tags in the dialogue box, or ", a(href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-IMAGETAGS.TXT', 'see the full list', target="_blank"), '.'), 
        tags$li(strong('Themes'), "- these offer a powerful way of searching for complex topics, since they can include hundreds or even thousands of different phrases or names under a single heading. Themes are based on GDELT's Global Knowledge Graph (GKG). Search for relevant themes in the dialogue box, or ", a(href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-GKGTHEMES.TXT', 'see the full list', target="_blank"), '.')
      ),
      p(strong('Combinations:'), 'These work together so e.g. you can specify a search for both search terms AND image tags. Note that when using the GEO24 output the available mode options tend to only work with either search terms or image tags - if you get an error message you may need to re-specify your criteria.'),
      p(strong('NOT criteria:'), "For most fields, inputs prefixed by the minus/hyphen symbol are interpretted as NOT, e.g. search term:", code('-trump'), ', image tag: ', code('-person'), ', country: ', code('-United Kingdom'), ', search languge: ', code('-eng'), ', domainis: ', code('-bbc.co.uk'), '.'),
      h4('Other criteria'),
      p("You can further narrow down your query in the following ways:"),
      tags$ul(
        tags$li(strong('Search language'), " - defines the language of the search terms if not English. (This feature seems to have some bugs.)"), 
        tags$li(strong('Country'), " - defines the country where the media outlets are located. (", a('country list', href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-COUNTRIES.TXT', target="_blank"), ')'), 
        tags$li(strong('Domain'), "- define web domain of content - e.g. website", code('bbc.co.uk'), ' or top-level domain ', code('.gov'), ' (US government content).'),
        tags$li(strong('Date range'), " - define any window for content dated in the past 3 months. The 'Hours' field must be empty to enable this."), 
        tags$li(strong('Hours'), " - set to return most recent content in terms of hours."),
        tags$li(strong('Source language'), " - defines the country where the media outlets are located. (", a('supported languages', href = 'http://data.gdeltproject.org/api/v2/guides/LOOKUP-LANGUAGES.TXT', target="_blank"), ')')
      ),
      h3('OUTPUTS'),
      tags$ul(
        tags$li(strong('CONTENT'), " - This tab offers the various modes to access the content matching the search."), 
        tags$li(strong('TIMELINE'), " - This offers the modes available to view volumetric trends for the query over time."), 
        tags$li(strong('GEO24'), " tab brings up a range of geographical tools to investigate media published in the past 24 hours. The ",em('"Geo mode"'), "dropdown gives the options. Some of the tools are image specific, and will only work for image tag searches (see ", em('IMAGE TAGS'), " tab). Others will work for search terms and not image tags. Most of the tools offer map-based insights not into media source locations but the places and countries mentioned in the content. You can specify your search in terms of countries, sub-national regions (NUTS1), place names or within a user-defined distance from a geographical point. This toolset uses a seperate API (", a('see documentation', href = 'https://blog.gdeltproject.org/gdelt-geo-2-0-api-debuts/', target="_blank"), ').')
      ),
      p("If you want to export the results from CONTENT or TIMELINE choose from the options under ", em("Format"),
        ". The URL at the bottom can be used to call the query output directly from the GDELT API."),
      p(strong("Translation:"), "If you check the 'GT' option box the results in CONTENT MEDIA list mode will include options for results translation as well as links to English translations of individual articles."),
      easyClose = TRUE
      , size = 'l'))
  })
}

# UI --------------------------------------------------------------------------------

pad = 'padding:0px 5px 0px 5px;'

# code to append app arguments to the URL
urlCode <- "shinyjs.pageURL = function(params){
if(params[0] != ''){ location.href = location.origin + location.pathname.slice(0, -1) + '/#' + params[0]; }
}"

ui <- fluidPage(
  theme = shinytheme("flatly"),
  # select existing textInput contents when clicking on it
  tags$script('$(document).ready(function(){ $("input").focus(function() { $(this).select(); } ); });'),
  # return screen dimensions (inc. responsively) to server to manage iframe dims
  tags$head(tags$script('
                        var dimension = 0;
                        $(document).on("shiny:connected", function(e) {
                        dimension = [window.innerWidth, window.innerHeight];
                        Shiny.onInputChange("dimension", dimension);
                        });
                        $(window).resize(function(e) {
                        dimension = [window.innerWidth, window.innerHeight];
                        Shiny.onInputChange("dimension", dimension);
                        });
                        ')
  ),
  tags$head(
    # custom css
    tags$style(HTML("
                    #sidebar { height: 100vh; overflow-y: auto; }
                    body { overflow:hidden; }

                    /* compress widgets a bit to fit on page */
                    hr {border-top: 1px solid #000; margin-top: 8px; margin-bottom: 12px; }
                    .selectize-input, .input-sm, .form-group, .shiny-input-container, .form-control
                    { padding:0px 5px 0px 5px; margin-bottom: 0px; min-height: 25px; }
                    .selectize-control { padding:0px 0px 0px 0px; }
                    .item { font-size: 11px; }
                    input { height: 27px; min-height: 27px; max-height: 27px; margin-bottom: 5px; padding:0px 5px 0px 5px; }

                    /* rescale iframe contents */
                    iframe {
                      overflow-x:hidden;
                      zoom: .8;
                      -webkit-zoom: .8;
                      -ms-zoom: .8;
                      -moz-transform: scale(.8, .8);
                      -webkit-transform: scale(1);
                      -o-transform: scale(1, 1);
                      -ms-transform: scale(1.25, 1.25);
                      transform: scale(1.25, 1.25);
                      -moz-transform-origin: top left;
                      -webkit-transform-origin: top left;
                      -o-transform-origin: top left;
                      -ms-transform-origin: top left;
                      transform-origin: top left;
                    }
                    @media screen and (-webkit-min-device-pixel-ratio:0) { #scaled-frame { zoom: .667;} }
                    
                    
                    /* remove numericInput increment buttons */
                    input[type=number]::-webkit-inner-spin-button,
                    input[type=number]::-webkit-outer-spin-button {
                    -webkit-appearance: none;
                    margin: 0;
                    }
                    #gdelt_url { color:#999; font-size: 90%; word-wrap: break-word; line-height: 100%; }
                    "))
    ),
  # Custom tooltips for widgets
  shinyjs::useShinyjs(),
  shinyBS:::shinyBSDep,
  
  # push arguments to URL
  extendShinyjs(text = urlCode),

  # UI - layout --------------------------------------------------------------------------------
  
  sidebarLayout(
    sidebarPanel(width = 4, id = 'sidebar',
                 fluidRow( column(12, h3("GDELT Project Interface"))),
                 fluidRow( column(12, p(a('GDELT', href = 'https://www.gdeltproject.org/', target="_blank"), " is the ", em("Global Database of Events, Language and Tone"), ", a multilingual web platform offering powerful search tools. This page is an interface to some GDELT APIs but has has no affiliation.", a(href = '#', 'Instructions.', onclick = "$('#help').trigger('click');"), a('Documentation.', href = 'https://blog.gdeltproject.org/gdelt-doc-2-0-api-debuts/', target="_blank")))),

                 # INPUTS
                 hr(),
                 tabsetPanel(id = 'input_tab',
                             tabPanel("SEARCH",
                                      fluidRow(
                                        column(12, textInput(inputId = 'search_terms', label = 'Search terms', value = 'Brexit'), style=pad)
                                      )
                             ),
                             tabPanel("IMAGES",
                                      fluidRow(
                                        column(12, selectizeInput(inputId = 'image_tags', label = 'Image tags', choices = NULL, selected = 1, multiple=T, options=list(create = T)), style=pad)
                                      )
                             ),
                             tabPanel("THEMES",
                                      fluidRow(
                                        column(12, selectizeInput(inputId = 'themes', label = 'Themes', choices = NULL, selected = 1, multiple = T), style=pad) #  (from GDELT Global Knowledge Graph)
                                      )
                             )
                 ),
                 fluidRow(
                   column(4, selectInput(inputId = 'search_lang', label = 'Search language', selected = 'English', choices = lang_codes), style=pad),
                   column(8, selectizeInput(inputId = 'source_lang', label = 'Source language(s)', choices = lang_codes, multiple = T, options=list(create = T)), style=pad)
                 ),
                 fluidRow(
                   column(6, selectizeInput(inputId = 'search_country', label = 'Country/ies of origin', choices = country_codes, multiple = T, options=list(create = T)), style=pad),
                   column(6, textInput(inputId = 'search_domain', label = 'Domain', value = '', placeholder = 'e.g. "bbc.co.uk"'), style=pad)
                 ),
                 fluidRow(
                   column(9, dateRangeInput(inputId = "daterange", label = "Date range",
                                            start = NULL, end = NULL,
                                            min = add_days(now, -82), max = now), style=pad),
                   column(3, textInput(inputId = 'timespan', label='Recent', value='24h'), style=pad)
                 ),
                 
                 # OUTPUTS       
                 hr(),
                 tabsetPanel(id = 'output_tab', selected = tab,
                             tabPanel("CONTENT",
                                      br(),
                                      fluidRow(
                                        column(12, selectInput(inputId = 'content_mode', label = 'Visualisation options', choices = content_modes, selected = 'Volume'), style=pad)
                                      )
                             ),
                             tabPanel("TIMELINE", 
                                      br(), #p('Timeline-based stats/data for matching media activity'),
                                      fluidRow(
                                        column(10, selectInput(inputId = 'timeline_mode', label = 'Statistic', choices = timeline_modes, selected = 'Volume'), style=pad),
                                        column(2, sliderInput('smooth', 'Smooth', 0, 5, 3, step = 1), style=pad)
                                      )
                             ),
                             tabPanel("GEO24",
                                      br(), #p('Geographic portal for matching media activity in the past 24 hours'),
                                      fluidRow(
                                        column(12, selectInput(inputId = 'geo_mode', label = 'Geo mode', choices = geo_modes, selected = ''), style=pad)
                                      ),
                                      fluidRow(
                                        column(4, sliderInput('geo_timespan', 'Time (m)', 15, 1440, 1440, step = 5), style=pad),
                                        column(4, selectInput(inputId = 'geo_format', label = 'Format', choices = c('','HTML','ImageHTML','ImageHTMLShow','GeoJSON','ImageGeoJSON'), selected = ''), style=pad),
                                        column(4, textInput(inputId = 'geo_near', label = 'Near', value = ''), style=pad)
                                      ),
                                      fluidRow(
                                        column(4, selectInput(inputId = 'geo_cc', label = 'Country', choices = country_codes, selected = 1, multiple=F), style=pad),
                                        column(5, selectizeInput(inputId = 'geo_adm1', label = 'ADM1', choices = NULL, selected = 1, multiple=F), style=pad),
                                        column(3, textInput(inputId = 'geo_loc', label = 'Location', value = ''), style=pad)
                                      )
                             )
                 ),
                 fluidRow(
                   column(3, sliderInput('max_records', 'Records', min=75, max=250, step=5, value=75), style=pad),
                   column(3, selectInput(inputId = 'data_format', label = 'Format', choices = c('','html','csv','rss','json','jsonp'), selected = ''), style=pad),
                   column(6, selectInput(inputId = 'data_sort', label = 'Sort', choices = sort_options, selected = ''), style=pad)
                 ),
                 hr(),
                 fluidRow(
                   column(10, uiOutput('gdelt_url')), 
                   column(2, checkboxInput(inputId = 'translate', label = 'GT', value = F))
                 ),
                 br(),
                 p('Page by Robin Edwards (@geotheory), 2017', id='footer', style="color:#999;font-size: 80%;")
    ),
    mainPanel(width = 8,
              fluidRow(
                column(11, uiOutput('plot_title')),
                column(1, actionButton("help", "Help"))
              ),
              fluidRow(htmlOutput("frame"))
    )
  )
)

shinyApp(ui, server)
