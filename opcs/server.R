##Reading in Library is necessary for functions to work properly
library(shiny)
library(dplyr)
library(datasets)
library(googleVis)
library(plotGoogleMaps)

# provide data "as of date"
date <- "20150827"

# Read in data file
file <- str_c("data/datafile_", date, ".csv")
datafile <- read.csv(file, stringsAsFactors = FALSE)

shinyServer(function(input, output, session) {
               
        myOptions <- reactive({
                list(
                        page = "enable",
                        pageSize = 20,
                        width=1500
                )
        })
        
        state_data <- reactive({
                # create placeholder selected_states variable to assign in if statements
                selected_states <- c()
                
                # if statements to handle "All states" input to state dropdown menu
                if("All states" %in% input$state){
                        selected_states <- unique(datafile$Proj.ST.Abbr)
                }
                
                if(!("All states" %in% input$state)){
                        selected_states <- input$state
                }
                
                filter(datafile, Proj.ST.Abbr %in% selected_states)        
        })
              
        # counties is a reactive variable that identifies the counties assosciated with the user's selection from the state input menu
        counties <- reactive({
                state_data <- state_data()
                state_data <- arrange(state_data, Proj.ST.Abbr, Proj.County.Name)
                unique(state_data$Proj.County.Name)                
        })
              
        # observe is a reactive function that repopulates the county select input menu using the reactive variable county
        observe({
                counties_all <- c("All counties", counties())
                updateSelectInput(session, "counties", choices = counties_all)
        })
        
        # filter data to only those states/counties selected
        data_table1 <- reactive({
                # assign previously created reactive variables to regular variables
                state_data <- state_data()
                counties <- counties()
                
                # create if statements to handle "All counties" option in dropdown menu
                if("All counties" %in% input$counties){
                        data_table1 <- filter(state_data, Proj.County.Name %in% counties)                        
                }
                
                if(!("All counties" %in% input$counties)){
                        data_table1 <- filter(state_data, Proj.County.Name %in% input$counties)                        
                }
                
#                 data_table1[ , c(1:3, 8:10, 18, 24, 26:28, 34:38)]  
                data_table1
        })
        
        # subset data to show/not show jobs or PI columns and show/not show Construction-only projects based on checkbox input
        data_table2 <- reactive({
                data_table1 <- data_table1()
                
                if(input$JobsPIFlag == FALSE){
                        data_table2 <- select(data_table1, -Jobs.Created, -Jobs.Saved, -Priv.Investment)
                }
                
                if(input$JobsPIFlag == TRUE){
                      data_table2 <- filter(data_table1, Cons.Non == "C" | Cons.Non == "B")
                }      
                
                data_table2
        })
        
        # filter data based on year input
        data_table <- reactive({
                data_table2 <- data_table2()
                range <- seq(input$years[1], input$years[2])
                data_table <- filter(data_table2, FY %in% range)
                data_table
        })
                
        # create output table
        output$table <- renderDataTable({
                data_table_output <- data_table()
                data_table_output
                }
                , options = list(pageLength = 10)
        )
        
        # create output for map
        output$map <- renderUI({
                data_table3 <- data_table()
                
                # create output for map when color-coding  by program
                program_map_data <- ""
                                
                                for(i in 1:nrow(data_table3)){
                                        lat <- data_table3$lat[i]
                                        lon <- data_table3$lon[i]
                                        tip <- data_table3$address[i]
                                        marker <- data_table3$program_markers[i]
                                        program_map_data <- str_c(program_map_data, "[", lat, ",", lon, ",", "\"", tip, "\"", ",", "\"", marker, "\"", "]")
                                        
                                        if(i == 1){
                                                program_map_data <- str_c("[", program_map_data)
                                        }
                                        if(i != nrow(data_table3)){
                                                program_map_data <- str_c(program_map_data, ",")
                                        }
                                        if(i == nrow(data_table3)){
                                                program_map_data <- str_c(program_map_data, "]")
                                        }
                                }
                
                # create output for map when color-coding  by year
                year_map_data <- ""
                
                for(i in 1:nrow(data_table3)){
                        lat <- data_table3$lat[i]
                        lon <- data_table3$lon[i]
                        tip <- data_table3$address[i]
                        marker <- data_table3$year_markers[i]
                        year_map_data <- str_c(year_map_data, "[", lat, ",", lon, ",", "\"", tip, "\"", ",", "\"", marker, "\"", "]")
                        
                        if(i == 1){
                                year_map_data <- str_c("[", year_map_data)
                        }
                        if(i != nrow(data_table3)){
                                year_map_data <- str_c(year_map_data, ",")
                        }
                        if(i == nrow(data_table3)){
                                year_map_data <- str_c(year_map_data, "]")
                        }
                }
                
                # create final_html to use for map iframe
                final_html <- ""
                
                if(input$marker_type == "By program type"){
                        final_html <- str_c(html1, program_map_data, html2, program_colors, html4)
                }
                if(input$marker_type == "By fiscal year awarded"){
                        final_html <- str_c(html1, year_map_data, html2, year_colors, html4)
                }
                
                # create iframe that gets passed to output$map
                tags$iframe(
                        srcdoc = final_html,
#                         srcdoc = "<img src=\"http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=Y%7cFFFF00%7c000000&.png\" />",
                        width = "100%",
                        height = "600px"
                )
        })

        # create download file
        output$downloadData <- downloadHandler(
                filename = function() {
                        str_c("datafile_", date, ".csv") 
                },
                content = function(file) {
                        write.csv(data_table(), file)
                }
        )
}
)

# create variable for text string of html gvismap output before and after data portion
html1 <- "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\"
\"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">
<html xmlns=\"http://www.w3.org/1999/xhtml\">
<head>
<title>Marker</title>
<meta http-equiv=\"content-type\" content=\"text/html;charset=utf-8\" />
<style type=\"text/css\">
body {
color: #444444;
font-family: Arial,Helvetica,sans-serif;
font-size: 75%;
}
a {
color: #4D87C7;
text-decoration: none;
}
</style>
</head>
<body>
<!-- Map generated in R 3.2.1 by googleVis 0.5.8 package -->
<!-- Sat Aug 29 10:07:33 2015 -->


<!-- jsHeader -->
<script type=\"text/javascript\">

// jsData 
function gvisDataMarker () {
var data = new google.visualization.DataTable();
var datajson ="

html2 <- ";
data.addColumn('number','lat');
data.addColumn('number','lon');
data.addColumn('string','address');
data.addColumn('string', 'marker');
data.addRows(datajson);
return(data);
}

// jsDrawChart
function drawChartMarker() {
var data = gvisDataMarker();
var options = {};
options[\"showTip\"] = true;
options[\"mapType\"] = \"normal\";
options[\"enableScrollWheel\"] = true;
options[\"icons\"] = "

program_colors <- "{'Public Works': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c5680FC%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c5680FC%7c000000&.png'
},
'Planning': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c66FF33%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c66FF33%7c000000&.png'
},
'Econ Adjst': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7cFFFF00%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7cFFFF00%7c000000&.png'
},
'Tech Asst': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c00FFFF%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c00FFFF%7c000000&.png'
},
'Disaster Supp': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7cFF6600%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7cFF6600%7c000000&.png'
},
'GCCMIF': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c9900CC%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c9900CC%7c000000&.png'
},
'Research': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c0000FF%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c0000FF%7c000000&.png'
},
'CTAA': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c006600%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c006600%7c000000&.png'
},
'Trade Adjst': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7cFF66FF%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7cFF66FF%7c000000&.png'
}
};"

year_colors <- "{'1990': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=90%7cFF0000%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=90%7cFF0000%7c000000&.png'
},
'1991': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=91%7cFF3300%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=91%7cFF3300%7c000000&.png'
},
'1992': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=92%7cC6600%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=92%7cC6600%7c000000&.png'
},
'1993': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=93%7cF9900%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=93%7cF9900%7c000000&.png'
},
'1994': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=94%7cCC9900%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=94%7cCC9900%7c000000&.png'
},
'1995': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=95%7cCCCC00%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=95%7cCCCC00%7c000000&.png'
},
'1996': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c99CC00%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=%7c99CC00%7c000000&.png'
},
'1997': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=97%7c669900%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=97%7c669900%7c000000&.png'
},
'1998': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=98%7c009900%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=98%7c009900%7c000000&.png'
},
'1999': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=99%7c009933%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=99%7c009933%7c000000&.png'
},
'2000': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=00%7c00CC00%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=00%7c00CC00%7c000000&.png'
},
'2001': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=01%7c00CC66%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=01%7c00CC66%7c000000&.png'
},
'2002': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=02%7c00CC99%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=02%7c00CC99%7c000000&.png'
},
'2003': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=03%7c009999%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=03%7c009999%7c000000&.png'
},
'2004': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=04%7c006699%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=04%7c006699%7c000000&.png'
},
'2005': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=05%7c0099CC%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=05%7c0099CC%7c000000&.png'
},
'2006': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=06%7c0066CC%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=06%7c0066CC%7c000000&.png'
},
'2007': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=07%7c0033CC%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=07%7c0033CC%7c000000&.png'
},
'2008': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=08%7c0000FF%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=08%7c0000FF%7c000000&.png'
},
'2009': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=09%7c3333FF%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=09%7c3333FF%7c000000&.png'
},
'2010': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=10%7c3333CC%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=10%7c3333CC%7c000000&.png'
},
'2011': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=11%7c6600FF%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=11%7c6600FF%7c000000&.png'
},
'2012': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=12%7c9933FF%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=12%7c9933FF%7c000000&.png'
},
'2013': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=13%7cCC00FF%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=13%7cCC00FF%7c000000&.png'
},
'2014': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=14%7cCC00CC%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=14%7cCC00CC%7c000000&.png'
},
'2015': {
'normal': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=15%7cCC0099%7c000000&.png',
'selected': 'http://chart.googleapis.com/chart?chst=d_map_pin_letter&chld=15%7cCC0099%7c000000&.png'
}
};"

html4 <- "var chart = new google.visualization.Map(
document.getElementById('Marker')
);
chart.draw(data,options);


}


// jsDisplayChart
(function() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
var chartid = \"map\";

// Manually see if chartid is in pkgs (not all browsers support Array.indexOf)
var i, newPackage = true;
for (i = 0; newPackage && i < pkgs.length; i++) {
if (pkgs[i] === chartid)
newPackage = false;
}
if (newPackage)
pkgs.push(chartid);

// Add the drawChart function to the global list of callbacks
callbacks.push(drawChartMarker);
})();
function displayChartMarker() {
var pkgs = window.__gvisPackages = window.__gvisPackages || [];
var callbacks = window.__gvisCallbacks = window.__gvisCallbacks || [];
window.clearTimeout(window.__gvisLoad);
// The timeout is set to 100 because otherwise the container div we are
// targeting might not be part of the document yet
window.__gvisLoad = setTimeout(function() {
var pkgCount = pkgs.length;
google.load(\"visualization\", \"1\", { packages:pkgs, callback: function() {
if (pkgCount != pkgs.length) {
// Race condition where another setTimeout call snuck in after us; if
// that call added a package, we must not shift its callback
return;
}
while (callbacks.length > 0)
callbacks.shift()();
} });
}, 100);
}

// jsFooter
</script>

<!-- jsChart -->  
<script type=\"text/javascript\" src=\"https://www.google.com/jsapi?callback=displayChartMarker\"></script>

<!-- divChart -->

<div id=\"Marker\" 
style=\"width: 500; height: automatic;\">
</div>
<div><span>Data: small &#8226; Chart ID: <a href=\"Chart_Marker.html\">Marker</a> &#8226; <a href=\"https://github.com/mages/googleVis\">googleVis-0.5.8</a></span><br /> 
<!-- htmlFooter -->
<span> 
R version 3.2.1 (2015-06-18) 
&#8226; <a href=\"https://developers.google.com/terms/\">Google Terms of Use</a> &#8226; <a href=\"https://google-developers.appspot.com/chart/interactive/docs/gallery/map\">Documentation and Data Policy</a>
</span></div>
</body>
</html>"




