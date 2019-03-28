/*
 * In house javascript
 */

// value of radio buttons, initialized to 2
var chooseSoils = 2;
var chooseComp = 2;
var calcFutures = 2;

$(document).ready(function(){
  // immedietly hide choose soil and veg options
  $("#chooseSoils").hide();
  $("#chooseComp").hide();
  // allow tooltips to show when highlighted
  $('[data-toggle="tooltip"]').tooltip();
  // show tooltip detail panel highlighting
  var heading;
  $('.glyphicon.glyphicon-info-sign').hover(function(){
    heading = $('#heading' + this.id.substr(this.id.length - 1));
    heading.attr("aria-expanded", "true");
  }, function(){
    heading.attr("aria-expanded", "false");
  })
  // when a radio button is clicked
  $('input[type="radio"]').click(function(){
    // show choose soils options when radio enabled
    if($(this).attr("value") == "chooseTrue"){
      $("#chooseSoils").show();
      chooseSoils = 2;
    }
    // hide choose soils options when radio enabled
    if($(this).attr("value") == "chooseFalse"){
      $("#chooseSoils").hide();
      chooseSoils = 1;
    }
    // button click for composition radios
    if($(this).attr("value") == "chooseCompTrue"){
      $("#chooseComp").show();
      chooseComp = 2;
    }
    if($(this).attr("value") == "chooseCompFalse"){
      $("#chooseComp").hide();
      chooseComp = 1;
    }
    // button click for future radios
    if($(this).attr("value") == "futureTrue"){
      calcFutures = 1;
    }
    if($(this).attr("value") == "futureFalse"){
      calcFutures = 2;
    }
  })
})
/*
 * Changes the marker lat and long values based on the lat long input fields
 * @returns void
 */
function changeMarkerXY(){
  var lat = document.getElementById("latMap").value;
  var long = document.getElementById("longMap").value;
  layerGroup.clearLayers();
  var marker = L.marker([lat, long], {draggable: true}).addTo(layerGroup);
  marker.on('drag', function(e){
    document.getElementById("latMap").value = marker.getLatLng().lat;
    document.getElementById("longMap").value = marker.getLatLng().lng;
  })
}
/*
 * Append status populating text dynamically
 * @returns void
 */
function changeFeedbackText(status){
  document.getElementById("feedback").innerHTML = document.getElementById("feedback").innerHTML + "<br>" + status;
}
/*
 * Adjusts silt values of the soil dynamically based on sand and clay values
 * @returns void
 */
function adjustSilt(){
  var sand = document.getElementById("sand").value;
  var clay = document.getElementById("clay").value;
  var siltVal = 100 - sand - clay;
  if (siltVal >= 0){
    document.getElementById("silt").value = siltVal;
  }
  drawPoint(sand, clay);
}
/*
 * Send information derived from user inputs to R
 * @returns void
 */
function sendToR(){
  // get user input lat and long coordinates
  var lat = parseFloat(document.getElementById("latMap").value);
  var long = parseFloat(document.getElementById("longMap").value);
  var sand = 34.0;
  var clay = 33.0;
  var silt = 33.0;
  var trees = 20.0;
  var shrubs = 20.0;
  var grasses = 20.0;
  var forbs = 20.0;
  var bareground = 20.0;
  // if the user selected to choose soils or comp, get their input values
  if (chooseSoils == 1){
    sand = parseFloat(document.getElementById("sand").value);
    clay = parseFloat(document.getElementById("clay").value);
    silt = parseFloat(document.getElementById("silt").value);
  }
  if (chooseComp == 1){
    trees = parseFloat(document.getElementById("trees").value);
    shrubs = parseFloat(document.getElementById("shrubs").value);
    grasses = parseFloat(document.getElementById("grasses").value);
    forbs = parseFloat(document.getElementById("forbs").value);
    bareground = parseFloat(document.getElementById("bareground").value);
  }
  // verify correct input values
  if ((trees + shrubs + grasses + forbs + bareground) != 100){
    alert("Composition must add up to 100. Composition is: " + (trees + shrubs + grasses + forbs + bareground))
  }
  else if (sand + clay + silt != 100){
    alert("Soils must add up to 100.");
  }
  else{
    // send values to RShiny, convert values to between 0 and 1
    Shiny.onInputChange("lat", lat);
    Shiny.onInputChange("lng", long);
    Shiny.onInputChange("future", calcFutures);
    Shiny.onInputChange("soils", chooseSoils);
    Shiny.onInputChange("sand", sand / 100);
    Shiny.onInputChange("clay", clay / 100 );
    Shiny.onInputChange("comp", chooseComp);
    Shiny.onInputChange("trees", trees / 100);
    Shiny.onInputChange("shrubs", shrubs / 100);
    Shiny.onInputChange("grasses", grasses / 100);
    Shiny.onInputChange("forbs", forbs / 100);
    Shiny.onInputChange("bg", bareground / 100);
    Shiny.onInputChange("simulate", true);
    // change options to make more user friendly
    if (calcFutures == 2) {calcFutures = "No";}
    else                  {calcFutures = "Yes";}
    if (chooseSoils == 1) {chooseSoils = "No";}
    else                  {chooseSoils = "Yes";}
    if (chooseComp == 1)  {chooseComp = "No";}
    else                  {chooseComp = "Yes";}
    // setup feedback status text
    changeFeedbackText("<br>Simulation running on location <span id='imp'>[" + lat + ", " +
                       long + "]</span> with calculate futures set to <span id='imp'>" + calcFutures +
                       "</span>.<br><br><pre>     Soils composition set to: </pre><span id='imp'>" +
                      "</span><br>Sand: <span id='imp'>" + sand + "</span><br>Clay: <span id='imp'>" + clay + "</span><br>Type: <span id='imp'>" + calcSoilType(sand, clay, silt) + "</span><br><br><pre>   Veg composition set to:</pre> <br>Trees: <span id='imp'>" + trees +
                       "</span><br>Shrubs: <span id='imp'>" + shrubs + "</span><br>Grasses: <span id='imp'>" + grasses +
                       "</span><br>Forbs: <span id='imp'>" + forbs + "</span><br>Bareground: <span id='imp'>" + bareground + "</span><br>");
   changeFeedbackText("<span id='imp'>Calculation Running...</span>");
  }
  /*
    * Calculates the type of soil a composition of sand, clay and silt are classified as
    * @returns string, indicating type of soil
  */
  function calcSoilType(sand, clay, silt){
    // categorize soil composition based on soil texture triangle
    if (sand > 85 && (silt + 1.5 * clay) < 15){
      return "Sand";
    }
    else if (sand >= 70 && sand <= 90 && (silt + (1.5 * clay) >= 15) && (silt + (2 * clay)) < 30){
      return "Loamy Sand";
    }
    else if (clay >= 7 && clay < 20 & sand > 52 && (silt + 2 * clay) >= 30){
      return "Sandy Loam";
    }
    else if (clay >= 7 && clay < 27 && silt >= 28 && silt < 50 && sand <= 52){
      return "Loam";
    }
    else if ((silt >= 50 && clay >= 12 && clay < 27) || (silt >= 50 && silt < 80 && clay < 12)){
      return "Silt Loam";
    }
    else if (silt >= 80 && clay < 12){
      return "Silt";
    }
    else if (clay >= 20 && clay < 35 && silt < 28 && sand > 45){
      return "Sandy Clay Loam";
    }
    else if (clay >= 27 && clay < 40 && sand > 20 && sand < 45){
      return "Clay Loam";
    }
    else if (clay >= 27 && clay < 40 && sand <= 20){
      return "Silty Clay Loam";
    }
    else if (clay >= 35 && sand >= 45){
      return "Sandy Clay";
    }
    else if (clay >= 40 && silt >= 40){
      return "Silty Clay";
    }
    else if (clay >= 40 && sand <= 45 && silt < 40){
      return "Clay";
    }
  }
}
