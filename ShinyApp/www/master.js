/*
 * In house javascript
 */
// value of radio buttons, initialized
var chooseSoils = 1;
var chooseComp = 1;
var calcFutures = 2;
// misc global variables for keeping track or state
var calcFuturesTxt;
var numRepeats = 0;
var valid = false;
// global variable init for user inputs
var lat = 35.1983;
var long = -111.6513;
var sand = 34.0;
var clay = 33.0;
var silt = 33.0;
var trees = 20.0;
var shrubs = 20.0;
var grasses = 20.0;
var forbs = 20.0;
var bareground = 20.0;

// do this stuff as soon as the document loads
$(document).ready(function(){
  // add click event listener to all accordion items
  var accItem = document.getElementsByClassName('accordionItem');
  var accHD = document.getElementsByClassName('accordionItemHeading');
  for (i = 0; i < accHD.length; i++) {
      accHD[i].addEventListener('click', toggleItem, false);
  }
  /*
   * Toggle open / close accordion items
   */
  function toggleItem() {
      var itemClass = this.parentNode.className;
      for (i = 0; i < accItem.length; i++) {
          accItem[i].className = 'accordionItem close';
      }
      if (itemClass == 'accordionItem close') {
          this.parentNode.className = 'accordionItem open';
      }
  }
  // immedietly hide choose soil and veg options
  $("#chooseSoils").hide();
  $("#chooseComp").hide();
  $("#simBtn").hide();
  $("#validTxt").css("color", "red");
  // allow tooltips to show when highlighted
  $('[data-toggle="tooltip"]').tooltip();
  // show tooltip detail panel highlighting
  var heading;
  $('.glyphicon.glyphicon-info-sign').hover(function(){
    heading = $('#accordionItemHeading' + this.id.substr(this.id.length - 1));
    heading.css("background-color", "#7f8c8d");
  }, function(){
    heading.css("background-color", "#FAFAFA");
  })
  // when a radio button is clicked
  $('input[type="radio"]').click(function(){
    // show choose soils options when radio enabled
    if($(this).attr("value") == "chooseTrue"){
      $("#chooseSoils").show();
      //if (screen.width > 1200) drawPoint(sand, clay);
      inValidate();
      chooseSoils = 1;
    }
    // hide choose soils options when radio enabled
    if($(this).attr("value") == "chooseFalse"){
      $("#chooseSoils").hide();
      inValidate();
      chooseSoils = 2;
    }
    // button click for composition radios
    if($(this).attr("value") == "chooseCompTrue"){
      $("#chooseComp").show();
      inValidate();
      chooseComp = 1;
    }
    if($(this).attr("value") == "chooseCompFalse"){
      $("#chooseComp").hide();
      inValidate();
      chooseComp = 2;
    }
    // button click for future radios
    if($(this).attr("value") == "futureTrue"){
      calcFutures = 1;
      inValidate();
    }
    if($(this).attr("value") == "futureFalse"){
      calcFutures = 2;
      inValidate();
    }
  })
})
/*
 * inValidates the the user inputs to make them reclick the validation button.
 */
function inValidate(){
  $("#validateBtn").show();
  $("#simBtn").hide();
  $("#validTxt").css("color", "red");
  document.getElementById("validTxt").innerHTML = "Click to validate inputs and see summary";
}
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
 * Change status populating text dynamically
 * @param status: (string) message to be dyanamically populating
 * @param append: (boolean) Append to or overwrite current status
 * @returns void
 */
function changeFeedbackText(status, append){
  if (append){
    document.getElementById("feedback").innerHTML = document.getElementById("feedback").innerHTML + "<br>" + status;
  }
  else{
    document.getElementById("feedback").innerHTML = status;
  }
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
}
function setGlobalInputs(){
  // future radio value
  var temp = $("input[name='futureSim']:checked").val();
  if(temp == "futureTrue"){
    calcFutures = 1;
  }
  if(temp == "futureFalse"){
    calcFutures = 2;
  }
  // choose soils value
  temp = $("input[name='chooseSoils']:checked").val();
  if(temp == "chooseTrue"){
    chooseSoils = 2;
  }
  if(temp == "chooseFalse"){
    chooseSoils = 1;
  }
  // choose composition value
  temp = $("input[name='chooseComp']:checked").val();
  if(temp == "chooseCompTrue"){
    chooseComp = 2;
  }
  if(temp == "chooseCompFalse"){
    chooseComp = 1;
  }
  // get user input lat and long coordinates
  lat = parseFloat(document.getElementById("latMap").value);
  long = parseFloat(document.getElementById("longMap").value);
  // if the user selected to choose soils or comp, get their input values
  if (chooseSoils == 2){
    sand = parseFloat(document.getElementById("sand").value);
    clay = parseFloat(document.getElementById("clay").value);
    silt = parseFloat(document.getElementById("silt").value);
  }
  if (chooseComp == 2){
    trees = parseFloat(document.getElementById("trees").value);
    shrubs = parseFloat(document.getElementById("shrubs").value);
    grasses = parseFloat(document.getElementById("grasses").value);
    forbs = parseFloat(document.getElementById("forbs").value);
    bareground = parseFloat(document.getElementById("bareground").value);
  }
}
/* Validates whether a given lat, long is within acceptable bounds
 * @returns string the state which the lat long falls in
 * @returns boolean false if it does not fall in an acceptable bounds
 */
function validateLocation(lat, long){
    // Arizona
    if (lat >= 31.3325 && lat <= 37.0004 && long <= -109.0475 && long >= -114.8126) return "Arizona";
    // New Mexico
    if (lat >= 31.3337 && lat <= 36.9982 && long <= -103.0023 && long >= -109.0489) return "New Mexico";
    /*
     * These are finished and bounds are 'perfect'
    */
    // Colorado
    if (lat >= 36.9949 && lat <= 41.0006 && long <= -102.0424 && long >= -109.0489) return "Colorado";
    // Utah
    // top rectangle
    if (lat >= 40.99752 && lat <= 41.9993 && long <= -111.04705 && long >= -114.0504) return "Upper Utah";
    // lower rectangle
    if (lat < 40.99752 && lat >= 36.99377 && long <= -109.05853 && long >= -114.0504) return "Lower Utah";
    return false;
}
/* Validates user inputs for simuation inputs
 * @returns boolean true if inputs are valid, otherwise false
 */
function validateInputs(){
  // get user input lat and long coordinates
  setGlobalInputs();
  // verify correct input values
  if ((trees + shrubs + grasses + forbs + bareground) != 100){
    alert("Composition must add up to 100. Composition is: " + (trees + shrubs + grasses + forbs + bareground));
    return false;
  }
  else if (sand + clay + silt != 100){
    alert("Soils must add up to 100. Soil texture is: " + (sand + clay + silt));
    return false;
  }
  // make sure that the lat and long the user entered is within AZ, NM, UT, CO
  // IN BETA, currently more inclusive than it needs to be and includes a small
  // part of some neighboring states.
  var state = validateLocation(lat, long);
  //console.log(state);
  if (!state){
    alert("Site location must be in the states AZ, UT, NM, or CO.");
    return false;
  }
  // change options to make more user friendly
  if (calcFutures == 2) {calcFuturesTxt = "No";}
  else                  {calcFuturesTxt = "Yes";}
  // determine soils and comp text based on user input
  var soilsTxt = "";
  var compTxt = "";
  if (chooseSoils == 2){
    soilsTxt = "<pre>     Soils composition set to: </pre><span id='imp'>" +
    "</span><br>Sand: <span id='imp'>" + sand + "</span><br>Clay: <span id='imp'>" + clay + "</span><br>Silt: <span id='imp'>" + silt + "</span><br>Type: <span id='imp'>" + calcSoilType(sand, clay, silt) + "</span></pre><br><br>";
  }
  else{
    soilsTxt = "<pre>Soils composition will be obtained from gSSURGO dataset</pre>";

  }
  if (chooseComp == 2){
    compTxt = "<pre>   Veg composition set to:</pre> <br>Trees: <span id='imp'>" + trees +
              "</span><br>Shrubs: <span id='imp'>" + shrubs + "</span><br>Grasses: <span id='imp'>" + grasses +
              "</span><br>Forbs: <span id='imp'>" + forbs + "</span><br>Bareground: <span id='imp'>" + bareground + "</span></pre><br>";
  }
  else{
    compTxt = "<pre>Veg composition will be generated based on site-specific climate</pre>";
  }
  // write feedback status text
  changeFeedbackText("<br>Simulation will be run on location <span id='imp'>[" + lat.toFixed(3) + ", " +
                     long.toFixed(3) + "]</span> with calculate futures set to <span id='imp'>" + calcFuturesTxt +
                     "</span>.<br><br>" + soilsTxt + compTxt, false);
  changeFeedbackText("<span id='imp'>Click Simulate to begin simulation...</span>", true);
  $("#simBtn").show();
  $("#validateBtn").hide();
  $("#validTxt").show();
  $("#validTxt").css("color", "green");
  document.getElementById("validTxt").innerHTML = "Inputs valid, simluation ready";
  alert("Input settings are valid, view 'Status' box for more information");
  valid = true;
  return true;
}
/*
 * Send information derived from user inputs to R
 * @returns void
 */
function sendToR(){
  //var validInputs = validateInputs();
  if (valid){
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
    // determine soils and comp text based on user input
    var soilsTxt = "";
    var compTxt = "";
    if (chooseSoils == 2){
      soilsTxt = "<pre>     Soils composition set to: </pre><span id='imp'>" +
      "</span><br>Sand: <span id='imp'>" + sand + "</span><br>Clay: <span id='imp'>" + clay + "</span><br>Silt: <span id='imp'>" + silt + "</span><br>Type: <span id='imp'>" + calcSoilType(sand, clay, silt) + "</span></pre><br><br>";
    }
    else{
      soilsTxt = "<pre>Obtaining soils composition from gSSURGO dataset</pre>";
    }
    if (chooseComp == 2){
      compTxt = "<pre> Vegetation composition set to:</pre> <br>Trees: <span id='imp'>" + trees +
                "</span><br>Shrubs: <span id='imp'>" + shrubs + "</span><br>Grasses: <span id='imp'>" + grasses +
                "</span><br>Forbs: <span id='imp'>" + forbs + "</span><br>Bareground: <span id='imp'>" + bareground + "</span></pre><br>";
    }
    else{
      compTxt = "<pre> Vegetation composition will be generated based on site-specific climate </pre>";
    }
    // write feedback status text
    changeFeedbackText("<br>Simulation will be run on location <span id='imp'>[" + lat.toFixed(3) + ", " +
                       long.toFixed(3) + "]</span> with calculate futures set to <span id='imp'>" + calcFuturesTxt +
                       "</span>.<br><br>" + soilsTxt + compTxt, false);
    changeFeedbackText("<span id='imp'>Calculation Running...</span>", true);
    toggleSimButtonActive();
    document.getElementById("validTxt").innerHTML = "Simulation currently running";
    // Give the user feedback in realtime
    // given the async nature of javascript, time should be absolute not relative
    if (calcFutures == 1){
      repeatWithTimer(function(){changeFeedbackText("Formatting inputs...", true)}, 2500, 1);
      repeatWithTimer(function(){changeFeedbackText("Gathering historical climatology...", true)}, 5000, 1);
      repeatWithTimer(function () { changeFeedbackText("Simulating historical ecohydrology...", true); }, 10000, 1);
      repeatWithTimer(function(){changeFeedbackText("Gathering future climatology...", true)}, 15000, 1);
      repeatWithTimer(function(){changeFeedbackText("Gathering future climatology...", true)}, 45000, 1);
      repeatWithTimer(function(){changeFeedbackText("Gathering future climatology...", true)}, 75000, 1);
      repeatWithTimer(function(){changeFeedbackText("Gathering future climatology...", true)}, 105000, 1);
      repeatWithTimer(function(){changeFeedbackText("Gathering future climatology...", true)}, 135000, 1);
      repeatWithTimer(function(){changeFeedbackText("Gathering future climatology...", true)}, 165000, 1);
      repeatWithTimer(function(){changeFeedbackText("Gathering future climatology...", true)}, 195000, 1);
      repeatWithTimer(function(){changeFeedbackText("Gathering of climate data is taking longer than usual. Rest assurred the app is still working.", true)}, 230000, 1);
    }
    else if (calcFutures == 2){
      repeatWithTimer(function(){changeFeedbackText("Formatting inputs...", true)}, 2500, 1);
      repeatWithTimer(function(){changeFeedbackText("Gathering historical climatology...", true)}, 5000, 1);
      repeatWithTimer(function(){changeFeedbackText("Simulating historical ecohydrology...", true)}, 10000, 1);
      repeatWithTimer(function(){changeFeedbackText("Formatting results...", true)}, 15000, 1);
      repeatWithTimer(function(){toggleSimButtonActive()}, 15000, 1);
    }
  }
}
/*
 * Toggle the sim button being clickable. Ie, if the button is clickable when
 * function is called it will become unclickable and vice versa.
 */
function toggleSimButtonActive(){
  if (document.getElementById("simBtn").disabled){
    document.getElementById("simBtn").disabled = false;
    document.getElementById("validTxt").innerHTML = "Inputs valid, simluation ready";
    // change button color
    document.getElementById("simBtn").style.background = '#FFFFFF';
  }
  else{
    document.getElementById("simBtn").disabled = true;
    // change button color
    document.getElementById("simBtn").style.background = '#000000';
  }
}
  /*
   * Repeats a passed function numRepeats times with a delay of time.
   * @param functionIn: (function) function to call
   * @param time: (int) number of milliseconds between each delay
   * @param numRepeats: (int) number of times to repeat the function call
  */
  function repeatWithTimer(functionIn, time, numRepeats){
    setTimeout(function(){
        functionIn();
        if(numRepeats > 1) repeatWithTimer(functionIn, time, numRepeats - 1);
    }, time);
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
/* Drag and drop functions for soils texture triangle */
function allowDrop(ev) {
  ev.preventDefault();
}
function drag(ev) {
  ev.dataTransfer.setData("text", ev.target.id);
}
function drop(ev) {
  ev.preventDefault();
  var data = ev.dataTransfer.getData("text");
  ev.target.appendChild(document.getElementById(data));
}