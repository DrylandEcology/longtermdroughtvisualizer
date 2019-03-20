// 1 = true, 2 = false
var chooseSoils = 2;
var chooseComp = 2;
var calcFutures = 2;

$(document).ready(function(){
  $("#chooseSoils").hide();
  $("#chooseComp").hide();
  // when a radio button is clicked
  $('input[type="radio"]').click(function(){
    // button click for soil radios
    if($(this).attr("value") == "chooseTrue"){
      $("#chooseSoils").show();
      chooseSoils = 2;
    }
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
    // button click for futrue radios
    if($(this).attr("value") == "futureTrue"){
      calcFutures = 1;
    }
    if($(this).attr("value") == "futureFalse"){
      calcFutures = 2;
    }
  })
})
function changeMarkerXY(){
  var lat = document.getElementById("latMap").value;
  var long = document.getElementById("longMap").value;
  //var map = L.map('map').setView([lat, long], 9);

  //L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
  //    attribution: '&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a> contributors'
  //}).addTo(map);
  layerGroup.clearLayers();
  var marker = L.marker([lat, long], {draggable: true}).addTo(layerGroup);
  marker.on('drag', function(e){
    document.getElementById("latMap").value = marker.getLatLng().lat;
    document.getElementById("longMap").value = marker.getLatLng().lng;
  })

  //document.getElementById("latMap").value = marker.getLatLng();
  //L.marker.setLatLng([lat, long]);
}

function changeFeedbackText(status){
  document.getElementById("feedback").innerHTML = document.getElementById("feedback").innerHTML + "<br>" + status;
}
function adjustSilt(){
  var sand = document.getElementById("sand").value;
  var clay = document.getElementById("clay").value;
  var siltVal = 100 - sand - clay;
  if (siltVal >= 0){
    document.getElementById("silt").value = siltVal;
  }
}
function sendToR(){
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
  if ((trees + shrubs + grasses + forbs + bareground) != 100){
    alert("Composition must add up to 100. Composition is: " + (trees + shrubs + grasses + forbs + bareground))
  }
  else if (sand + clay + silt != 100){
    alert("Soils must add up to 100.");
  }
  else{
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
    changeFeedbackText("<br><p><pre>                 Inputs</pre></p>Latitude: " + lat + "<br>Longitude: " + long + "<br>Calculate Futures: " + calcFutures + "<br>Choose Soils: " + chooseSoils +
    "<br>Choose Veg Composition " + chooseComp + "<br>Sand: " + sand + "<br>Clay: " + clay + "<br>Trees: " + trees + "<br>Shrubs: " + shrubs + "<br>Grasses: " + grasses +
    "<br>Forbs: " + forbs + "<br>Bareground: " + bareground + "<br>");
    changeFeedbackText("Calculation Running...");
  }
}
