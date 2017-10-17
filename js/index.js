$(document).ready(function() {
		$(".button-collapse").sideNav();
    
	});

$(document).ready(function(){
    $('ul.tabs').tabs();
  });
      

function getWeather() {

    // Get a free key at http://openweathermap.org/. Replace the "Your_Key_Here" string with that key.
    var OpenWeatherAppKey = "4dceda87ae307b1ef049d014dbca77d3";
    
    var queryString = 'http://api.openweathermap.org/data/2.5/forecast?id=4140963&appid=' + OpenWeatherAppKey + "&units=metric";
    console.log(queryString);
    
    $.getJSON(queryString, function (results) {
    

        if (results.list.length) {


            $.getJSON(queryString, function (results) {
                
                var weatherList =[];
                    var weatherList1 =[];
                    var weatherList2 =[];
                    var weatherList3 =[];
                    var weatherList4 =[];

                if (results.list.length) {
                    
                    var datacsv = [];
                     var instant = 17380;
                    for (var i = 0; i < results.list.length; i++) {
                       
                
                        var date = results.list[i].dt_txt.split(" ");
                        var dteday = date[0];
                        var season = 4;
                        var yr = 2017;
                        var mnth = date[0].split("-")[1];
                        
                        if (mnth[0]==0) {
                            mnth=mnth[1];
                        }
                        var hr = (date[1].split(":"))[0];
                        if (parseInt(hr)==0){
                            hr =24;
                        }
                        
                        var weekday = new Date(dteday).getDay();
                        
                        var workingday = 1;
                        
                        if (weekday == 6 || weekday == 0){
                            workingday=0;
                        }
                        
                        var holidays = ['2017-01-02','2017-01-16','2017-01-20','2017-02-20','2017-04-17','2017-05-29','2017-07-04','2017-09-04', '2017-10-09','2017-11-10','2017-11-23','2017-12-25'];
                        
                        
                        var holiday = 0;
                        
                        if (contains(holidays,dteday)) {
                            holiday =1;
                            workingday =0;
                        }
                        
                        var temp = (results.list[i].main.temp)/41;
                        
                        if (temp<0){
                            temp=0;
                        }
                        var atemp=temp;
                        var hum = (results.list[i].main.humidity)/100;
                        var windspeed = (results.list[i].wind.speed)/67;
                        
                        var weatherDes = results.list[i].weather[0].description;
                      
                        
                        
                        var weathersit = 0;
                        
                        
                    weather1 = ['clear sky', 'few clouds', 'scattered clouds', 'overcast clouds'];
		              weather2 = ['mist','broken clouds'];
                        weather3 = ['light snow', 'light rain', 'thunderstorm', 'moderate rain'];
                        weather4 = ['heavy rain', 'ice pallets', 'snow'];
                        
                        if (contains(weather1,weatherDes)) {
                            weathersit = 1;
                        }
                        
                        if (contains(weather2,weatherDes)) {
                            weathersit = 2;
                        }
                        
                        if (contains(weather3,weatherDes)) {
                            weathersit = 3;
                        }
                        
                        if (contains(weather4,weatherDes)) {
                            weathersit = 4;
                        }
                        
                        
                        datacsv.push([instant,dteday,season,yr,mnth,parseInt(hr)%24,holiday,weekday,workingday,weathersit,temp,atemp,hum,windspeed]);
                        datacsv.push([instant,dteday,season,yr,mnth,(parseInt(hr)+1)%24,holiday,weekday,workingday,weathersit,temp,atemp,hum,windspeed]);
                        datacsv.push([instant,dteday,season,yr,mnth,(parseInt(hr)+2)%24,holiday,weekday,workingday,weathersit,temp,atemp,hum,windspeed]);
                        
                       instant = instant +1; 
                        
                         
                    var today = new Date().getDate();
                        
                        var dd = date[0].split("-")[2];
                    
                         if (dd == today) {
                             weatherList.push(weathersit);
                         }
                        
                         if (dd == (today+1)) {
                              weatherList1.push(weathersit);
                         }
                        
                        if (dd == (today+2)) {
                             weatherList2.push(weathersit);
                         }
                        if (dd == (today+3)) {
                              weatherList3.push(weathersit);
                         }
                        if (dd == (today+4)) {
                              weatherList4.push(weathersit);
                         }
                        
                        
                    
                    
                    }
                
                    }
                
                    
                        
                   
                    
                
                    
                    if (maxOcc(weatherList) == 1){
                         document.getElementById('today').innerHTML += "<i class=\"material-icons\">wb_sunny</i>";
                    }
                    if (maxOcc(weatherList) == 2){
                         document.getElementById('today').innerHTML += "<i class=\"material-icons\">filter_drama</i>";
                    }
                    if (maxOcc(weatherList) == 3){
                         document.getElementById('today').innerHTML += "<i class=\"material-icons\">wb_cloudy</i>";
                    }
                    if (maxOcc(weatherList) == 4){
                        document.getElementById('today').innerHTML += "<i class=\"material-icons\">flash_on</i>";
                    }
                    
                
                if (maxOcc(weatherList1) == 1){
                         document.getElementById('today1').innerHTML += "<i class=\"material-icons\">wb_sunny</i>";
                    }
                    if (maxOcc(weatherList1) == 2){
                         document.getElementById('today1').innerHTML += "<i class=\"material-icons\">filter_drama</i>";
                    }
                    if (maxOcc(weatherList1) == 3){
                         document.getElementById('today1').innerHTML += "<i class=\"material-icons\">wb_cloudy</i>";
                    }
                    if (maxOcc(weatherList1) == 4){
                        document.getElementById('today1').innerHTML += "<i class=\"material-icons\">flash_on</i>";
                    }
                    
                
                if (maxOcc(weatherList2) == 1){
                         document.getElementById('today2').innerHTML += "<i class=\"material-icons\">wb_sunny</i>";
                    }
                    if (maxOcc(weatherList2) == 2){
                         document.getElementById('today2').innerHTML += "<i class=\"material-icons\">filter_drama</i>";
                    }
                    if (maxOcc(weatherList2) == 3){
                         document.getElementById('today2').innerHTML += "<i class=\"material-icons\">wb_cloudy</i>";
                    }
                    if (maxOcc(weatherList2) == 4){
                        document.getElementById('today2').innerHTML += "<i class=\"material-icons\">flash_on</i>";
                    }
                
                 if (maxOcc(weatherList3) == 1){
                         document.getElementById('today3').innerHTML += "<i class=\"material-icons\">wb_sunny</i>";
                    }
                    if (maxOcc(weatherList3) == 2){
                         document.getElementById('today3').innerHTML += "<i class=\"material-icons\">filter_drama</i>";
                    }
                    if (maxOcc(weatherList3) == 3){
                         document.getElementById('today3').innerHTML += "<i class=\"material-icons\">wb_cloudy</i>";
                    }
                    if (maxOcc(weatherList3) == 4){
                        document.getElementById('today3').innerHTML += "<i class=\"material-icons\">flash_on</i>";
                    }
                
                
                
                 if (maxOcc(weatherList4) == 1){
                         document.getElementById('today4').innerHTML += "<i class=\"material-icons\">wb_sunny</i>";
                    }
                    if (maxOcc(weatherList4) == 2){
                         document.getElementById('today4').innerHTML += "<i class=\"material-icons\">filter_drama</i>";
                    }
                    if (maxOcc(weatherList4) == 3){
                         document.getElementById('today4').innerHTML += "<i class=\"material-icons\">wb_cloudy</i>";
                    }
                    if (maxOcc(weatherList4) == 4){
                        document.getElementById('today4').innerHTML += "<i class=\"material-icons\">flash_on</i>";
                    }            
                    
                    var csvContent = "instant,dteday,season,yr,mnth,hr,holiday,weekday,workingday,weathersit,temp,atemp,hum,windspeed \n" ;
                    datacsv.forEach(function(infoArray, index){
                        dataString = infoArray.join(",");
                    csvContent += index < datacsv.length ? dataString+ "\n" : dataString;
                });
                
                writeCSV(csvContent);
                

            });
        }
    }).fail(function () {
        console.log("error getting location");
    });
}

function getDate() {
    var weekDay = ['Dimanche', 'Lundi', 'Mardi', 'Mercredi', 'Jeudi', 'Vendredi', 'Samedi'];
    var month = ['Janvier', 'Février', 'Mars', 'Avril', 'Mai', 'Juin', 'Juillet', 'Août', 'Septembre', 'Octobre', 'Novembre', 'Décembre'];
    
    var today = new Date();
    var day = today.getDay();
    var dd = today.getDate();
    var mm = today.getMonth();
    var yyyy = today.getFullYear();

    today =weekDay[day]+' '+dd+' ' +month[mm]+' ' +yyyy;
    document.getElementById('today').innerHTML += today;
    
    
    today1 =weekDay[(day+1)%7]+' '+(dd+1)%31+' ' +month[mm]+' ' +yyyy;
    document.getElementById('today1').innerHTML += today1;
    
    
    today2 =weekDay[(day+2)%7]+' '+(dd+2)%31+' ' +month[mm]+' ' +yyyy;
    document.getElementById('today2').innerHTML += today2;
    
    today3 =weekDay[(day+3)%7]+' '+(dd+3)%31 +' ' +month[mm]+' ' +yyyy;
    document.getElementById('today3').innerHTML += today3;
    
    today4 =weekDay[(day+4)%7]+' '+(dd+4)%31+' ' +month[mm]+' ' +yyyy;
    document.getElementById('today4').innerHTML += today4;
    
}


function onWeatherError(error) {
    console.log('code: ' + error.code + '\n' +
        'message: ' + error.message + '\n');
}

getWeather();
getDate();

function writeCSV(csvContent) {
    $.post("write.php", {csv : csvContent} );
}


function contains(a, obj) {
    for (var i = 0; i < a.length; i++) {
        if (a[i] === obj) {
            return true;
        }
    }
    return false;
}

function prediction(){
    $.post( "exec.php", function() {
        //alert( "success" );
    });
    
    $.ajax({
        type: "GET",
        url: "result.csv",
        dataType: "text",
        success: function(data) {displayInfo(data);}
     });
}

function displayInfo(allText) {
    var allTextLines = allText.split(/\r\n|\n/);
    var headers = allTextLines[0].split(',');
    var lines = [];
    
    var today = new Date().getDate();
    var table = "<table class=\"centered\"><tbody><thead><tr><th>Heure</th><th>Habitués</th><th>Enregistrés</th><th>Total</th></tr></thead><tbody>";
    var table1 = table;
    var table2 = table;
    var table3 = table;
    var table4 = table;
    
    var ttday = 0;
    var ttday1 =0;
    var ttday2 =0;
    var ttday3 =0;
    var ttday4 =0;
    
                    
    
    for (var i=1; i<allTextLines.length; i++) {
        var data = allTextLines[i].split(',');
        if (data.length == headers.length) {
            
            var hr = data[1].split('"')[1];
            hr = (parseInt(hr)%24).toString();
        
            if (hr.length ==1) {
                hr = "0"+hr;
            }
            hr = hr+":00";
            
            var dd = (data[0].split('-')[2]).split('"')[0];
            if (dd == today) {
                ttday = ttday+Math.round(data[4]);    
                table += "<tr><td>" + hr + '</td>';
                table += "<td>" + Math.round(data[2]) + '</td>';
                table += "<td>" +  Math.round(data[3]) + '</td>';
                table += "<td>" +  Math.round(data[4]) + '</td></tr>';
                             
            }
            
            if (dd == today+1) {
                ttday1 = ttday1+Math.round(data[4]);
                table1 += "<tr><td>" + hr + '</td>';
                table1 += "<td>" + Math.round(data[2]) + '</td>';
                table1 += "<td>" +  Math.round(data[3]) + '</td>';
                table1 += "<td>" +  Math.round(data[4]) + '</td></tr>';
                             
            }
            
            if (dd == today+2) {
                ttday2 = ttday2+Math.round(data[4]);
                table2 += "<tr><td>" + hr + '</td>';
                table2 += "<td>" + Math.round(data[2]) + '</td>';
                table2 += "<td>" +  Math.round(data[3]) + '</td>';
                table2 += "<td>" +  Math.round(data[4]) + '</td></tr>';
                             
            }
            
            if (dd == today+3) {
                ttday3 = ttday3+Math.round(data[4]);
                table3 += "<tr><td>" + hr + '</td>';
                table3 += "<td>" + Math.round(data[2]) + '</td>';
                table3 += "<td>" +  Math.round(data[3]) + '</td>';
                table3 += "<td>" +  Math.round(data[4]) + '</td></tr>';
                             
            }
            
            if (dd == today+4) {
                ttday4 = ttday4+Math.round(data[4]);
                table4 += "<tr><td>" + hr + '</td>';
                table4 += "<td>" + Math.round(data[2]) + '</td>';
                table4 += "<td>" +  Math.round(data[3]) + '</td>';
                table4 += "<td>" +  Math.round(data[4]) + '</td></tr>';
                             
            }
            
                
            
            }
        }
    
       table += '</tbody></table>';
                    table1 += '</tbody></table>';
                    table2 += '</tbody></table>';
                    table3 += '</tbody></table>';
                    table4 += '</tbody></table>';
                     document.getElementById('table').innerHTML = table;
                    document.getElementById('table1').innerHTML = table1;
                    document.getElementById('table2').innerHTML = table2;
                    document.getElementById('table3').innerHTML = table3;
                    document.getElementById('table4').innerHTML = table4;
    
    document.getElementById('today').innerHTML += "<span class=\"new badge\" data-badge-caption=\"\">"+ttday+"</span>";
    document.getElementById('today1').innerHTML += "<span class=\"new badge\" data-badge-caption=\"\">"+ttday1+"</span>";
    document.getElementById('today2').innerHTML += "<span class=\"new badge\" data-badge-caption=\"\">"+ttday2+"</span>";
    document.getElementById('today3').innerHTML += "<span class=\"new badge\" data-badge-caption=\"\">"+ttday3+"</span>";
    document.getElementById('today4').innerHTML += "<span class=\"new badge\" data-badge-caption=\"\">"+ttday4+"</span>";
    

    }
     

function maxOcc(array) {
    if(array.length == 0)
        return null;
    var modeMap = {};
    var maxEl = array[0], maxCount = 1;
    for(var i = 0; i < array.length; i++)
    {
        var el = array[i];
        if(modeMap[el] == null)
            modeMap[el] = 1;
        else
            modeMap[el]++;  
        if(modeMap[el] > maxCount)
        {
            maxEl = el;
            maxCount = modeMap[el];
        }
    }
    return maxEl;
}

    /*var today = new Date().getDate();
                    var table = "<table><tbody>";
                    var table1 = "<table><tbody>";
                    var table2 = "<table><tbody>";
                    var table3 = "<table><tbody>";
                    var table4 = "<table><tbody>";
                    
                    
                    for (var i = 0; i < results.list.length; i++) {
                        
                        var date = results.list[i].dt_txt.split(" ");
                        var dateSplit = date[0].split("-");
                        var dd = dateSplit[2];
                        
                        var hour = date[1].split(":")[0] + ':00' ;
                        
                         if (dd == today) {
                             table += "<tr><td>" + hour + '</td>';
                             table += "<td>" + results.list[i].weather[0].description + '</td>';
                             table += "<td>" + results.list[i].main.temp + '</td>';
                              table += "<td>" + results.list[i].main.humidity + '</td>';
                              table += "<td>" + results.list[i].wind.speed + '</td></tr>';
                             
                             
                         }
                        
                         if (dd == (today+1)) {
                             table1 += "<tr><td>" + hour + '</td>';
                             table1 += "<td>" + results.list[i].weather[0].description + '</td>';
                             table1 += "<td>" + results.list[i].main.temp + '</td>';
                              table1 += "<td>" + results.list[i].main.humidity + '</td>';
                              table1 += "<td>" + results.list[i].wind.speed + '</td></tr>';
                         }
                        
                        if (dd == (today+2)) {
                             table2 += "<tr><td>" + hour + '</td>';
                             table2 += "<td>" + results.list[i].weather[0].description + '</td>';
                             table2 += "<td>" + results.list[i].main.temp + '</td>';
                              table2 += "<td>" + results.list[i].main.humidity + '</td>';
                              table2 += "<td>" + results.list[i].wind.speed + '</td></tr>';
                         }
                        if (dd == (today+3)) {
                             table3 += "<tr><td>" + hour + '</td>';
                             table3 += "<td>" + results.list[i].weather[0].description + '</td>';
                             table3 += "<td>" + results.list[i].main.temp + '</td>';
                              table3 += "<td>" + results.list[i].main.humidity + '</td>';
                              table3 += "<td>" + results.list[i].wind.speed + '</td></tr>';
                         }
                        if (dd == (today+4)) {
                             table4 += "<tr><td>" + hour + '</td>';
                             table4 += "<td>" + results.list[i].weather[0].description + '</td>';
                             table4 += "<td>" + results.list[i].main.temp + '</td>';
                              table4 += "<td>" + results.list[i].main.humidity + '</td>';
                              table4 += "<td>" + results.list[i].wind.speed + '</td></tr>';
                         }
                        
                    }

                    table += '</tbody></table>';
                    table1 += '</tbody></table>';
                    table2 += '</tbody></table>';
                    table3 += '</tbody></table>';
                    table4 += '</tbody></table>';
                     document.getElementById('table').innerHTML = table;
                    document.getElementById('table1').innerHTML = table1;
                    document.getElementById('table2').innerHTML = table2;
                    document.getElementById('table3').innerHTML = table3;
                    document.getElementById('table4').innerHTML = table4;*/


