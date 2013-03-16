var airTemperature
var atmosphericPressure
var relativeHumidity
var trackTemperature
var wetTrack
var windDirection
var windSpeed

var ws

function onMessage(evt)
{
    var obj = JSON.parse(evt.data)
    switch (obj.type) {
    case 'air_temperature' :
        airTemperature.innerHTML = obj.temp
        break
    case 'atmospheric_pressure' :
        atmosphericPressure.innerHTML = obj.pressure
        break
    case 'relative_humidity' :
        relativeHumidity.innerHTML = obj.humidity
        break
    case 'track_temperature' :
        trackTemperature.innerHTML = obj.temp
        break
    case 'wet_track' :
        if (obj.wet) {
            wetTrack.innerHTML = 'Yes'
        } else {
            wetTrack.innerHTML = 'No'
        }
        break
    case 'wind_direction' :
        windDirection.innerHTML = obj.direction
        break
    case 'wind_speed' :
        windSpeed.innerHTML = obj.speed
        break
    }
}

function onLoad()
{
    airTemperature = document.getElementById('airTemperature')
    atmosphericPressure = document.getElementById('atmosphericPressure')
    relativeHumidity = document.getElementById('relativeHumidity')
    trackTemperature = document.getElementById('trackTemperature')
    wetTrack = document.getElementById('wetTrack')
    windDirection = document.getElementById('windDirection')
    windSpeed = document.getElementById('windSpeed')

    ws = new WebSocket(prompt('WebSocket URL', 'ws://localhost:8642/events'))
    ws.onclose = function(evt) { onClose(evt) }
    ws.onerror = function(evt) { onError(evt) }
    ws.onmessage = function(evt) { onMessage(evt) }
    ws.onopen = function(evt) { onOpen(evt) }
}

window.addEventListener('load', onLoad, false)
