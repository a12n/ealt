var commentary
var output
var ws

function appendOutput(text)
{
    var p = document.createElement('p')
    p.style.wordWrap = 'break-word'
    p.innerHTML = text
    output.appendChild(p)
}

function onClose(evt)
{
    appendOutput('Disconnected')
}

function onError(evt)
{
    appendOutput('<span style="color: red;">' + evt.data + '</span>')
    ws.close()
}

function onMessage(evt)
{
    var obj = JSON.parse(evt.data)
    if (obj.type == 'commentary') {
        commentary += obj.text
        if (obj.flush) {
            var ts = new Date(obj.timestamp * 1000)
            appendOutput('<b>' + ts.toLocaleString() + '</b><br>' + commentary)
            commentary = ''
        }
    }
}

function onOpen(evt)
{
    appendOutput('Connected')
}

function onLoad()
{
    commentary = ''

    output = document.getElementById('output')

    ws = new WebSocket(prompt('WebSocket URL', 'ws://localhost:8642/events'))
    ws.onclose = function(evt) { onClose(evt) }
    ws.onerror = function(evt) { onError(evt) }
    ws.onmessage = function(evt) { onMessage(evt) }
    ws.onopen = function(evt) { onOpen(evt) }
}

window.addEventListener('load', onLoad, false)
