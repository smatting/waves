'use strict';

// import css from 'main.css';


// require('ace-css/css/ace.css');
// require('font-awesome/css/font-awesome.css');

require('purecss/build/pure-min.css');
// Require index.html so it gets copied to dist
require('./main.css');
require('./index.html');


var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');


// The third value on embed are the initial values for incomming ports into Elm
var app = Elm.Main.embed(mountNode);


window.saveSVG = function() {
    // var svgData = $("#wave-svg")[0].outerHTML;
    var svgData = document.querySelector("#wave-svg");

    //get svg source.
    var serializer = new XMLSerializer();
    var source = serializer.serializeToString(svgData);

    //add name spaces.
    if(!source.match(/^<svg[^>]+xmlns="http\:\/\/www\.w3\.org\/2000\/svg"/)){
        source = source.replace(/^<svg/, '<svg xmlns="http://www.w3.org/2000/svg"');
    }
    if(!source.match(/^<svg[^>]+"http\:\/\/www\.w3\.org\/1999\/xlink"/)){
        source = source.replace(/^<svg/, '<svg xmlns:xlink="http://www.w3.org/1999/xlink"');
    }

    var svgBlob = new Blob([source], {type:"image/svg+xml;charset=utf-8"});
    var svgUrl = URL.createObjectURL(svgBlob);
    var downloadLink = document.createElement("a");
    downloadLink.href = svgUrl;
    var ts = (new Date()).getTime();
    downloadLink.download = "wave-" + ts + ".svg";
    document.body.appendChild(downloadLink);
    downloadLink.click();
    document.body.removeChild(downloadLink);
}

