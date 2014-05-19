var express = require('express');
var path = require('path');
var favicon = require('static-favicon');
var logger = require('morgan');
var cookieParser = require('cookie-parser');
var bodyParser = require('body-parser');

var routes = require('./routes/index');
var users = require('./routes/users');

var res=[];
var app = express();
var http = require('http');
var options = {
  host: 'localhost',
  path: '/',
  port: '8090'
};
function File(name, dir) {
  this.fileName = name;
  this.dir = dir;
}
global.records = [];
global.remoteLink='http://localhost:8090/';
callback = function(response) {
  var str = '';

  //another chunk of data has been recieved, so append it to `str`
  response.on('data', function (chunk) {
    str += chunk;
  });

  //the whole response has been recieved, so we just print it out here
  response.on('end', function () {
    //console.log(str);
    res = str.split(",");
    
    for (var i in res) {
	var first = res[i].indexOf("\"");
	var last = res[i].lastIndexOf("\"");
	res[i]=res[i].substring(first+1,last);
	var fileName=res[i].substring(res[i].lastIndexOf("/")+1,res[i].length);
	records.push(new File(fileName, res[i]));
    }
console.log(res);

  });
}

http.request(options, callback).end();



// view engine setup
app.set('views', path.join(__dirname, 'views'));
app.set('view engine', 'jade');

app.use(favicon());
app.use(logger('dev'));
app.use(bodyParser.json());
app.use(bodyParser.urlencoded());
app.use(cookieParser());
app.use(express.static(path.join(__dirname, 'public')));

app.use('/', routes);
app.use('/users', users);

/// catch 404 and forward to error handler
app.use(function(req, res, next) {
    var err = new Error('Not Found');
    err.status = 404;
    next(err);
});

/// error handlers

// development error handler
// will print stacktrace
if (app.get('env') === 'development') {
    app.use(function(err, req, res, next) {
        res.status(err.status || 500);
        res.render('error', {
            message: err.message,
            error: err
        });
    });
}

// production error handler
// no stacktraces leaked to user
app.use(function(err, req, res, next) {
    res.status(err.status || 500);
    res.render('error', {
        message: err.message,
        error: {}
    });
});

module.exports = app;
