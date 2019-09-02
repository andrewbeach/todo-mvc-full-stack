// module

exports.mkApplication = function() {
  var express = require('express');
  return express();
};

exports._httpServer = function(app) {
  return function() {
    var http = require('http');
    var server = http.createServer(app);
    return server;
  };
};

// :: Application -> Int -> (Event -> Effect Unit) -> Effect Server
exports._listenHttp = function(app) {
  return function(port) {
    return function(callback) {
      return function() {
        var http = require('http');
        var server = http.createServer(app);
        server.listen(port, function(e) {
          return cb(e)();
        });
        return server;
      };
    };
  };
};

exports._http = function(app, method, route, handler) {
  return function() {
    app[method](route, function(req, resp) {
      return handler(req)(resp)();
    });
  };
};

exports._get = function(app, route, handler) {
  return function() {
    app.get(route, function(req, resp) {
      return handler(req)(resp)();
    });
  };
};
