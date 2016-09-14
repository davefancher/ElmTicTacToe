"use strict";

var gulp = require("gulp"),
    elm = require("gulp-elm");

var bootstrapDist = "bower_components/bootstrap/dist"
var webroot = "src";

gulp.task("copy:bootstrapCss", function() {
    return gulp.src([ bootstrapDist + "/css/bootstrap.min.css" ])
        .pipe(gulp.dest(webroot + "/styles"));
});

gulp.task("copy:bootstrapJs", function() {
    return gulp.src([ bootstrapDist + "/js/bootstrap.min.js" ])
        .pipe(gulp.dest(webroot + "/scripts"));
});

gulp.task("copy:glyphicons", function() {
    return gulp.src([ bootstrapDist + "/fonts/*.*" ])
        .pipe(gulp.dest(webroot + "/fonts"));
})

gulp.task("copy:bootstrap", [ "copy:bootstrapCss", "copy:bootstrapJs", "copy:glyphicons" ]);

gulp.task("compile:ticTacToe", function() {
    return gulp.src([ webroot + "/scripts/tictactoe.elm" ])
        .pipe(elm({ "warn": false }))
        .pipe(gulp.dest(webroot + "/scripts"))
});

gulp.task("default", [ "compile:ticTacToe", "copy:bootstrap" ]);