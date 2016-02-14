var gulp = require("gulp");
var ts = require("gulp-typescript");
var babel = require("gulp-babel");
var rename = require("gulp-rename");

// Using my existing tsconfig.json file
var tsProject = ts.createProject(__dirname + "/tsconfig.json");

gulp.task("ts-babel", function () {

  var compiledTs = tsProject.src()
        .pipe(ts(tsProject))
        .js;

  return compiledTs
    .pipe(babel())
    .pipe(rename(function (path) {
      path.extname = ".js";
    }))
    .pipe(gulp.dest("./_build/"));
});

gulp.task("watch", ["ts-babel"], function() {
  gulp.watch(["lib.ts"], ["ts-babel"]);
});

