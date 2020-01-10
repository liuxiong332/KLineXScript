const yaml = require('gulp-yaml');
const gulp = require('gulp');

function syntax() {
    return gulp.src('./syntaxes/*.yml')
        .pipe(yaml({ schema: 'DEFAULT_SAFE_SCHEMA', space: 2 }))
        .pipe(gulp.dest('./dist/'));
}

gulp.task("copy:ls", function () {
    return gulp.src('../target/debug/pine-ls.exe')
        .pipe(gulp.dest('./dist/'));
});

exports.default = function () {
    gulp.watch("./syntaxes/*.yml", syntax);
}