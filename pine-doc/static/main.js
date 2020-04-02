function replaceName(name) {
    return '<a href="#' + name + '" class="pine-reference-item" data-href="' + name + '">int</a>'
}

$(function () {
    for (var i = 0; i < VARIABLES.length; i += 1) {
        $('.variables-body').append(replaceName("var_" + VARIABLES[i]));
    }

    for (var i = 0; i < FUNCTIONS.length; i += 1) {
        $('.functions-body').append(replaceName("fun_" + FUNCTIONS[i]));
    }
});