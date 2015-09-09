function getParameterByName(name) {
    name = name.replace(/[\[]/, "\\\[").replace(/[\]]/, "\\\]");
    var regex = new RegExp("[\\?&]" + name + "=([^&#]*)"),
        results = regex.exec(location.search);
    return results == null ? "" : decodeURIComponent(results[1].replace(/\+/g, " "));
}

test("Cross domain get request", function () {
    var testUrl = getParameterByName("url");
    console.log(testUrl);
    var xhr = new XMLHttpRequest();
    xhr.open("GET", testUrl + "/hello", false);
    xhr.send();

    equal("CORS request accepted.", xhr.responseText);

});