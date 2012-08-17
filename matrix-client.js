function mcCreateXMLHTTP() {
    var xmlhttp;
    if (window.XMLHttpRequest) {
        // code for IE7+, Firefox, Chrome, Opera, Safari
        xmlhttp = new XMLHttpRequest();
    } else {
        // code for IE6, IE5
        xmlhttp = new ActiveXObject("Microsoft.XMLHTTP");
    }
    return xmlhttp;
}

function mcDrawMatrix(table_data_path, matrix_server_path, element_id) {
    var table = mcCreateXMLHTTP();
    table.onreadystatechange = function() {
        if (table.readyState == 4 && table.status == 200) {
            var render = mcCreateXMLHTTP();
            render.onreadystatechange = function () {
                if (render.readyState == 4 && render.status == 200) {
                    document.getElementById(element_id).innerHTML =
                        render.responseText;
                }
            }
            render.open("POST", matrix_server_path, true);
            render.send(table.responseText);
        }
    }
    table.open("GET", table_data_path, true);
    table.send();
}
