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

function mcDrawMatrix(table_content_id, matrix_server_path, table_id) {
    var render = mcCreateXMLHTTP();
    render.onreadystatechange = function () {
        if (render.readyState == 4 && render.status == 200) {
            document.getElementById(table_id).innerHTML =
                render.responseText;
            document.getElementById(table_content_id).innerHTML = "";
        }
    }
    render.open("POST", matrix_server_path, true);
    render.send(document.getElementById(table_content_id).innerHTML);
}
