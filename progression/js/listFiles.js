function listFiles() {
var oFrame = document.getElementById("frmFile");
var strRawContents = oFrame.contentWindow.document.body.childNodes[0].innerHTML;
while (strRawContents.indexOf("\r") >= 0)
    strRawContents = strRawContents.replace("\r", "");
var arrLines = strRawContents.split("\n");
var curLine = [];

for (var i = 0; i < arrLines.length; i++) {
    curLine.push(arrLines[i]);
}

var select = document.getElementById("selectDept");
for(var i = 0; i < curLine.length; i++) {
    var opt = curLine[i];
    var el = document.createElement("option");
    el.textContent = opt;
    el.value = "data/"+opt;
    select.appendChild(el);
    console.log(el.value);
}

}
