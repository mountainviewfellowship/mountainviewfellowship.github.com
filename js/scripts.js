// This script enables obfuscated email links on the website.
(function () {
    var aol = "aol.com";
    var gm = "gmail.com";

    function email(name, service) {
        return "mailto:" + name + "@" + service;
    }

    function maintainerEmail() {
        return email("anton.tayanovskyy", gm);
    }

    function pastorEmail() {
        return email("wpencemvf", aol);
    }

    var links = document.getElementsByTagName("a");
    for (var i=0; i < links.length; i++) {
        var link = links[i];

        switch (link.getAttribute("class")) {
        case "pastor":
            link.setAttribute("href", pastorEmail());
            break;
        case "maintainer":
            link.setAttribute("href", maintainerEmail());
            break;
        }
    }
})();