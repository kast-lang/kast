function raise(id) {
    //the actual tabs
    const tabs = document.getElementById("tabs").children;
    for (tab of tabs) {
        tab.removeAttribute("selected");
    }
    tabs[id].setAttribute("selected", true);

    //the tab bar buttons
    const tabBar = document.getElementById("tab-bar").children;
    for (tab of tabBar) {
        tab.removeAttribute("selected");
    }
    tabBar[id].setAttribute("selected", true);
}
