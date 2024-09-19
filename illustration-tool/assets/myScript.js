
//..............................
// handling of collapsibles
// see: https://www.w3schools.com/howto/howto_js_collapsible.asp
let coll = document.getElementsByClassName("coll-button-cl");
let i;

for (i = 0; i < coll.length; i++) {
  coll[i].addEventListener("click", function() {
    this.classList.toggle("active");
    var content = this.nextElementSibling;
    if (content.style.display === "block") {
      content.style.display = "none";
    } else {
      content.style.display = "block";
    }
  });
}

//..............................
// handling of tabs      
// see: https://www.w3schools.com/howto/howto_js_tabs.asp

let activeTabId; // define as global variable
activeTabId = "tab-link-l1-n1-id";

// open specific tab
function openSpecificTab(tabContentId, tabLinkId, color) {

  // hide all elements with class="tab-content-l1" by default */
  let i, tabcontent, tablinks;

  tabcontent = document.getElementsByClassName("tab-content-l1-cl");
  for (let i = 0; i < tabcontent.length; i++) {
    tabcontent[i].style.display = "none";
  }

  // remove the background color of all tablinks/buttons
  tablinks = document.getElementsByClassName("tab-link-l1-cl");
  for (let i = 0; i < tablinks.length; i++) {
    tablinks[i].style.backgroundColor = "";
  }

  // show the specific tab content
  document.getElementById(tabContentId).style.display = "block";
  // store the ID of the currently active tab
  activeTabId = tabContentId;

  // add the specific color to the button used to open the tab content
  document.getElementById(tabLinkId).style.backgroundColor = color;

}

// set start page
document.getElementById("tab-link-l1-n1-id").click();
// initialize figure and slider
updateFiguresAndSliderValues();
