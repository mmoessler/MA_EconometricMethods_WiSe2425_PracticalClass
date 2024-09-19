# MA Econometric Methods WiSe 24/25 Practical Class

This repository containst the material for the practical class part of the course Econometric Methods in Business and Economics (EMBE).

The material, i.e. the slides, videos and additional material, are designed in such a way that they can be used across all semesters, with the exception of a few minor adjustments.

These adjustments include:

* Adjust the link to the ILIAS cours in `./index.Rmd`
* Adjust the link to datasets upload to ILIAS for each exercise sheet, e.g., in `01_ExerciseSheet_No01_XX.Rmd`

Note on show/hide interpretation:

* For uploads use:
  * `Sol01[i].style.display = "none";`
  * `NoSol01[i].style.display = "block";`
* For discussion use:
  * `Sol01[i].style.display = "block";`
  * `NoSol01[i].style.display = "none";`

```javascript
// Hide all solutions (by default)
var i, Sol01;
Sol01 = document.getElementsByClassName("showSol01");
for (i = 0; i < Sol01.length; i++) {
  Sol01[i].style.display = "none"; // type "block" for show or "none" (default) for exclude
}
  
var i, NoSol01;
NoSol01 = document.getElementsByClassName("noSol01");
for (i = 0; i < NoSol01.length; i++) {
  NoSol01[i].style.display = "block"; // type "block" (default) for show or "none" for exclude
}
```
