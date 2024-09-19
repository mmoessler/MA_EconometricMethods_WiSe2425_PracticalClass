
//..................................................

// collect all tab content ids
let tabContentAll = document.getElementsByClassName('tab-content-l1-cl');
let tabContentIds = [];
for (let i = 0; i < tabContentAll.length; i++) {
  tabContentIds.push(tabContentAll[i].id);
}

// collect all figure ids
let figureAll = document.getElementsByClassName('figure-cl');
let figureIds = [];
for (let i = 0; i < figureAll.length; i++) {
  figureIds.push(figureAll[i].id);
}

// collect slider ids
let sliderAll = document.getElementsByClassName('slider-cl');
let sliderIds = [];
for (let i = 0; i < sliderAll.length; i++) {
  sliderIds.push(sliderAll[i].id);
}

// collect slider value ids
let sliderValueAll = document.getElementsByClassName('slider-value-cl');
let sliderValueIds = [];
for (let i = 0; i < sliderValueAll.length; i++) {
  sliderValueIds.push(sliderValueAll[i].id);
}

//..................................................

// initialize sliders for all sliders
for (let i = 0; i < sliderIds.length; i++) {
  eval(
    'var slider' + (i + 1) + ' = new Slider("#slider-' + (i + 1) + '-id", { \n tooltip: "never", \n formatter: function(value) { \n return sliders.value.slider' + (i + 1) + '[value] \n } \n })'
  );
}

// set slide events for all sliders
for (let i = 0; i < sliderIds.length; i++) {
  eval(
    'slider' + (i + 1) + '.on("slide", function() { updateFiguresAndSliderValues() })'
    );
}

// set initial slider values
for (let i = 0; i < sliderIds.length; i++) {
  eval(
    'slider' + (i + 1) + '.setValue(sliders.initialIndex[' + i + '])'
  );
}

//..................................................

updateFiguresAndSliderValues = function() {
  
  let figurePathTmp = null;
  let sliderTmp = null;
  let sliderValue1Tmp = null;
  
  // loop over figures
  for (let i = 0; i < figureIds.length; i++) {
    figurePathTmp = './figures/figure_0' + (i + 1);
    // loop over sliders
    for (let j = 0; j < sliderIds.length; j++) {
      sliderTmp = 'slider' + (j + 1);
      sliderValue1Tmp = eval('slider' + (j + 1) + '.getValue()');
      figurePathTmp = figurePathTmp.concat('_' + (sliderValue1Tmp + 1));
      // set the slider value
      document.getElementById(sliderValueIds[j]).innerHTML = "\\(" + sliders.sliderValueParStr[j] + " = " + sliders.value[sliderTmp][sliderValue1Tmp] + "\\)";
      MathJax.typeset([document.getElementById(sliderValueIds[j])]);

      // set the math value for tab 1 (here for 1 slider only)
      let equTab1ValTmp = sliders.value.slider1[slider1.getValue()];
      let equTab1Val = Math.round(((1 - jStat.normal.cdf(equTab1ValTmp, 0, 1)) + Number.EPSILON) * 1000) / 1000;
      let equTabN1Content = `
      $$
      \\begin{align}
      p\\text{-value} &= 1 - \\phi\\left(t^{act}\\right) \\\\
      &= 1 - \\phi\\left(${equTab1ValTmp}\\right) \\\\
      &= ${equTab1Val}
      \\end{align}
      $$
      `;
      document.getElementById("tab-n1-id").innerHTML = equTabN1Content;
      MathJax.typeset([document.getElementById("tab-n1-id")]);

      // set the math value for tab 2 (here for 1 slider only)
      let equTab2ValTmp = sliders.value.slider1[slider1.getValue()];
      let equTab2Val = Math.round(((2 * jStat.normal.cdf(-Math.abs(equTab2ValTmp), 0, 1)) + Number.EPSILON) * 1000) / 1000;
      let equTabN2Content = `
      $$
      \\begin{align}
      p\\text{-value} &= 2 * \\phi\\left(-|t^{act}|\\right) \\\\
                     &= 2 * \\phi\\left(-|${equTab2ValTmp}|\\right) \\\\
                     &= ${equTab2Val} 
      \\end{align}
      $$
      `;
      document.getElementById("tab-n2-id").innerHTML = equTabN2Content;
      MathJax.typeset([document.getElementById("tab-n2-id")]);

      // set the math value for tab 3 (here for 1 slider only)
      let equTab3ValTmp = sliders.value.slider1[slider1.getValue()];
      let equTab3Val = Math.round((jStat.normal.cdf(equTab3ValTmp, 0, 1) + Number.EPSILON) * 1000) / 1000;
      let equTabN3Content = `
      $$
      \\begin{align}
      p\\text{-value} &= \\phi\\left(t^{act}\\right) \\\\
      &= \\phi\\left(${equTab3ValTmp}\\right) \\\\
      &= ${equTab3Val}
      \\end{align}
      $$
      `;
      document.getElementById("tab-n3-id").innerHTML = equTabN3Content;
      MathJax.typeset([document.getElementById("tab-n3-id")]);
      
    }

    console.log(figurePathTmp);

    document.getElementById(figureIds[i]).setAttribute("src", figurePathTmp.concat('.svg'));
  }
}

updateFiguresAndSliderValues();

//..................................................

// explain figure
explainButtonClick = function() {
  
  let tabEqual = false;
  let tab = 0;
  while (tabEqual==false) {
    tabEqual = activeTabId == tabContentIds[tab];
    tab++;
  }
  
  let audioShowTextIdTmp = 'audio-show-text-figure-' + tab + '-id'; // construct show text id
  let audioTextIdTmp = 'audio-text-figure-' + tab + '-overall-id'; // construct audio text id
  
  let audioTextDiv = document.getElementById(audioTextIdTmp); // get show text
  let audioTextSpa = audioTextDiv.getElementsByTagName("span");
  let audioShowPar = document.getElementById(audioShowTextIdTmp); // get audio text
  
  audioShowPar.innerHTML = audioTextDiv.innerHTML;
  audioShowPar.style.display = "block";
  
  let stopLoop = false;
  let index = 0;
  
  // function to read out each <span> with a delay in between
  function readSpans() {      
    if (index < audioTextSpa.length) {
      let span = audioTextSpa[index];
      let spanText = span.textContent;
      let speech = new SpeechSynthesisUtterance(spanText);
      speechSynthesis.speak(speech);
      speech.onend = function() {
        setTimeout(function() {
          index++;
          readSpans();
        }, 1000);
      };                
    // finished with reading out spans
    } else {
      audioShowPar.style.display = "none";  
      let intervalLoop = setInterval(function() {
        stopLoop = true;      
        eval('slider' + slider + '.setValue(slider' + slider + '.options.value)');
        updateFiguresAndSliderValues();
        clearInterval(intervalLoop);      
      }, 1000);
    }
  }
  
  // start reading out the spans
  readSpans();
    
}

// animate slider
animateButtonClick = function(slider) {
  
  let tabEqual = false;
  let tab = 0;
  while (tabEqual==false) {
    tabEqual = activeTabId == tabContentIds[tab];
    tab++;
  }
    
  let audioShowTextIdTmp = 'audio-show-text-figure-' + tab + '-id'; // construct show text id
  let audioTextIdTmp = 'audio-text-figure-' + tab + '-slider-' + slider + '-id'; // construct audio text id
  
  let audioTextDiv = document.getElementById(audioTextIdTmp); // get show text
  let audioTextSpa = audioTextDiv.getElementsByTagName("span");
  let audioShowPar = document.getElementById(audioShowTextIdTmp); // get audio text
  
  audioShowPar.innerHTML = audioTextDiv.innerHTML;
  audioShowPar.style.display = "block";
  
  let stopLoop = false;
  let index = 0;
  
  // function to read out each <span> with a delay in between
  function readSpans() {      
    if (index < audioTextSpa.length) {
      let span = audioTextSpa[index];
      let spanText = span.textContent;
      let speech = new SpeechSynthesisUtterance(spanText);
      speechSynthesis.speak(speech);
      speech.onend = function() {
        setTimeout(function() {
          index++;
          readSpans();
        }, 1000);
      };                
      // finished with reading out spans
    } else {
      audioShowPar.style.display = "none";  
      let intervalLoop = setInterval(function() {
        stopLoop = true;      
        eval('slider' + slider + '.setValue(slider' + slider + '.options.value)');
        updateFiguresAndSliderValues();
        clearInterval(intervalLoop);      
      }, 1000);
    }
  }
  
  // start reading out the spans
  readSpans();
  
  // iteration over sliders (sider specific)
  let direction = 1; // 1 for increment, -1 for decrement
  let minValue = eval('slider' + slider + '.options.min');
  let maxValue = eval('slider' + slider + '.options.max');
  
  let loopInterval = setInterval(function() {
    // Get the current value of the slider
    let value = eval('slider' + slider + '.getValue()');
    
    // Check if the event to stop the loop has been triggered
    if (stopLoop) {
      eval('slider' + slider + '.setValue(slider' + slider + '.options.value)');
      updateFiguresAndSliderValues();
      clearInterval(loopInterval);      
    } else {
      // Check if the slider value reaches the minimum or maximum
      if (value === maxValue || value === minValue) {
        // Change the direction to move the slider back and forth
        direction *= -1;
      }
      // Increment or decrement the slider value based on the direction
      eval('slider' + slider + '.setValue(value + direction)');
      
      //sliderLoop(slider0.sliderValueParStrgetValue(), slider1.getValue());
      updateFiguresAndSliderValues();
      
    }
    
  }, 1000); // Adjust the interval as needed (e.g., 100ms for a fast loop)
  
}
