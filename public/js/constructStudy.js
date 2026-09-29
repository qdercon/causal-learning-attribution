// import task info from versionInfo file
import { allowDevices, intCond, taskQuestions, timepoint, redirect } from "./versionInfo.js"; 
 
// import study elements
import { timeline_causal_training } from "./learning-task-min.js";
import { timeline_learn_training } from "./control-task-min.js";
import { timeline_quests_prescreen, timeline_quests_final } from "./self-reports-screen.js";
import { saveEndData } from "./saveData.js";

//////////////////////////////////////initialise jsPsych///////////////////////////////////
var jsPsych = initJsPsych({
    show_progress_bar: false,
    on_finish: function() {
        window.onbeforeunload = null; // allow reloading the page after the task is finished
        saveEndData()
            .then(() => {
                // redirect to the end page
                window.location.href = redirect;
            })
            .catch((error) => {
                // Handle any errors that might occur during the save operation
                console.error("Error saving end data:", error);
                // You might still want to redirect or inform the user
                window.location.href = redirect; // Or some other fallback
            });
    }
});
// export jsPsych object so can be accessed by other study modules
export { jsPsych };

////////////////////////////construct overall study timeline///////////////////////////////
export function runStudy(){
    // initialise overall study timeline
    var timeline = [];
    if (taskQuestions == true) {
        if (timepoint == 1) {
            timeline = timeline.concat(timeline_quests_prescreen);
        } else if (timepoint == 2) {
            timeline = timeline.concat(timeline_quests_final);
        }
    } else {
        if (intCond == "causal") {
            // 1. causal training
            timeline = timeline.concat(timeline_causal_training);
        }
        else if (intCond == "control") {
            // 2. control training
            timeline = timeline.concat(timeline_learn_training);
        }
    }
    jsPsych.run(timeline);
}; 

///////////////////////////////////// misc functions //////////////////////////////////////
// allow access on mobile devices?
if (allowDevices == false) {
    if (/Android|webOS|iPhone|iPad|iPod|BlackBerry|IEMobile|Opera Mini/i.test(navigator.userAgent)) {
       alert("Sorry, this study does not work on mobile devices!");
    }
};

