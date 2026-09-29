// import task info from versionInfo file
import { nBlocksLearning, nScenarios, debugging, session } from "./versionInfo.js"; 
import { controlSessions } from "./trialsControl.js"; 
 
// import our data saving function
import { saveStartData, saveTaskData, saveQuestData } from "./saveData.js";

// import jspsych object so can access modules
import { jsPsych } from "./constructStudy.js";

// initialize task vars
var nTrialsLearning;
var timeBeforeChoice = 750;         // time in ms before choice can be entered
var trialTimeoutTime;               // time in ms after which trial times out
var feedbackTime = 2500;            // time in ms feedback is displayed on screen
var nTimeouts = 0;
var blockNo = 0;
// var trialEvents;
var trialValence;
var trialAttrIntGlob;
var trialAttrIntSpec;
var trialAttrExtGlob;
var trialAttrExtSpec;
var scenarioColours = ["#D5D6EA", "#D7ECD9", "#F6F6EB"];
var allBlocks = [
    [0, 1, 2], [2, 1, 0], [1, 2, 0], [1, 0, 2], [0, 2, 1], [1, 2, 0]
]
var blockOrder = allBlocks[session-1]; // get block order for this session
var sessColours = blockOrder.map(index => scenarioColours[index]); // get colours for this session

// grab trial info for current session
const trialsC = controlSessions[session-1];
trialValence = trialsC.event_valence;
trialAttrIntGlob = trialsC.int_glob;
trialAttrIntSpec = trialsC.int_spec;
trialAttrExtGlob = trialsC.ext_glob;
trialAttrExtSpec = trialsC.ext_spec;

// set number of trials, blocklength, and max trial length according to debug condition
if ( debugging == false ) {
    nTrialsLearning = trialValence.length;
    trialTimeoutTime = 15000;
} else {
    nTrialsLearning = 12;
    trialTimeoutTime = 4000;
}
var blockLengthLearning = Math.round(nTrialsLearning/nBlocksLearning); // default 3 learning blocks

///////////////////////////////////////////// LEARNING TASK TIMELINE /////////////////////////////////////////////////////////
var timeline_learn_training = [];

// preload images for this session
const imgPath = "./assets/img/tlc/"
var reqImgs = [];

// for each of the four arrays in trialsC (int_glob, int_spec, ext_glob, ext_spec)
// extract the strings containing .png (i.e., image names excluding "dummy"),
// prepend the pathname, and add to the reqImgs array for preloading
for (const [key, value] of Object.entries(trialsC)) {
    if (key !== "event_valence") {
        for (const img of value) {
            reqImgs.push(imgPath + img);
        }
    }
}

var preload = {
    type: jsPsychPreload,
    images: reqImgs
};
timeline_learn_training.push(preload);

// define instructions
var instructions_learn_training = {
    type: jsPsychInstructions,
    allow_backward: true,
    show_clickable_nav: true,
    allow_keys: false,
    button_label_previous: "back",
    button_label_next: "next",
    pages: [
        /////////////////////page one////////////////////////
        "<div style='font-size:1.25rem; line-height:2rem'>"+
        "<p>"+
        "<h1>Welcome!</h1>"+
        "</p>"+
        "<br>"+
        "<p>In the following pages, we will walk you through how to complete this task.</p>"+
        "<p>If you have already completed this training before, feel free to skip through the instructions.</p>"+
        "<br><br>"+
        "</p>"+
        "</div>",
        /////////////////////page two////////////////////////
        "<p>"+
        "<h2>What do I need to do?</h2>"+
        "<br>"+
        "</p>"+
        "<p>"+
        "Some researchers believe that <b>the kinds of reasons we think are likely to be responsible "+
        "for events can differ, depending on our moods</b>."+
        "</p>"+
        "<p>"+
        "In this part of the study, we will ask you to learn how to <b>sort various everyday objects into different 'baskets'</b>. "+
        "We will ask you to learn how to sort the object across <b>"+nScenarios+" different scenarios</b>. "+
        "</p>"+
        "<br>"+
        "<div class='center-content'><img src='assets/img/3_scenarios.png' style='width:600px;'></img></div>"+
        "<p>"+
        "The kinds of objects that you will encounter will all be different from each other, but "+
        "<b>the same kinds of objects will belong in each basket <i>within</i> each scenario</b>. "+
        "</p>"+
        "<p>"+
        "For each scenario, it is therefore your job to try and work out <i>what kinds of objects "+
        "belong in each basket</i>."+
        "</p>",
        /////////////////////page three////////////////////////
        "<p>"+
        "<h2>What do I need to do?</h2>"+
        "<br>"+
        "</p>"+
        "<p>"+
        "Specifically, you will <b>see a series of different coloured and shaped baskets</b>. "+
        "</p>"+
        "<p>"+
        "Below each basket, are <b>two different potential objects</b> that could belong to them. "+
        "</p>"+
        "<br>"+
        "<div class='center-content'><img src='assets/img/eg3c.png' style='width:450px;'></img></div>"+
        "<br>"+
        "<p>"+
        "For each basket, you must <b>choose which you think the most likely object is</b>, by clicking "+
        "on it."+
        "<br><br>"+
        "</p>",
        /////////////////////page four////////////////////////
        "<p>"+
        "<h2>What do I need to do?</h2>"+
        "<br>"+
        "</p>"+
        "<p>"+
        "You will then <b>discover if that object was correct or not, for that basket in <i> that scenario</i></b>. "+
        "</p>"+
        "<p>If you chose the correct object, it will turn <span style='color:green;'>green</span>, and you will see a tick symbol with the word CORRECT.</p>"+
        "<p>If you chose the incorrect object, it will turn <span style='color:red;'>red</span>, and you will see a cross symbol with the word INCORRECT. "+
            "Your answer will then be <span style='color:grey;'>greyed out</span>, and the correct answer will be highlighted in <span style='color:green;'>green</span>.</p>"+
        "<div class='center-content'><img src='assets/img/corr_incorr.png' style='width:450px;'></img></div>"+
        "<p>"+
        "As we would like people to stay focused on learning during each scenario, there is a <b>time limit</b> "+
        "to choose your answer each time. If no choice is made within the time limit (<b>15 seconds</b>), then the screen "+
        "will show the message 'You didn't choose in time!'. You will then be asked to choose between those objects again. "+
        "</p>"+
        "<p>"+
        "We ask that people try and not have too many timed-out choices. Submissions with a very high rate of time-out choices "+
        "may not be approved. <b>Each scenario should take around 2 minutes</b> to run through ("+nScenarios+" scenarios in total) "+
        " - and you can take a short break between scenarios if you like."+
        "<br><br>"+
        "</p>",
        /////////////////////page five////////////////////////
        "<p>"+
        "<h2>What do I need to do?</h2>"+
        "<br>"+
        "</p>"+
        "<p>"+
        "To recap, <b>for each scenario, there is a certain kind of object that belongs in each basket</b>. This will stay the same "+
        "all the way through each scenario, but may change <i>between</i> scenarios."+
        "</p>"+
        "<p>"+
        "Your job during this part of the study is therefore to learn, through trial and error, for each scenario <b>what you think the "+
        "right kind of objects are for each basket</b>. "+
        "</p>"+
        "</p>"+
        "<div class='center-content'><img src='assets/img/head_why.png' style='width:150px;'></img></div>",
        // "<p>"+
        // "In order to motivate you to learn the right objects, all approved submissions will <b>earn a bonus</b> payment, "+
        // "the size of which depends on <b> how many answers you get right</b>. Specifically, you will earn an extra "+bonusRate+" pence "+
        // "for every correct object you choose (<b>max possible bonus £"+maxBonus.toFixed(2)+"</b>)."+
        // "</p>"+
        // "<br><br><br>"+
        // "</p>",
        // /////////////////////page six////////////////////////
        "<p>"+
        "<h2>Approval rules for this part of the study</h2>"+
        "<br>"+
        "</p>"+
        "<p>"+
        "As ensuring that our data is as high quality as possible forms part of our responsibility to the "+
        "bodies that fund our research, we will also be <b>applying two quality control rules</b> to the "+
        "data we receive for this part of the study. "+
        "</p>"+
        "<ul>"+
        "<p><b>1. Choice times</b>. Submissions with choice times of <i>less than 1 second "+
        "for a majority of trials</i> will not be approved, as we believe it is not possible to properly process "+
        "the required information in this time.</p>"+
        "<p><b>2. Time-outs</b>. Submissions with a <i>high number of time-outs (10% or more of choices)</i> "+
        "may also not be approved, as it's important for the study results that people try and stay "+
        "focused on learning during each scenario.</p>"+
        "</ul>"+
        "<p>"+
        "We hope that the above measures are reasonable and clearly explained. If you don't think this is the case, "+
        "please get in touch and let us know."+
        "</p>"+
        "<div class='center-content'><img src='assets/img/thank-you.png' style='width:150px;'></img></div>"+
        "<p>"+
        "Above all, <b>we are very grateful to our study participants for volunteering their time to help us "+
        "with our research</b>. Having quality control checks like the above on our data means that we can "+
        "be more confident in the conclusions we can draw from online studies, and be more likely "+
        "to be able to conduct these kind of studies in the future."+
        "<br>"+
        "</p>",
        /////////////////////page seven////////////////////////
        "<br>"+
        "<p>"+
        "Before you continue to the training, we will ask you to <b>answer some quick questions</b>. "+
        "This is in order to make sure we have explained the new information clearly enough."+
        "</p>"+
        "<div class='center-content'><img src='assets/img/quiz.png' style='width:200px;'></img></div>"+
        "<p>"+
        "<b>If you don't get all the questions right, you will be routed back to the start of these instructions "+
        "to try again</b>."+
        "<br><br>"+
        "</p>"
    ],
    on_start: function() {
        //this.type.jsPsych.setProgressBar(0);
    },
    on_finish: function() {
        var startTime = performance.now(); // this.type.jsPsych.getStartTime();
        saveStartData(startTime);
    }
};

var quizQuestions = {
    showQuestionNumbers: "off",
    pages: [
        {
            name: "quiz",
            elements: [
                {
                    type: "radiogroup",
                    name: "Q0", // used in on_finish logic
                    title: "1. The point of this part of the the study is to...",
                    isRequired: true,
                    choices: [
                        "A. Select the object you think is most likely to belong in each basket, according to whichever object you prefer", 
                        "B. Select the object you think is most likely to belong in each basket, according to which object you think would be the most popular of the two", 
                        "C. Select the object you think is most likely to belong in each basket, thinking about the kinds of objects that have belonged in each basket so far in each scenario"
                    ]
                },
                {
                    type: "radiogroup",
                    name: "Q1",
                    title: "2. This part of the study will ask me to choose between objects across "+nScenarios+" different scenarios. The best way to think about these different scenarios is...",
                    isRequired: true,
                    choices: [
                        "A. Objects within each scenario can belong to any of the baskets, which it is impossible to learn",
                        "B. The same kinds of objects will belong to each basket within each scenario, but when the scenario changes, the kind of objects that belong may also change",
                        "C. The same kinds of objects will belong to both baskets all the way through the study"
                    ]
                },
                {
                    type: "radiogroup",
                    name: "Q2",
                    title: "3. After I select which object I think belongs in the basket, I will find out whether my choice was correct or incorrect.",
                    isRequired: true,
                    choices: [
                        "A. To help me learn, the incorrect object will disappear from the screen.",
                        "B. To help me learn, the incorrect object will be highlighted in green.",
                        "C. To help me learn, the correct object will be highlighted in green and the incorrect object will be greyed out."
                    ]
                },
                {
                    type: "radiogroup",
                    name: "Q3",
                    title: "4. I understand that some quality-control rules will be applied to my submission.",
                    isRequired: true,
                    choices: [
                        "A. Submissions with a high number of timed-out choices from this part of the study will definitely be approved.",
                        "B. Submissions with a high number of timed-out choices from this part of the study may not be approved.",
                        "C. Submissions with a high number of correct choices from this part of the study may not be approved."
                    ]
                }
            ]
        }
    ]
};

var nCorrect = 0;
var nQuests = 4;
var introQuiz = {
    type: jsPsychSurvey,
    survey_json: quizQuestions,
    data: {
        correct_answers: ["C", "B", "C", "B"]
    },
    randomize_question_order: false,
    button_label: "check answers", 
    on_finish: function (data) {
        // compare answers to correct answers
        nCorrect = 0;
        for (var i=0; i < nQuests; i++) {
            var questID = "Q"+i;
            if (data.response[questID][0] == data.correct_answers[i]) {
                nCorrect++;
            }
        }
        data.nCorrect = nCorrect;
    }
};

var loop_node = {
    timeline: [ instructions_learn_training, introQuiz ],
    loop_function: function(data) {
        if ( nCorrect >= nQuests ) {
            return false;
        } else {
            return true;
        }
    }
};

var continueText= {
    type: jsPsychInstructions,
    allow_backward: false,
    show_clickable_nav: true,
    allow_keys: true,
    //button_label_previous: "back",
    button_label_next: "continue",
    pages: [
        "<p><h2>Thank you! You got all the questions correct!</h2></p>"+
        "<p>"+
        "Just to remind you one more time, what we would like you to do for this "+
        "part of the study is to <b>select the object you think "+
        "is most likely to belong</b> in each basket, thinking about <b>the kinds of objects "+
        "that have been correct for that basket so far during this particular scenario</b>."+
        "</p>"+
        "<p>"+
        "Please try and choose your answer as accurately as possible. <i>If you try and "+
        "click on an answer too quickly after the description has been displayed, it may not register yet</i>. "+
        "</p>"+
        "<p>"+
        "<b>Each scenario should take around 2 minutes</b> to run through, and we will task you about "+nScenarios+" different scenarios in total. "+
        "If you like, you can take a short break between each of the scenarios."+
        "</p>"+
        // "<p>"+
        // "<b>The progress bar at the top of the screen shows you how far you are through this part of the study</b>."+
        // "</p>"
        "<p>"+
        "Please press the <b>continue</b> button when you are ready to start!"+
        "</p>"
    ]
};

// define intro text screen
var task_intro = {
    type: jsPsychHtmlButtonResponseCA,
    choices: ['start'],
    is_html: true,
    stimulus: function () {
        var stim_br = (
            "<br>"+
            "<p>"+
            "<b>You are now ready to start learning about the first scenario</b>."+
            "</p>"+
            "<p>"+
            "Remember, in each scenario <i>different "+
            "kinds of objects may belong in different baskets.</i>"+
            "</p>"+
            "<p>"+
            "Press the button below when you are ready to start!</b>. "+
            "</p>"+
            "<br>"+
            "</p>"
        )
        return stim_br;
    },
    on_start: function () {
        document.body.style.background = sessColours[blockNo];
        blockNo++;
    }
};

// define trial stimuli and choice array for use as a timeline variable
var events_causes_learning = [];
for ( var j = 0; j < blockOrder.length; j++ ) {
    var blockType = blockOrder[j];
    for ( var i = 0; i < blockLengthLearning; i++ ) {
        var itemNo = i;
        var tr = i + j*blockLengthLearning;
        events_causes_learning[tr] = { 
                                trialIndex: i,
                                stimulus: null,
                                valence: trialValence[tr],
                                intGlob: trialAttrIntGlob[tr],
                                intSpec: trialAttrIntSpec[itemNo],
                                extGlob: trialAttrExtGlob[itemNo],
                                extSpec: trialAttrExtSpec[itemNo],
                                itemNo: itemNo,
                                blockNo: null,
                                choice1: null,
                                choice2: null,
                                correct_answer: null };
        if ( blockType === 0 ) {
            events_causes_learning[tr].choice1 = trialAttrIntGlob[tr];
            events_causes_learning[tr].choice2 = trialAttrIntSpec[itemNo];
            if ( trialValence[tr] == "negative") {
                events_causes_learning[tr].stimulus = "blue-basket.png";
                events_causes_learning[tr].correct_answer = trialAttrIntSpec[itemNo];
            } else {
                events_causes_learning[tr].stimulus = "red-basket.png";
                events_causes_learning[tr].correct_answer = trialAttrIntGlob[tr];
            }
        } else if ( blockType === 1 ) {
            events_causes_learning[tr].choice1 = trialAttrIntGlob[tr];
            events_causes_learning[tr].choice2 = trialAttrExtGlob[itemNo];
            if ( trialValence[tr] == "negative") {
                events_causes_learning[tr].stimulus = "blue-basket.png";
                events_causes_learning[tr].correct_answer = trialAttrExtGlob[itemNo];
            } else {
                events_causes_learning[tr].stimulus = "red-basket.png";
                events_causes_learning[tr].correct_answer = trialAttrIntGlob[tr];
            }
        } else if ( blockType === 2 ) {
            events_causes_learning[tr].choice1 = trialAttrIntGlob[tr];
            events_causes_learning[tr].choice2 = trialAttrExtSpec[itemNo];
            if ( trialValence[tr] == "negative") {
                events_causes_learning[tr].stimulus = "blue-basket.png";
                events_causes_learning[tr].correct_answer = trialAttrExtSpec[itemNo];
            } else {
                events_causes_learning[tr].stimulus = "red-basket.png";
                events_causes_learning[tr].correct_answer = trialAttrIntGlob[tr];
            }
        }
        events_causes_learning[tr].blockNo = j+1;
    }
};

// define individual choice trials
var learningTrialNo = 0;
var learning_choice_types = ['choice1', 'choice2'];
var learning_trial = {
    // jsPsych plugin to use
    type: jsPsychHtmlButtonResponseCA,
    // trial info
    prompt: null,  
    stimulus: function () {
        return `
            <div><img src='assets/img/tlc/${jsPsych.evaluateTimelineVariable('stimulus')}' style='height:25vh;'></img></div>
            <div><img src='assets/img/head_why.png' style='height:20vh;'></img></div>
        `;
    },
    choices: function () {
        var display_order = jsPsych.randomization.repeat(learning_choice_types, 1);
        return [
            jsPsych.evaluateTimelineVariable(display_order[0]), 
            jsPsych.evaluateTimelineVariable(display_order[1])
        ];
    },
    save_trial_parameters: {
        choices: true
    },
    // trial timing
    trial_duration: trialTimeoutTime,       // after this time, move on to next trial (but trial re-added)
    stimulus_duration: null,                // stim text remains on screen indefinitely
    time_before_choice: timeBeforeChoice,   // time in ms before the ppt can enter a choice
    response_ends_trial: true,              // trial ends only when response entered
    time_after_choice: 0,                 // time in ms to leave trial info on screen following choice
    post_trial_gap: 0,                     
    // styling
    margin_vertical: '0px',                 // vertical margin of the button (px)
    margin_horizontal: '20px',              // horizontal margin of the button (px)
    button_html: "<div class='thought'><img src='assets/img/tlc/%choice%' style='width:180px;'></img></div>",  // use images as response buttons
    // at end of each trial
    on_finish: function(data, trial) {
        // add chosen interpretation type to output
        data.stimulus = jsPsych.evaluateTimelineVariable('stimulus');
        data.valence = jsPsych.evaluateTimelineVariable('valence');
        data.itemNo = jsPsych.evaluateTimelineVariable('itemNo'); 
        data.trialNo = learningTrialNo;
        data.blockNo = jsPsych.evaluateTimelineVariable('blockNo');
        // did participant enter a choice for the trial?
        if (data.response == null) {
            // if the participant didn't respond...
            data.timedout = true;
            data.correct = null;
            data.chosen_attr_type = null;
            nTimeouts++;
        } else {
            // if the participant responded...
            data.timedout = false;
            // was chosen attribution the 'correct' option?
            data.chosen_attr = data.choices[data.response];
            if ( data.chosen_attr == jsPsych.evaluateTimelineVariable('correct_answer')) {
                data.correct = 1;
            } else {
                data.correct = 0;
            };
            // what attribution type was chosen?
            data.chosen_attr_type = '';
            if ( data.chosen_attr == jsPsych.evaluateTimelineVariable('intGlob') ) {
                data.chosen_attr_type = "internal_global";
            } else if ( data.chosen_attr  == jsPsych.evaluateTimelineVariable('intSpec') ) {
                data.chosen_attr_type = "internal_specific";
            } else if ( data.chosen_attr == jsPsych.evaluateTimelineVariable('extGlob') ) {
                data.chosen_attr_type = "external_global";
            } else if ( data.chosen_attr  == jsPsych.evaluateTimelineVariable('extSpec') ) {
                data.chosen_attr_type = "external_specific";
            }; 
        }
        data.nTimeouts = nTimeouts;
        // save data and increment trial number
        var respData = jsPsych.data.getLastTrialData().trials[0];
        saveTaskData("learningTask_"+learningTrialNo, respData);
        learningTrialNo++;
        // // manually update progress bar so just reflects task progress
        // var curr_progress_bar_value = this.type.jsPsych.getProgressBarCompleted();
        // this.type.jsPsych.setProgressBar(curr_progress_bar_value + 1/nTrials);
    }
};

var feedback = {
    type: jsPsychHtmlButtonResponseCA,
    is_html: true,
    // display previous choice options
    choices: function () {
        var prev_data = jsPsych.data.getLastTrialData().trials[0];
        if (prev_data.timedout == false) {
            return prev_data.choices; // These are the actual choice strings
        } else {
            return []; // No choices if timed out, so no buttons will be rendered
        }
    },
    button_html: "<div class='thought'><img src='assets/img/tlc/%choice%' style='width:180px;'></img></div>",  // use images as response buttons
    // highlight correct response in green text
    on_load: function () {
        jsPsych.pluginAPI.setTimeout(function() { // Defer execution to ensure DOM is ready
            var prev_data = jsPsych.data.getLastTrialData().trials[0];
            var chosenIndex = prev_data.response;

            if (prev_data.timedout == false && chosenIndex !== null) {
                // Add "responded" class to the button that was chosen in the previous trial
                const chosenButton = document.querySelector(`#jspsych-html-button-response-btngroup-ca [data-choice="${chosenIndex}"]`);
                if (chosenButton) {
                    chosenButton.classList.add("responded");
                }
            }

            var correctChoice = prev_data.correct === 1; // Check if the previous choice was correct
            if (prev_data.timedout == false) {
                var choices_shown = prev_data.choices; // Actual text of choices that were displayed
                var correct_answer_text = jsPsych.evaluateTimelineVariable('correct_answer');
                
                var correct_choice_index = -1;
                if (choices_shown && choices_shown.length > 0) {
                    for (var i = 0; i < choices_shown.length; i++) {
                        if (choices_shown[i] === correct_answer_text) {
                            correct_choice_index = i;
                            break;
                        }
                    }
                }

                if (correct_choice_index !== -1) {
                    const buttonGroup = document.querySelector('.jspsych-btn-group');
                    if (buttonGroup) {
                        buttonGroup.classList.add('responded');
                    }
                    const correctButton = document.querySelector(`#jspsych-html-button-response-btngroup-ca [data-choice="${correct_choice_index}"]`);
                    const incorrectButton = document.querySelector(`#jspsych-html-button-response-btngroup-ca [data-choice="${1 - correct_choice_index}"]`);
                    const chosenButton = document.querySelector(`#jspsych-html-button-response-btngroup-ca [data-choice="${chosenIndex}"]`);

                    chosenButton.classList.add("responded");

                    function highlightCorrect(ms) {
                        setTimeout(() => {
                            correctButton.classList.add('correct-choice');
                            incorrectButton.classList.remove('incorrect-choice'); // remove red
                            incorrectButton.classList.add('disabled-choice'); // make it grey
                            setTimeout(() => {}, 500);
                        }, ms);
                    }

                    if (correctChoice) {
                        incorrectButton.classList.add('disabled-choice');
                        highlightCorrect(0);
                    } else { // incorrect choice
                        incorrectButton.classList.add('incorrect-choice');
                        highlightCorrect(1000);
                    }
                }
            }
        }, 0);
    },
    // and give response-contingent feedback
    stimulus: function () {
        var prev_data = jsPsych.data.getLastTrialData().trials[0];
        var stim_fb;
        if ( prev_data.timedout == false & prev_data.correct == 1 ) {
            stim_fb = "<div><img src='assets/img/tlc/"+jsPsych.evaluateTimelineVariable('stimulus')+"' style='height:25vh;'></img></div>"+
                            "<div><img src='assets/img/head_correct.png' style='height:20vh;'></img></div>";
        } else if ( prev_data.timedout == false ) {
            stim_fb = "<div><img src='assets/img/tlc/"+jsPsych.evaluateTimelineVariable('stimulus')+"' style='height:25vh;'></img></div>"+
                            "<div><img src='assets/img/head_incorrect.png' style='height:20vh;'></img></div>";
        } else {
            stim_fb = "<p style='font-size:30px; font-weight: bold; color: #ff0000;'><br> You didn't choose in time!<br></p>"
        }
        return stim_fb;
    },
    // feedback displayed for set amount of time for all outcome types
    time_before_choice: function() {
        // If the participant timed out, we want to display feedback for a shorter time
        var prev_data = jsPsych.data.getLastTrialData().trials[0];
        if (prev_data.timedout) {
            return 1000; // Shorter feedback time for timeouts
        } else {
            return feedbackTime; // Standard feedback time for correct/incorrect responses
        }
    },
    // feedback displayed for set amount of time for all outcome types
    trial_duration: function() {
        // If the participant timed out, we want to display feedback for a shorter time
        var prev_data = jsPsych.data.getLastTrialData().trials[0];
        if (prev_data.timedout) {
            return 1000; // Shorter feedback time for timeouts
        } else {
            return feedbackTime; // Shorter feedback time for incorrect responses
        }
    },
    response_ends_trial: false, // despite any participant repsonses
    post_trial_gap: 750
};

// got here.

// define free text description screen (at end of block)
var freeTextFeedback = {
    type: jsPsychSurvey,
    survey_json: {
        pages: [
            { 
                name: 'qa_blue',
                elements: [
                    {
                        name: 'html_blue',
                        type: 'html',
                        html: `
                          <div><img src='assets/img/tlc/blue-basket.png' style='width:150px;'></img></div>
                          <p style="font-size:1.3rem">Please describe the kinds of objects that belonged in the <span style="color:#5a79b5;font-size:1.3rem"><b>blue basket</b></span> during the previous scenario.</p>
                        `,
                    },
                    {
                        name: 'answer_blue',
                        title: ' ',
                        type: 'comment',
                        placeholder: 'A single phrase or sentence is fine!',
                        rows: 3,
                        validators: [
                            {
                                type: 'text',
                                minLength: 12,
                                text: 'Please write a little more!'
                            }
                        ],
                        //titleLocation: 'hidden', // Hide the title
                        autoGrow: true,
                        isRequired: true,
                        requiredErrorText: 'Please write an answer to continue.'
                    }
                ],
                showQuestionNumbers: 'off'
            }, 
            {
                name: 'qa_red',
                elements: [
                    {
                        name: 'html_red',
                        type: 'html',
                        html: `
                            <div><img src='assets/img/tlc/red-basket.png' style='width:150px;'></img></div>
                            <p style="font-size:1.3rem">Now, please describe the kind of objects that belonged in the <span style="color: #d75b5b;font-size:1.3rem"><b>red basket</b></span> during the previous scenario.</p>
                        `,
                    },
                    {
                        name: 'answer_red',
                        type: 'comment',
                        title: ' ',
                        placeholder: 'A single phrase or sentence is fine!',
                        isRequired: true,
                        rows: 3,
                        validators: [
                            {
                                type: 'text',
                                minLength: 12,
                                text: 'Please write a little more!'
                            }
                        ],
                        autoGrow: true,
                        requiredErrorText: 'Please write an answer to continue.'
                    }
                ],
                showQuestionNumbers: 'off'
            }
        ]
    },
    on_finish: function() {
        // get response and RT data
        var respData = {
            "answer_neg": jsPsych.data.getLastTrialData().trials[0].response.answer_blue,
            "answer_pos": jsPsych.data.getLastTrialData().trials[0].response.answer_red,
        };
        var respRT = jsPsych.data.getLastTrialData().trials[0].rt;
        saveQuestData(["freeText_block"+blockNo], respData, respRT);
    }
};

// define cause rating screen (for at the start then after each block)
var causeRatingNeg = {
    type: jsPsychHtmlMultiSliderResponse,
    stimulus: function () {
        var stim_text;
        if ( blockNo == 0 ) {
            stim_text = `
            <div style="font-size:1.75rem; padding-bottom:2rem;"><b>Before you start...</b></div>
            <div style="font-size:1.3rem;line-height:2.2rem;">
            <p>we'd like you to <em><span style="color:#8A8A6A">look at the object below.</em></span></p> 
            <div class='center-content'><img src='assets/img/tlc/test-manmade-small.png' style='width:300px;'></img></div>
            <p>How would you describe this object using the below scales?</p>
            </div>
        `;  
        } else {
            stim_text = `
            <div style="font-size:1.3rem;line-height:2.2rem;">
            <p>Still thinking about the previous scenario, how would you describe the kind of objects which belonged in the 
                <span style="color: #5a79b5;"><b>blue basket</b></span> on the below scales?</p>
            <div class='center-content'><img src='assets/img/tlc/blue-basket.png' style='width:150px;margin-bottom:2rem'></img></div>
            </div>
        `; 
        }
        return stim_text;
    },
    multiple_sliders: true,
    require_movement: true,
    labels: [
        ['human-made', 'natural'], 
        ['bigger than a shoebox', 'smaller than a shoebox']
    ],
    button_label: 'enter answer',
    //min: 1, max: 100, slider_start: 50,  // default values
    // slider_width: 500,                     // width in px, if null, sets equal to widest element of display
    on_finish: function() {
    // get response and RT data
        var respData = jsPsych.data.getLastTrialData().trials[0].response;
        var respRT = jsPsych.data.getLastTrialData().trials[0].rt;
        saveQuestData(["ratings_neg_block"+blockNo], respData, respRT);
    }
};
var causeRatingPos = {
    type: jsPsychHtmlMultiSliderResponse,
    stimulus: function () {
        var stim_text;
        if ( blockNo == 0 ) {
            stim_text = `
            <div style="font-size:1.3rem;line-height:2.2rem;">
            <p>Next, we'd like you to <b>look at the object below</b>.</p>
            <div class='center-content'><img src='assets/img/tlc/test-natural-big.png' style='width:200px;'></img></div>
            <p>How would you describe this object using the below scales?</p>
            </div>
        `; 
        } else {
            stim_text = `
            <div style="font-size:1.3rem;line-height:2.2rem;">
            <p>Still thinking about the previous scenario, how would you describe the kind of objects which belonged in the 
                <span style="color: #d75b5b;"><b>red basket</b></span> on the below scales?</p>
            <div class='center-content'><img src='assets/img/tlc/red-basket.png' style='width:150px;margin-bottom:2rem'></img></div>
            </div>
        `;
        }
        return stim_text;
    },
    multiple_sliders: true,
    require_movement: true,
    labels: [
        ['human-made', 'natural'],
        ['bigger than a shoebox', 'smaller than a shoebox']
    ],
    button_label: 'enter answer',
    //min: 1, max: 100, slider_start: 50,  // default values
    // slider_width: 500,                     // width in px, if null, sets equal to widest element of display
    on_finish: function() {
    // get response and RT data
        var respData = jsPsych.data.getLastTrialData().trials[0].response;
        var respRT = jsPsych.data.getLastTrialData().trials[0].rt;
        saveQuestData(["ratings_pos_block"+blockNo], respData, respRT);
    }
};

// define break screen (between blocks) and end of training
var takeABreak = {
    type: jsPsychHtmlButtonResponseCA,
    is_html: true,
    choices: function() {
        // If all learning blocks are completed, this is the final screen.
        if (blockNo === nBlocksLearning) {
            return ['finish the study'];
        } else {
            return ['continue'];
        }
    },
    stimulus: function () {
        var stim_text;
        // blockNo is incremented after the intro and after each intermediate break.
        // When takeABreak is called, if blockNo equals nBlocksLearning, it means all blocks are done.
        if (blockNo === nBlocksLearning) { // All learning blocks are completed
            stim_text = ("<p>"+
                        "<h2>Thank you very much for your time. You have now finished this part of the study.</h2>"+
                        "</p>"+
                        "If you would like to find out more about the ideas behind this study, "+
                        "please see <a href=\"https://www.psychologytools.com/self-help/thoughts-in-cbt/\" target=\"_blank\">this article</a> "+
                        "about why some psychologists believe the way we interpret events is key to understanding our feelings about them."+
                        "</p>"+
                        "If you became upset at any point during the study, "+
                        "or are concerned about your mental health for any other reason, we recommend the below resources for further "+
                        "information. You may also wish to discuss any concerns with your family doctor."+
                        "</p>  " +
                        "<ul>  " +
                        "<p><a href=\"http://mind.org.uk\" target=\"_blank\">Mind Charity</a></p>"+
                        "<p><a href=\"https://www.samaritans.org\" target=\"_blank\">The Samaritans</a></p>"+
                        "<p><a href=\"https://www.nhs.uk/mental-health\" target=\"_blank\">NHS Choices mental health page</a></p>"+
                        "</ul>"+
                        "</p>"+
                        "<b>Please click the button below to return to Prolific!</b>"+
                        "</p>");
        } else { // This is an intermediate break between blocks
            stim_text = ("<p><h2>Well done!</h2></p>"+
                        "<br>"+
                        "<p>"+
                        "You are <b>now finished with this scenario</b>!"+
                        "</p>"+
                        "<p>"+
                        "When you are ready, <b>press continue to move " +
                        "on</b>. "+
                        "</p>"+
                        "<p>"+
                        "Remember, the kinds of object that belong in each basket may be different "+
                        "to the kinds of objects that were correct during the previous scenario"+
                        "</p>"+
                        "<br><br>"+
                        "</p>");
        }
        return stim_text;
    },
    on_finish: function () {
        if (blockNo !== nBlocksLearning) { // This was an intermediate break
            // Change background colour for the next scenario and increment blockNo
            // blockNo here is the 0-indexed number for the *next* block's color
            if (blockNo < sessColours.length) {
                 document.body.style.background = sessColours[blockNo];
            }
            blockNo++;
        }
    }
};

// various loops and conditionals to control the flow of the learning task ---------------------------------------------
// if trial timed out, loop trial and feedback again until participant responds
var learning_trial_node = {
    timeline: [ learning_trial, feedback ],
    loop_function: function (data_for_current_iteration) { // Renamed argument for clarity
        // Get the data from the learning_trial within the current loop iteration
        var learning_trial_data_values = data_for_current_iteration.filter({trial_id: 'learning_choice_trial'}).values();
        
        if (learning_trial_data_values.length > 0) {
            var learning_trial_outcome = learning_trial_data_values[0];
            // Check the 'timedout' property set in learning_trial.on_finish
            if (learning_trial_outcome.timedout === true) {
                return true; // Loop if the learning trial specifically timed out
            }
        }
        return false; // Otherwise, do not loop
    }
};

// display these screens if at the end of a block
var learning_break_node = {
    timeline: [ freeTextFeedback, causeRatingNeg, causeRatingPos, takeABreak ],
    conditional_function: function () {
        var trialIndex = jsPsych.evaluateTimelineVariable('trialIndex')                // use trialIndex not absolute trialNo
        if ( (trialIndex+1) % blockLengthLearning == 0  && trialIndex !=nTrialsLearning ) {
            return true;
        } else {
            return false;
        }
    }
};

// finally, define the whole set of choice trials based on above logic and timeline variables
var learning_trials = {
    timeline: [ learning_trial_node, learning_break_node ],
    timeline_variables: events_causes_learning         
};

///////////////////////////////////////////// CONCAT ////////////////////////////////////////////////////////
timeline_learn_training.push(loop_node);
timeline_learn_training.push(continueText);
timeline_learn_training.push(causeRatingNeg);
timeline_learn_training.push(causeRatingPos);
timeline_learn_training.push(task_intro);
timeline_learn_training.push(preload);
timeline_learn_training.push(learning_trials);
 
// export timeline for use in other modules
export { timeline_learn_training };