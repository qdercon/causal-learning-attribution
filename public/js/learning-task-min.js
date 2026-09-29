// import task info from versionInfo file
import { nBlocksLearning, nScenarios, session, debugging } from "./versionInfo.js"; 
import { trials } from "./trials.js"; 
 
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
var trialEvents;
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

// grab trial info
trialEvents = trials.events;
trialValence = trials.event_valence;
trialAttrIntGlob = trials.int_glob;
trialAttrIntSpec = trials.int_spec;
trialAttrExtGlob = trials.ext_glob;
trialAttrExtSpec = trials.ext_spec

// we have 6 sessions worth of learning items, in 3 blocks of 10 items each, balanced for valence and interpersonal content
var learning_items_list = [
    [77, 97, 121, 123, 103, 53, 23, 110, 6, 98, 115, 40, 114, 122, 75, 25, 100, 69, 65, 9, 34, 67, 35, 27, 37, 90, 16, 119, 51, 64],
    [48, 117, 11, 45, 89, 59, 29, 46, 20, 63, 84, 58, 56, 14, 55, 30, 21, 60, 94, 38, 76, 124, 126, 107, 88, 10, 102, 33, 125, 36],
    [103, 57, 51, 50, 26, 77, 91, 37, 66, 47, 53, 90, 118, 119, 34, 61, 109, 110, 25, 81, 87, 75, 108, 73, 12, 70, 4, 85, 42, 28],
    [64, 31, 95, 86, 121, 52, 96, 78, 69, 125, 111, 21, 33, 126, 19, 71, 102, 120, 36, 83, 20, 55, 65, 49, 76, 11, 6, 40, 44, 10],
    [58, 61, 26, 97, 47, 35, 94, 16, 123, 117, 100, 67, 73, 9, 38, 92, 87, 27, 82, 4, 88, 114, 108, 106, 115, 98, 14, 80, 81, 28],
    [70, 107, 78, 118, 30, 31, 46, 23, 57, 84, 60, 59, 83, 44, 48, 91, 42, 12, 56, 124, 29, 109, 89, 85, 50, 63, 122, 45, 120, 66]
];
var learning_items = learning_items_list[session-1]; // get learning items for this session

// set number of trials, blocklength, and max trial length according to debug condition
if (debugging == false) {
    nTrialsLearning = learning_items.length;
    trialTimeoutTime = 15000;
} else {
    nTrialsLearning = 12;
    trialTimeoutTime = 4000;
}
var blockLengthLearning = Math.round(nTrialsLearning/nBlocksLearning);

///////////////////////////////////////////// LEARNING TASK TIMELINE /////////////////////////////////////////////////////////
var timeline_causal_training = [];

var introText = {
  type: jsPsychInstructions,
  allow_backward: true,
  show_clickable_nav: true,
  allow_keys: false,
  button_label_previous: "back",
  button_label_next: "next",
  pages: [ /////////////////////page one////////////////////////
    "<p>"+
    "<h1>Welcome!</h1>"+
    "</p>"+
    "<br>"+
    "<p>In the following pages, we will walk you through how to complete this task.</p>"+
    "<p>If you have already completed this training before, feel free to skip through the instructions.</p>"+
    "<br><br>"+
    "</p>",
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
    "In this part of the study, we will ask you to think about a new series of events, that occur in <b>"+nScenarios+" different scenarios</b>. "+
    "<b>Each scenario represents a different kind of mood a person can be in</b> (although we won’t tell you "+
    "what kind of mood it is!)"+
    "</p>"+
    "<br>"+
    "<div class='center-content'><img src='assets/img/3_scenarios.png' style='width:600px;'></img></div>"+
    "<p>"+
    "This means that, although the events that occur during each mood scenario will all be different, "+
    "<b>the same kind of underlying reasons will be thought to be responsible for events <i>within</i> each scenario</b>. "+
    "</p>"+
    "<p>"+
    "For each scenario or mood state, it is now your job to try and work out <i>what kinds of reasons are "+
    "thought to be responsible for events</i>."+
    "</p>",
    /////////////////////page three////////////////////////
    "<p>"+
    "<h2>What do I need to do?</h2>"+
    "<br>"+
    "</p>"+
    "<p>"+
    "Specifically, you will be asked to <b>read a new series of sentences describing everyday events</b>. "+
    "</p>"+
    "<p>"+
    "Below each sentence, are <b>two different potential explanations</b> for why that event occurred. "+
    "</p>"+
    "<br>"+
    "<div class='center-content'><img src='assets/img/eg3.png' style='width:450px;'></img></div>"+
    "<br>"+
    "<p>"+
    "For each event, you must <b>choose which you think the most likely explanation is</b>, by clicking "+
    "on the relevant thought bubble."+
    "<br><br>"+
    "</p>",
    /////////////////////page four////////////////////////
    "<p>"+
    "<h2>What do I need to do?</h2>"+
    "<br>"+
    "</p>"+
    "<p>"+
    "You will then <b>discover if that explanation was correct or not, for <i>this particular scenario or mood</i></b>. "+
    "</p>"+
    "<p>If you chose the correct explanation, it will turn <span style='color:green;'>green</span>, and you will see a tick symbol with the word CORRECT.</p>"+
    "<p>If you chose the incorrect explanation, it will turn <span style='color:red;'>red</span>, and you will see a cross symbol with the word INCORRECT. "+
        "Your answer will then be <span style='color:grey;'>greyed out</span>, and the correct answer will be highlighted in <span style='color:green;'>green</span>.</p>"+
    "<div class='center-content'><img src='assets/img/corr_incorr.png' style='width:450px;'></img></div>"+
    "<p>"+
    "As we would like people to stay focused on learning during each scenario, there is a <b>time limit</b> "+
    "to enter your answer for each event. If no choice is made within the time limit (<b>15 seconds</b>), then the screen "+
    "will show the message 'You didn't choose in time!'. You will then be asked to choose for that event "+
    "again. "+
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
    "To recap, <b>for each scenario or mood, there is one kind of explanation that is the correct answer</b>. This will stay the same "+
    "all the way through each scenario, but may change <i>between</i> scenarios."+
    "</p>"+
    "<p>"+
    "Your job during the study is therefore to learn, through trial and error, <b>what you think the "+
    "right kind of explanation is for that scenario or mood</b>. "+
    "</p>"+
    "</p>"+
    "<div class='center-content'><img src='assets/img/head_why.png' style='width:150px;'></img></div>",
    // "<p>"+
    // "In order to motivate you to learn the right explanations, all approved submissions will <b>earn a bonus</b> payment, "+
    // "the size of which depends on <b> how many answers you get right</b>. Specifically, you will earn an extra "+bonusRate+" pence "+
    // "for every correct explanation you choose (<b>max possible bonus £"+maxBonus.toFixed(2)+"</b>)."+
    // "</p>"+
    // "<p>"+
    // "We know that reading descriptions of many different events can, over time, "+
    // "start to feel repetitive. However, it is really important for the purposes of our research that "+
    // "we understand how well people can learn about different kinds of explanations for events. "+
    // "</p>"+
    // "<p>"+
    // "We have tried to design our study in a way that makes it as easy as possible for our participants "+
    // "to provide accurate answers. Specifically, as well as giving a <b>bonus payment</b>, we have tried to make sure we have allowed <b>plenty "+
    // "of time in the study completion estimate</b> to fully read all the questions and answers. "+
    // //"A <b>‘progress’ bar</b> has also been included to show <b>how far through the study you are</b> at any one point in time. "+
    // "(If you have any other suggestions - please let us know!)."+
    //"</p>",
    // /////////////////////page six////////////////////////
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
            "A. Select the reason you think is most likely to be correct, according to how you think most people would explain the causes of different events", 
            "B. Select the reason you think is most likely to be correct, according to what you think would be the main cause behind an event if it happened to you right now", 
            "C. Select the reason you think is most likely to be correct, thinking about the kinds of reasons that have been responsible for events so far for this scenario or mood"
          ]
        },
        {
          type: "radiogroup",
          name: "Q1",
          title: "2. This part of the study will ask me to think about events that happen in " + nScenarios + " different scenarios. The best way to think about these different scenarios is...",
          isRequired: true,
          choices: [
            "A. That each scenario represents a different kind of mood a person can be in. This means that events within each scenario will have all kinds of different underlying reasons, which it is impossible to learn",
            "B. That each scenario represents a different kind of mood a person can be in. This means that events within each scenario are thought to be caused by similar reasons, but that when the scenario or mood changes, the kind of reasons thought to be correct may also change",
            "C. That each scenario represents a different kind of mood a person can be in. This means that events are likely to be explained by the same kinds of reasons all the way through the study"
          ]
        },
        {
          type: "radiogroup",
          name: "Q2",
          title: "3. After I select the reason I think is responsible for each event, I will find out whether my choice was correct or incorrect.",
          isRequired: true,
          choices: [
            "A. To help me learn, the incorrect explanation will disappear from the screen.",
            "B. To help me learn, the incorrect explanation will be highlighted in green.",
            "C. To help me learn, the correct explanation will be highlighted in green and the incorrect explanation will be greyed out."
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
    const correctAnswers = jsPsych.getCurrentTrial().data.correct_answers;
    for (var i = 0; i < nQuests; i++) {
        var questID = "Q" + i;
        // Check if the response exists and if its first letter matches the correct answer
        if (data.response[questID] && data.response[questID].length > 0 && data.response[questID][0].toUpperCase() === correctAnswers[i].toUpperCase()) {
            nCorrect++;
        }
    }
    data.nCorrect = nCorrect;
  }
};

var loop_node = {
  timeline: [ introText, introQuiz ],
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
    "part of the study is read the description of each event, then <b>select the reason you think "+
    "is most likely to be correct</b> for the event, thinking about <b>the kinds of reasons "+
    "that have been thought to be correct for events so far for during this particular scenario or mood</b>."+
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
            "<p><h2>Welcome to the task</h2></p>"+
            "<br>"+
            "<p>"+
            "<b>You are now ready to start learning about the first scenario</b>."+
            "</p>"+
            "<p>"+
            "Remember, each scenario can be be thought of as representing a <i>a different "+
            "kind of mood a person can be in.</i>"+
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
        // 
        blockNo++;
    }
};

// define trial stimuli and choice array for use as a timeline variable 
var events_causes_learning = [];
// define array blockNums of length nTrialsLearning, to indicate which block each trial belongs to
var blockNums = new Array(nTrialsLearning);
var blockType = new Array(nTrialsLearning);
// assign block type to each trial
for ( var i = 0; i < nTrialsLearning; i++ ) {
    blockNums[i] = Math.floor(i / blockLengthLearning);
    blockType[i] = blockOrder[blockNums[i]];
}

for ( var i = 0; i < nTrialsLearning; i++ ) {
    var itemNo = learning_items[i];
    events_causes_learning[i] = { 
                               trialIndex: i,
                               stimulus: trialEvents[itemNo],
                               valence: trialValence[itemNo],
                               intGlob: trialAttrIntGlob[itemNo],
                               intSpec: trialAttrIntSpec[itemNo],
                               extGlob: trialAttrExtGlob[itemNo],
                               extSpec: trialAttrExtSpec[itemNo],
                               itemNo: itemNo,
                               blockNo: null,
                               choice1: null,
                               choice2: null,
                               correct_answer: null };
    if ( blockType[i] == 0 ) {
        events_causes_learning[i].choice1 = trialAttrIntGlob[itemNo];
        events_causes_learning[i].choice2 = trialAttrIntSpec[itemNo];
        if ( trialValence[itemNo] == "negative") {
            events_causes_learning[i].correct_answer = trialAttrIntSpec[itemNo];
        } else {
            events_causes_learning[i].correct_answer = trialAttrIntGlob[itemNo];
        }
    } else if ( blockType[i] == 1 ) {
        events_causes_learning[i].choice1 = trialAttrIntGlob[itemNo];
        events_causes_learning[i].choice2 = trialAttrExtGlob[itemNo];
        if ( trialValence[itemNo] == "negative") {
            events_causes_learning[i].correct_answer = trialAttrExtGlob[itemNo];
        } else {
            events_causes_learning[i].correct_answer = trialAttrIntGlob[itemNo];
        }
    } else if ( blockType[i] == 2 ) {
        events_causes_learning[i].choice1 = trialAttrIntGlob[itemNo];
        events_causes_learning[i].choice2 = trialAttrExtSpec[itemNo];
        if ( trialValence[itemNo] == "negative") {
            events_causes_learning[i].correct_answer = trialAttrExtSpec[itemNo];
        } else {
            events_causes_learning[i].correct_answer = trialAttrIntGlob[itemNo];
        }
    }
    events_causes_learning[i].blockNo = blockNums[i] +1;
};

// define individual choice trials
var learningTrialNo = 0;
var learning_choice_types = ['choice1', 'choice2'];
var learning_trial = {
    type: jsPsychHtmlButtonResponseCA,
    data: { trial_id: 'learning_choice_trial' }, // Added for specific identification
    // trial info
    prompt: null, 
    stimulus: function() {
        return `
            <p style='font-size:30px; font-weight: bold;'>${jsPsych.evaluateTimelineVariable('stimulus')}</p>
            <img src='assets/img/head_why.png' style='height:25vh;'></img>
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
    button_html: "<div class='thought'>%choice%</div>",
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
            data.timedout = true; // This property will be checked in the loop_function
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

        // Add "responded" class to the chosen button after the trial finishes data processing
        // but before the next trial (feedback) starts.
        // This needs to happen in a way that it can be cleared or handled by the feedback trial.
        // The feedback trial's on_load will now be responsible for this.
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
    button_html: "<div class='thought'>%choice%</div>",
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
            stim_fb = "<p style='font-size:30px; font-weight: bold;'>"+jsPsych.evaluateTimelineVariable('stimulus')+"</p>"+
                          "<img src='assets/img/head_correct.png' style='height:25vh;'></img>";
        } else if ( prev_data.timedout == false ) {
            stim_fb = "<p style='font-size:30px; font-weight: bold;'>"+jsPsych.evaluateTimelineVariable('stimulus')+"</p>"+
                          "<img src='assets/img/head_incorrect.png' style='height:25vh;'></img>";
        } else {
            stim_fb = "<p style='font-size:30px; font-weight: bold; color: #FF0000;'><br> You didn't choose in time!<br></p>"
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
    trial_duration: function() {
        // If the participant timed out, we want to display feedback for a shorter time
        var prev_data = jsPsych.data.getLastTrialData().trials[0];
        if (prev_data.timedout) {
            return 1000; // Shorter feedback time for timeouts
        } else {
            return feedbackTime; // Standard feedback time for correct/incorrect responses
        }
    },
    response_ends_trial: false, // despite any participant repsonses
    post_trial_gap: 750                     // post trial gap (ITI)              
};

// define free text description screen (at end of block)
var freeTextFeedback = {
    type: jsPsychSurvey,
    survey_json: {
        pages: [
            { 
                name: 'qa_neg',
                elements: [
                    {
                        name: 'question_neg_html',
                        type: 'html',
                        html: `<p style="font-size:1.3rem">Please describe your general impression of the <span style="color:#64C850;font-size:1.3rem">correct</span> reasons 
                        for <b><em><span style="color:#368BC1;font-size:1.3rem">negative</span></em></b> events during the previous scenario.</p></div>`,
                    },
                    {
                        name: 'answer_neg',
                        title: ' ',
                        type: 'comment',
                        placeholder: 'A single phrase or sentence is fine!',
                        rows: 3,
                        validators: [
                            {
                                type: 'text',
                                minLength: 12,
                                text: 'Please write a few more words!'
                            }
                        ],
                        autoGrow: true,
                        isRequired: true,
                        requiredErrorText: 'Please write an answer to continue.'
                    }
                ],
                showQuestionNumbers: 'off'
            }, 
            {
                name: 'qa_pos',
                elements: [
                    {
                        name: 'question_pos_html',
                        type: 'html',
                        html: `<p style="font-size:1.3rem">Now, please describe your general impression of the <span style="color:#64C850;font-size:1.3rem">correct</span> reasons
                        for <b><em><span style="color:green;font-size:1.3rem">positive</span></em></b> events during the previous scenario.</p></div>`,
                    },
                    {
                        name: 'answer_pos',
                        title: ' ',
                        type: 'comment',
                        placeholder: 'A single phrase or sentence is fine!',
                        isRequired: true,
                        rows: 3,
                        validators: [
                            {
                                type: 'text',
                                minLength: 12,
                                text: 'Please write a few more words!'
                            }
                        ],
                        autoGrow: true,
                        isRequired: true,
                        requiredErrorText: 'Please write an answer to continue.'
                    }
                ],
                showQuestionNumbers: 'off'
            }
        ]
    },
    on_finish: function() {
        // get responses for the free text questions (i.e., answer_neg and answer_pos)
        var respData = {
            "answer_neg": jsPsych.data.getLastTrialData().trials[0].response.answer_neg,
            "answer_pos": jsPsych.data.getLastTrialData().trials[0].response.answer_pos
        };
        var respRT = jsPsych.data.getLastTrialData().trials[0].rt;
        saveQuestData(["freeText_block"+blockNo], respData, respRT);
    }
};

// define cause rating screen (for at the start then after each block)
// <p>Click and drag the sliders below until you are happy with your answer,
// then press the 'enter answer' button to continue. You will have to move all the sliders 
// before your answer can be entered.</p></div>
var causeRatingNeg = {
    type: jsPsychHtmlMultiSliderResponse,
    stimulus: function () {
        var stim_text;
        if ( blockNo == 0 ) {
            stim_text = `
                <div style="font-size:1.75rem; padding-bottom:2rem;"><b>Before you start...</b></div>
                <div style="font-size:1.3rem;line-height:2.2rem;">
                <p>...we'd like you to <em><span style="color:#8A8A6A">think about something 
                    </span></em><span style="color: #FF0000;"><b>negative</b></span> that happened to you over the past few weeks.</p>
                <p>For example, something that didn’t go the way you would have liked it to at work, or in a social situation.
                    <br>How would you describe the main reasons behind this event on the below scales?</p></div>
                <div style="font-size:1.5rem; padding: 3rem 0 2rem 0;"><em>the <span style="color: #FF0000;"><b>negative</b></span> event was caused...</em></div>            `;
        } else {
            stim_text = `
                <div style="font-size:1.3rem;line-height:2.2rem;">
                <p>Still thinking about the <em><span style="color:#8A8A6A">previous mood scenario, </span></em> how would you describe the kind of reasons thought 
                    to be behind <span style="color: #FF0000;"><b>negative</b></span> events on the below scales?</p></div>
                <div style="font-size:1.5rem; padding: 3rem 0 2rem 0;"><em>the <span style="color: #FF0000;"><b>negative</b></span> events were thought to be caused...</em></div>
            `;
        }
        return stim_text;
    },
    multiple_sliders: true,
    require_movement: true,
    labels: [
        ['completely by other people or circumstances', 'completely by myself'],
        ['by things related to the specific circumstances', 'by things that affect all areas of my life']
    ],
    button_label: 'enter answer',
    //slider_width: 500,                     // width in px, if null, sets equal to widest element of display
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
                <p>Next, we'd like you to <em><span style="color:#8A8A6A">think about something 
                    </span></em><span style="color: #64C850;"><b>positive</b></span> that happened to you over the past few weeks.</p>
                <p>For example, something that went well at work, socially, or when taking part in a leisure or hobby activity.
                    <br>How would you describe the main reasons behind this event on the below scales?</p></div>
                <div style="font-size:1.5rem; padding: 3rem 0 2rem 0;"><em>the <span style="color: #64C850"><b>positive</b></span> event was caused...</em>
                </div>
            `; 
        } else {
            stim_text = `
                <div style="font-size:1.3rem;line-height:2.2rem;">
                <p> Still thinking about the <em><span style="color:#8A8A6A">previous mood scenario, </span></em> how would you describe the kind of reasons thought 
                    to be behind <span style="color: #64C850"><b>positive</b></span> events on the below scales?</p></div>
                <div style="font-size:1.5rem; padding: 3rem 0 2rem 0;"><em>the <span style="color: #64C850"><b>positive</b></span> events were thought to be caused...</em></div>
            `; 
        }
        return stim_text;
    },
    multiple_sliders: true,
    require_movement: true,
    labels: [
        ['completely by other people or circumstances', 'completely by myself'],
        ['by things related to the specific circumstances', 'by things that affect all areas of my life']
    ],
    button_label: 'enter answer',
    //min: 1, max: 100, slider_start: 50,  // default values
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
                        "Remember, each scenario represents a different kind of mood a person can be in. "+
                        "</p>"+
                        "<p>"+
                        "The kinds of reasons thought to be behind events in the new scenario may be different "+
                        "to the kinds of reasons thought to be correct during the previous scenario."+
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
timeline_causal_training.push(loop_node);        // loop through instructions and quiz until correct
timeline_causal_training.push(continueText);     // loop through instructions and quiz until correct
timeline_causal_training.push(causeRatingNeg);
timeline_causal_training.push(causeRatingPos);
timeline_causal_training.push(task_intro);
timeline_causal_training.push(learning_trials);

// Run experiment
export { timeline_causal_training };