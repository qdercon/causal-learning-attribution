// import task info from versionInfo file
import { nBlocksChoice, debugging } from "./versionInfo.js"; 
import { trials } from "./trials.js"; 

// import our data saving function
import { saveTaskData } from "./saveData.js";

// import jspsych object so can access modules
import { jsPsych } from "./constructStudy.js";

// initialize task no        
var taskNo = 0;

// initialize task vars
var nTrialsChoice;
var timeBeforeChoice = 1500;         // time in ms before choice can be entered
var timeAfterChoice = 750;          // time in ms to leave trial info on screen after choice
// var trialTimeoutTime;               // time in ms after which trial times out
var blockNo = 0;
// var nRepeatTrials = 3;                  // only if using self-consistency check
var nTimeouts = 0;                      // only if using trial time-outs
var trialEvents;
var trialValence;
var trialAttrIntGlob;
var trialAttrIntSpec;
var trialAttrExtGlob;
var trialAttrExtSpec;

// grab trial info
trialEvents = trials.events;
trialValence = trials.event_valence;
trialAttrIntGlob = trials.int_glob;
trialAttrIntSpec = trials.int_spec;
trialAttrExtGlob = trials.ext_glob;
trialAttrExtSpec = trials.ext_spec

// this is the first version of the choice task (in previous studies, there was an additional version but those trials are moved to the learning task)
var choice_items = [0, 1, 2, 3, 5, 7, 8, 13, 15, 17, 18, 22, 24, 32, 39, 41, 43, 54, 62, 68, 72, 74, 79, 93, 99, 101, 104, 105, 112, 113, 116, 127];
// var choice_items_2 = [11, 21, 25, 28, 29, 31, 35, 37, 44, 45, 47, 53, 55, 56, 59, 61, 67, 73, 75, 76, 77, 78, 83, 84, 97, 100, 102, 108, 110, 115, 120, 125];

// set number of trials, blocklength, and max trial length according to debug condition
if (debugging == false) {
    nTrialsChoice = choice_items.length;
    //trialTimeoutTime = 15000;
} else {
    nTrialsChoice = 6;
    //trialTimeoutTime = 4000;
}
var blockLengthChoice = Math.round(nTrialsChoice/nBlocksChoice); 

///////////////////////////////////////////// INSTRUCTIONS CHOICE TASK /////////////////////////////////////////////////////////
var introText_choice = {
  type: jsPsychInstructions,
  allow_backward: true,
  show_clickable_nav: true,
  allow_keys: true,
  button_label_previous: "back",
  button_label_next: "next",
  pages: [ 
          "<p>"+
          "<h2>Thank you for completing the questionnaires!</h2>"+
          "</p>"+
          "<br>"+
          "<p>"+
          "When things happen to us in our day-to-day lives, we often draw assumptions about the <b>reasons "+
          "these particular events happened</b>."+
          "</p>"+
          "<div class='center-content'><img src='./assets/img/eg1_2.png' style='width:750px;'></img></div>"+
          "<br>"+
          "<p>"+
          "For the <b>final part</b> of this study, we will ask you to <b>imagine yourself in various different everyday situations</b>."+
          "<p>"+
          "For each situation, we will ask you to choose which of several possible explanations for what happened "+
          "you think is the most likely."+
          "<p>"+
          "Although we know that events can have many different causes, we would like you to "+
          "choose which explanation you think would be the <b>main reason</b> the event happened, "+
          "<b>if it actually happened to <i>you</i></b>."+
          "<br>"+
          "</p>",
          "<p>"+
          "<h2>What do I need to do?</h2>"+
          "<br>"+
          "</p>"+
          "<p>"+
          "In order to do this, we will ask you to <b>first read the sentence at the top of each page."+
          "</p>"+
          "<p>"+
          "<br>"+
          "<i>Picture the situation described as clearly as you can, as if the events were "+
          "happening to you right now.</b></i>"+
          "</p>"+
          "<br>"+
          "<div class='center-content'><img src='./assets/img/eg4.png' style='width:450px;'></img></div>"+
          "<br>"+
          "<p>"+
          "Your job is then to <b>click on one of the four ‘thought clouds’</b> below each statement, selecting which you think "+
          "<b>best describes why the situation or event happened</b>."+
          "<br>"+
          "</p>",
          "<p>"+
          "<h2>Reasons behind the design of this part of study</h2>"+
          "<br>"+
          "<p>"+
          "We know that reading many different descriptions can, over time, "+
          "start to feel repetitive. However, it is really important for the purposes of our research "+
          "that we record responses to the different events that are as <i>accurate and honest as possible</i>. "+
          "</p>"+
          "<p>"+
          "Ensuring that the responses we collect are as high quality as possible forms part of our "+
          "responsibility to the organizations that fund our research, as well any future "+
          "beneficiaries of the research."+
          "</p>"+
          "<p>"+
          "We have therefore tried to design the study in a way that makes it as easy as possible "+
          "for participants to provide accurate answers."+
          "</p>"+
          "<p>"+
          "Specifically, we have tried to make sure we have allowed <b>plenty of time in the study "+
          "completion estimate</b> to fully read all the questions and answers, and to <b>limit the number "+
          "of questions to the minimum we need</b> to answer the study questions properly."+
          "</p>"+
          "<div class='center-content'><img src='./assets/img/exam.png' style='width:200px;'></img></div>"+
          "<br>",
          "<p>"+
          "<h2>Reasons behind our approval rules</h2>"+
          "<br>"+
          "</p>"+
          "<p>"+
          "In addition to this, we will be applying <b>two quality control rules</b> to the data "+
          "from this part of the study. These rules will be used to help decide whether or not to approve submissions."+
          "</p>"+
          "<ol>"+
            "<p><li><b>Choice times</b>. Submissions with reading and choice times of <i>less than 2 seconds "+
            "for a majority of trials</i> will not be approved, as it is not possible to properly process "+
            "the question and answer information in this time.</li></p>"+
            "<p><li><b>Choice repetition</b>. Submissions with the <i>same option chosen for a majority of questions</i> "+
            "(e.g., almost all answers are the top right option) may also not be approved.</li></p>"+
          "</ol>"+
          "<p>"+
          "We hope that the above measures are reasonable and clearly explained. If you don\'t think this is the case, "+
          "please get in touch and let us know."+
          "</p>"+
          "<div class='center-content'><img src='./assets/img/thank-you.png' style='width:150px;'></img></div>"+
          "<p>"+
          "Above all, <b>we are very grateful to our study participants for volunteering their time to help us "+
          "with our research</b>. Having quality control checks like the above on our data means that we can "+
          "be more confident in the conclusions we can draw from online studies, and be more likely "+
          "to be able to conduct these kind of studies in the future."+
          "<br>"+
          "</p>",
          "<br>"+
          "<p>"+
          "Before you continue to the first part of the study, we will ask you to <b>answer some quick questions</b>. "+
          "This is in order to make sure we have explained all the previous information clearly enough."+
          "</p>"+
          "<div class='center-content'><img src='./assets/img/quiz.png' style='width:200px;'></img></div>"+
          "<p>"+
          "<b>If you don\'t get all the questions right the first time, you will be routed back to the start of these instructions "+
          "to try again</b>. This helps us make sure everything is completely clear before we get started!"+
          "</br>"+
          "</p>"
          ],
//   on_finish: function() { // integrated with prescreen/final task, so don't want to overwrite!
//     var startTime = performance.now();
//     saveStartData(startTime);
//   }
};

var quizQuestions_choice_json = {
  showQuestionNumbers: "off",
  pages: [
    {
      name: "quiz_choice",
      elements: [
        {
          type: "radiogroup",
          name: "Q0",
          title: "1. The point of this part of the study is to...",
          isRequired: true,
          choices: [
            "A. Answer each question according to how you think most people would explain the causes of different events",
            "B. Answer each question according to what you think would be the main cause behind an event, if it actually happened to you",
            "C. Answer each question completely at random, in order to generate unusable data"
          ]
        },
        {
          type: "radiogroup",
          name: "Q1",
          title: "2. Events usually have multiple reasons behind them. For this part of the study, I should...",
          isRequired: true,
          choices: [
            "A. Select the main reason I think explains the events, from the options available",
            "B. Select all of the reasons I think are relevant to the event",
            "C. Type in my own answers"
          ]
        },
        {
          type: "radiogroup",
          name: "Q2",
          title: "3. I understand that some quality control checks will be applied to my data, in order to ensure the scientific integrity of the study. The first check is that submissions with very quick choice times (majority less than 2s) are likely to be rejected. The second check is that...",
          isRequired: true,
          choices: [
            "A. If choose the same answer every time, my data may not be approved",
            "B. If choose the same answer every time, my data will definitely be approved",
            "C. If I always select the bottom-left choice, my data will be approved"
          ]
        }
      ]
    }
  ]
};

var nCorrect = 0;
const nQuests_choice = 3; 
var introQuiz_choice = {
    type: jsPsychSurvey,
    survey_json: quizQuestions_choice_json,
    data: {
        correct_answers: ["B", "A", "A"] // Correct letter choices
    },
    button_label: "check answers", 
    on_finish: function (data) {
        nCorrect = 0;
        const correctAnswers = data.correct_answers;
        for (var i = 0; i < nQuests_choice; i++) {
            var questID = "Q" + i;
            // Check if the response exists and if its first letter matches the correct answer
            if (data.response[questID] && data.response[questID].length > 0 && data.response[questID][0].toUpperCase() === correctAnswers[i].toUpperCase()) {
                nCorrect++;
            }
        }
        data.nCorrect = nCorrect;
    }
};

var loop_node_choice = {
    timeline: [introText_choice, introQuiz_choice],
    loop_function: function(data) {
        if (nCorrect >= nQuests_choice) {
            return false;
        } else {
            return true;
        }
    }
};

var continueText_choice = {
    type: jsPsychInstructions,
    allow_backward: false,
    show_clickable_nav: true,
    allow_keys: true,
    button_label_next: "continue",
    pages: [
        "<p><h2>Thank you! You got all the questions correct!</h2></p>"+
        "<br>"+
        "<p>"+
        "Just to remind you one more time, what we would like you to do <b>for this part of the study</b> is read "+
        "the description of each event, then <b>select the main reason you think that event would have happened, "+
        "if it actually happened to you</b>."+
        "</p>"+
        "<p>"+
        "Please try and choose your answer as truthfully and carefully as possible. <i>If you try and "+
        "click on an answer too quickly after the description has been displayed, it may not register yet</i>. "+
        "</p>"+
        "<p>"+
        "In this part of the study, we will ask you about to think about the reasons behind <b>32 different events</b>. "+
        "We will let you know when you are half-way through the events. At this point, you can take a short break if you like."+
        "</p>"+
        "<p>"+
        "Please press the <b>continue</b> button when you are ready to start!"+
        "</p><br>"
    ],
};

// initialize task vars
var nTrialsChoice;          
var blockNo = 0;

// define trial stimuli and choice arrays for use as a timeline variable 
var events_causes_choice = [];
for (var i = 0; i < nTrialsChoice; i++ ) {
    var itemNo = choice_items[i];
    events_causes_choice[i] = { stimulus: trialEvents[itemNo],
                                  valence: trialValence[itemNo],
                                  intGlob: trialAttrIntGlob[itemNo],
                                  intSpec: trialAttrIntSpec[itemNo],
                                  extGlob: trialAttrExtGlob[itemNo],
                                  extSpec: trialAttrExtSpec[itemNo],
                                  itemNo: itemNo,
                                  trialIndex: i,
                                  taskNo: taskNo };
};

// define individual choice trials
var choiceTrialNo = 0;
var choice_types = ['intGlob', 'intSpec', 'extGlob', 'extSpec'];
var choice_trial = {
    // jsPsych plugin to use
    type: jsPsychHtmlButtonResponseCA,
    data: { trial_id: 'choice_selection_trial' }, // Added for v8 loop compatibility
    // trial info
    prompt: null,
    stimulus: function() {
        return `
            <p style='font-size:2rem; line-height:3rem; padding:0px; font-weight: bold;'>
                ${jsPsych.evaluateTimelineVariable('stimulus')}</p>
            <img src='assets/img/head_why.png' style='height:25vh;'></img>
        `;
    },
    choices: function () {
        var display_order = jsPsych.randomization.repeat(choice_types, 1);
        return [
            jsPsych.evaluateTimelineVariable(display_order[0]),
            jsPsych.evaluateTimelineVariable(display_order[1]),
            jsPsych.evaluateTimelineVariable(display_order[2]),
            jsPsych.evaluateTimelineVariable(display_order[3])
        ];
    },
    save_trial_parameters: {
        choices: true
    },
    // // trial timing - infinite wait version
    trial_duration: null,                   // wait indefinitely for response
    stimulus_duration: null,                // stim text remains on screen indefinitely
    time_before_choice: timeBeforeChoice,   // time in ms before the ppt can enter a choice
    time_after_choice: timeAfterChoice,     // time in ms to leave trial info on screen following choice
    response_ends_trial: true,              // trial ends only when response entered
    // styling
    button_html: "<div class='thought'>%choice%</div>",
    button_layout: 'flex',
    // at end of each trial
    on_finish: function(data, trial) {
        // add chosen interpretation type to output
        data.stimulus = jsPsych.evaluateTimelineVariable('stimulus');
        data.valence = jsPsych.evaluateTimelineVariable('valence');
        data.itemNo = jsPsych.evaluateTimelineVariable('itemNo'); 
        data.taskNo = jsPsych.evaluateTimelineVariable('taskNo'); 
        data.trialNo = choiceTrialNo;
        // did participant enter a choice for the trial?
        if (data.response == null) {
            // if the participant didn't respond...
            data.timedout = true;
            data.chosen_attr_type = null;
            nTimeouts++;
        } else {
            // if the participant responded...
            data.timedout = false;
            data.chosen_attr = data.choices[data.response];
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
        // // save data and increment trial number
        var respData = jsPsych.data.getLastTrialData().trials[0];
        saveTaskData("choiceTask"+taskNo+"_"+choiceTrialNo, respData);
        choiceTrialNo++;
        // // manually update progress bar so just reflects task progress
        // var curr_progress_bar_value = this.type.jsPsych.getProgressBarCompleted();
        // this.type.jsPsych.setProgressBar(curr_progress_bar_value + 1/nTrials);
    }
};

// define break screen (between blocks)
var takeABreak = {
    type: jsPsychHtmlButtonResponseCA,
    is_html: true,
    choices: ['continue'],
    stimulus: function () {
        return `
            <p><h2>Thank you!</h2></p>
            <br>
            <p>You are <b>half-way through this part of the study</b>. If you like, you can take a short break now.</p>
            <p>When you are ready, press <b>continue</b> to finish the second half of the questions.</p>
            <br><br>
        `;
    },
    on_finish: function () {
        if ( blockNo >= nBlocksChoice-1 ) {
            choiceTrialNo = 0;
            blockNo = 0;
        } else {
            // increment blockNo
            blockNo++;
        }
    }
};

// if trial timed out, loop trial and feedback again until participant responds
var choice_trial_node = {
    timeline: [ choice_trial ],
    loop_function: function (data) { // data is from the timeline iteration
        var choice_trial_data = data.filter({ trial_id: 'choice_selection_trial' }).values();
        if (choice_trial_data.length > 0 && choice_trial_data[0].timedout) {
            return true; 
        } else {
            return false; 
        }
    }
};

// display these screens if at the end of a block
var choice_break_node = {
    timeline: [ takeABreak ],
    conditional_function: function () {
        var trialIndex = jsPsych.evaluateTimelineVariable('trialIndex'); // use trialIndex not absolute trialNo
        if ( (trialIndex+1) % blockLengthChoice == 0  && trialIndex != nTrialsChoice -1 ) { // check against nTrialsChoice-1 for last trial
            return true;
        } else {
            return false;
        }
    }
};

// finally, define the whole set of choice trials based on above logic and timeline variables
var choice_trials = {
    timeline: [ choice_trial_node, choice_break_node ],
    timeline_variables: events_causes_choice     
};

// brief re-instructions for choice test 2
var preamble_choice_2 = {
    type: jsPsychHtmlButtonResponseCA,
    choices: ['continue'],
    is_html: true,
    stimulus: (`
        <p><h2>Welcome to the final part of the study!</h2></p>
        <p>
        Like the first part of the study, we would like you to read the description of each of the following events, 
        then <b><i>select the main reason you think that event would have happened, if it actually happened 
        to you</i></b>. 
        </p>
        <p>
        For this part of the study, there are <b>no right or wrong answers</b>. All we ask if that you
        try and choose the answer that <i>most accurately reflects the main reason you think would be behind
        the event, if it happened to you right now</i>.
        </p>
        <p>
        We will again ask you to think about 32 different events. Like the first part of the study, we
        will let you know when you are half-way through, and you can take a short break then if you like.
        </p>
        <p>
        Please press the <b>continue</b> button when you are ready to start!
        </p>
    `)
};

///////////////////////////////////////////// CONCAT ////////////////////////////////////////////////////////
var timeline_choice_1 = [];
timeline_choice_1.push(loop_node_choice);
timeline_choice_1.push(continueText_choice);
timeline_choice_1.push(choice_trials);

var timeline_choice_2 = [];
timeline_choice_2.push(preamble_choice_2);
timeline_choice_2.push(choice_trials);

export { timeline_choice_1, timeline_choice_2 };