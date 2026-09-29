// Script to run a series of self-report questionnaires, using built-in JsPsych functionality

// import relevant task info
import { nQuests } from "./versionInfo.js";

// import data saving functions
import { saveStartData, saveQuestData, saveCondData, saveEligData } from "./saveData.js";

// import global jspsych object
import { jsPsych } from "./constructStudy.js";

// import choice task elements
import { timeline_choice_1, timeline_choice_2 } from "./choice-task-min.js";

// Initialize study eligibility object
let studyEligibility = {
    isEligible: true,
    reasonIneligible: [],
    phq9_score: null,
    phq2_score: null, // Total score for PHQ-2 (first two items)
    phq9_item9_response: null,
    das_catch_1_response: null,
    erqcr_catch_2_response: null,
    age: null,
    all_criteria_checked: { // to ensure all checks are performed
        age: false,
        phq9: false,
        das_catch_1: false,
        erqcr_catch_2: false
    }
};

// Helper function to update eligibility status
function updateEligibility(criterion, value, messageIfNotMet) {
    let conditionMet = false;
    studyEligibility.all_criteria_checked[criterion] = true;

    switch (criterion) {
        case 'age':
            studyEligibility.age = value;
            conditionMet = value >= 18 && value <= 65;
            break;
        case 'phq9':
            const { phq9_total, phq9_item9, phq2_total } = value;
            studyEligibility.phq9_score = phq9_total;
            studyEligibility.phq9_item9_response = phq9_item9;
            studyEligibility.phq2_score = phq2_total;
            if ((phq9_total >= 5 && phq9_total <= 9 && phq2_total >= 2) ||
                (phq9_total >= 10 && phq9_total <= 14 && phq9_item9 <= 1 && phq2_total >= 2)) {
                conditionMet = true;
            }
            break;
        case 'das_catch_1': // DAS catch_1 response is 2 ("Disagree") or 3 ("Totally Disagree")
            studyEligibility.das_catch_1_response = value;
            conditionMet = value === 2 || value === 3;
            break;
        case 'erqcr_catch_2': // ERQCR catch_2 response is 0 ("1 (strongly disagree)") or 1 ("2")
            studyEligibility.erqcr_catch_2_response = value;
            conditionMet = value === 0 || value === 1;
            break;
    }

    if (!conditionMet && studyEligibility.isEligible) { // Only set to false if currently eligible, and add reason once
        studyEligibility.isEligible = false;
        studyEligibility.reasonIneligible.push(messageIfNotMet);
    } else if (!conditionMet && !studyEligibility.reasonIneligible.includes(messageIfNotMet)) {
        studyEligibility.reasonIneligible.push(messageIfNotMet); // Add reason if not already present
    }
}


// initialize sizing vars
var scaleDisplayWidth = 600;  // in px

///////////////////////////////////////////// MISC TEXT /////////////////////////////////////////////////////////
var questsIntroText = {
    type: jsPsychInstructions,
    allow_backward: false,
    show_clickable_nav: true,
    allow_keys: false,
    button_label_next: "next",
    pages: [
        "<p><h2>Welcome to the study!</h2></p>" +
        "<br>" +
        "<p>For this study, we will ask you to complete " + nQuests + " short questionnaires, which include some questions about your general feelings, thinking style, and mood.</p>" +
        "<p>We will then ask you provide some information about yourself and your personal circumstances.</p>" +
        "<p>These questionnaires should take <b>no more than 8 minutes</b> to complete.</p>",
        "<p>After finishing this study, some people will then be <b>invited to take part in a longer study</b>, which will take place over the next two weeks.</p>" +
        "<p>This will involve completing a short task (~6 minutes) every two days (6 times), and playing a short game twice (once today, and once in two weeks).</p>",
        "<p>We are carrying out this initial study to make sure that we are able to include people with a range of different kinds of thinking styles and moods.</p>" +
        "<p>This is very important in helping us achieve the research goals of the longer study, so <b>please choose your responses to the following questions as carefully and accurately as you can.</b></p>"
    ],
    on_start: function () {
        //jsPsych.setProgressBar(0);
        document.body.style.background = "aliceblue";
    },
    on_finish: function () {
        var startTime = performance.now(); // jsPsych.getStartTime();
        saveStartData(startTime);
    }
};

// Text for the follow-up questionnaires and task
var questsFinalText = {
    type: jsPsychHtmlButtonResponseCA,
    stimulus: "<h2> Thank you for participating in our study and completing the tasks over the last two weeks!</h2><br>" +
        "<p> We are extremely grateful for your time and effort in helping us with our research, and we hope you have enjoyed taking part.</p>" +
        "<p> For the last part of the study, we will ask you to complete a few more questionnaires, and a final task.</p>" +
        "<p>Please press <b>continue</b> to proceed.</p><br>"
    ,   
    choices: ['continue'],
    button_html: '<button class="jspsych-btn">%choice%</button>',
    is_html: true,
    on_start: function () {
        //jsPsych.setProgressBar(0);
        document.body.style.background = "aliceblue";
    },
    on_finish: function () {
        var startTime = performance.now(); // jsPsych.getStartTime();
        saveStartData(startTime);
    }
};

// text for proceeding to the choice task after the questionnaires
var proceedToChoiceText = {
    type: jsPsychHtmlButtonResponseCA,
    stimulus: "<p><b>Thank you for completing the questionnaires!</b></p>" +
        "<p>Now we will ask you to complete a final task, which is similar to the task you completed in the first part of this study, and will take around 10 minutes.</p>" +
        "<p>As before, you will be presented with a series of scenarios, and you will have to make choices about them.</p>" +
        "<p>Press <b>continue</b> to start the task.</p>",
    choices: ['continue'],
    button_html: '<button class="jspsych-btn">%choice%</button>'
};

///////////////////////////////////////////// SCALES /////////////////////////////////////////////////////////

/// DAS-SF
// define response labels
var respOptsDAS = ["Totally Agree", "Agree", "Disagree", "Totally Disagree"];

// Instruction screen for DAS
var DAS_instructions = {
    type: jsPsychHtmlButtonResponseCA,
    stimulus: "<p>The following sentences describe people’s attitudes.</p>" +
        "<p style='margin-bottom:2em'>Please select how much each sentence describes your attitude. Your answer should describe the way you think <b>most of the time</b>.</p>",
    choices: ['continue'],
    button_html: '<button class="jspsych-btn">%choice%</button>',
    is_html: true
};

// scale items
var DAS = {
    timeline: [
        {
            type: jsPsychSurveyLikert,
            questions: () => { return ([jsPsych.evaluateTimelineVariable('question')]) },
            data: function () {
                return {
                    questionnaire: 'DAS',
                    question_name: jsPsych.evaluateTimelineVariable('question').name,
                    // catch_question flag for DAS (catch_1)
                    catch_question: jsPsych.evaluateTimelineVariable('question').name === 'catch_1'
                };
            },
            scale_width: scaleDisplayWidth,
            button_label: 'next',
        }
    ],
    timeline_variables: [
        {
            question: {
                prompt: "<b>If I don’t set the highest standards for myself, I am likely to end up a second-rate person</b>",
                labels: respOptsDAS, name: "DAS_1", required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>My value as a person depends greatly on what others think of me</b>",
                labels: respOptsDAS, name: "DAS_2", required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>People will probably think less of me if I make a mistake</b>",
                labels: respOptsDAS, name: "DAS_3", required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>I am nothing if a person I love doesn’t love me</b>",
                labels: respOptsDAS, name: "DAS_4", required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>I think it should be against the law to listen to music.</b>",
                labels: respOptsDAS, name: "catch_1", required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>If other people know what you are really like, they will think less of you</b>",
                labels: respOptsDAS, name: "DAS_5", required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>If I fail at my work, then I am a failure as a person</b>",
                labels: respOptsDAS, name: "DAS_6", required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>My happiness depends more on other people than it does me</b>",
                labels: respOptsDAS, name: "DAS_7", required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>I cannot be happy unless most people I know admire me.</b>",
                labels: respOptsDAS, name: "DAS_8", required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>It is best to give up your own interests in order to please other people.</b>",
                labels: respOptsDAS, name: "DAS_9", required: true, horizontal: true
            }
        }
    ],
    preamble: "Most of the time I think that...",
    on_timeline_finish: function () {
        const non_catch_trials = jsPsych.data.get().filter({ questionnaire: 'DAS', catch_question: false });
        const catch_trial = jsPsych.data.get().filter({ questionnaire: 'DAS', question_name: 'catch_1' });

        let responses = {};
        let rts = {};
        let total_score = 0;

        non_catch_trials.trials.forEach(trial => {
            if (trial.response) {
                for (const key in trial.response) {
                    responses[key] = trial.response[key];
                    rts[key] = trial.rt;
                    total_score += trial.response[key];
                }
            }
        });

        let catch_trial_response = null;
        let catch_trial_rt = null;
        if (catch_trial.count() > 0 && catch_trial.trials[0].response) {
            catch_trial_response = catch_trial.trials[0].response['catch_1'];
            catch_trial_rt = catch_trial.trials[0].rt;
        }
        updateEligibility('das_catch_1', catch_trial_response, 'Failed DAS catch question (catch_1).');

        // responses.total_score = total_score; // Optionally add total score to saved data
        saveQuestData("DAS", responses, rts);
        saveQuestData("catch_1", { catch_1_das: catch_trial_response }, { catch_1_das_rt: catch_trial_rt });
    }
};
//////////////////////////// PHQs ///////////////////////
// define response labels
var respOptsPHQ9 = ["Not at all", "Several days", "More than half the days", "Nearly every day"];
var respOptsPHQ9Difficulty = ["Not difficult at all", "Somewhat difficult", "Very difficult", "Extremely difficult"];

// Instruction screen for PHQ9
var PHQ9_instructions = {
    type: jsPsychHtmlButtonResponseCA,
    stimulus: "<p style='margin-bottom:2em'>For the following statements, please choose the option that best describes <em>how often</em> you have been bothered by any of the following problems <b>over the past week</b>.</p>",
    choices: ['continue'],
    button_html: '<button class="jspsych-btn">%choice%</button>',
    is_html: true
};

// PHQ9 scale items
var PHQ9 = {
    timeline: [
        {
            type: jsPsychSurveyLikert,
            questions: () => { return ([jsPsych.evaluateTimelineVariable('question')]) },
            data: function () {
                return {
                    questionnaire: 'PHQ9',
                    question_name: jsPsych.evaluateTimelineVariable('question').name
                    // No catch question in PHQ9 based on current requirements
                };
            },
            scale_width: scaleDisplayWidth,
            button_label: 'next'
            // No on_finish here, moved to on_timeline_finish
        }
    ],
    timeline_variables: [
        {
            question: {
                prompt: "<b>Little interest or pleasure in doing things</b>",
                name: "PHQ9_1", labels: respOptsPHQ9, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>Feeling down, depressed, or hopeless</b>",
                name: "PHQ9_2", labels: respOptsPHQ9, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>Trouble falling/staying asleep, sleeping too much</b>",
                name: "PHQ9_3", labels: respOptsPHQ9, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>Feeling tired or having little energy</b>",
                name: "PHQ9_4", labels: respOptsPHQ9, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>Poor appetite or overeating</b>",
                name: "PHQ9_5", labels: respOptsPHQ9, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>Feeling bad about yourself or that you are a failure or have let yourself or your family down</b>",
                name: "PHQ9_6", labels: respOptsPHQ9, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>Trouble concentrating on things, such as reading the newspaper or watching television.</b>",
                name: "PHQ9_7", labels: respOptsPHQ9, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>Moving or speaking so slowly that other people could have noticed.\n" +
                    "Or the opposite; being so fidgety or restless that you have been moving around a lot more than usual.</b>",
                name: "PHQ9_8", labels: respOptsPHQ9, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>Thoughts that you would be better off dead or of hurting yourself in some way.</b>",
                name: "PHQ9_9", labels: respOptsPHQ9, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "If you have been bothered by any of these problems, how difficult have they " +
                    "made it for you to do your work, take care of things at home, or get along with other people?",
                name: "PHQ9_D", labels: respOptsPHQ9Difficulty, required: true, horizontal: true
            }
        }
    ],
    // preamble: "Over the <b>last week</b>, how often have you been bothered by...", // Removed
    on_timeline_finish: function () { // Changed from on_finish
        const relevant_trials = jsPsych.data.get().filter({ questionnaire: 'PHQ9' });
        let responses = {};
        let rts = {};
        let total_score = 0;
        let phq2_total = 0; // Total score for PHQ-2 (first two items)
        let phq9_item9_response = null;

        relevant_trials.trials.forEach(trial => {
            if (trial.response) {
                for (const key in trial.response) {
                    responses[key] = trial.response[key];
                    rts[key] = trial.rt;
                    if (key !== 'PHQ9_D') { // Exclude PHQ9_D from total score
                        total_score += trial.response[key];
                    }
                    if (key === 'PHQ9_9') {
                        phq9_item9_response = trial.response[key];
                    }
                    if (key === 'PHQ9_1' || key === 'PHQ9_2') { // Calculate PHQ-2 score
                        phq2_total += trial.response[key];
                    }
                }
            }
        });
        let phq9_reason = [];
        if (total_score < 5) {
            phq9_reason.push('PHQ-9 score below 5.');
        } else if (total_score > 14) {
            phq9_reason.push('PHQ-9 score above 14.');
        } else if (phq2_total < 2) {
            phq9_reason.push('PHQ-2 score below 2.');
        } else if (total_score >= 10 && total_score <= 14 && phq9_item9_response > 1) {
            phq9_reason.push('PHQ-9 item 9 more than several days.');
        }

        updateEligibility('phq9', { phq9_total: total_score, phq9_item9: phq9_item9_response, phq2_total: phq2_total }, 'PHQ-9 score/item 9 condition not met.');
        // responses.total_score = total_score; // Optionally add to saved data
        saveQuestData("PHQ9", responses, rts);
    }
};


////////////////////MINI-SPIN//////////////////////////////
// define response labels
var respOptsMiniSPIN = ["Not at all", "A little bit", "Somewhat", "Very much", "Extremely"];

var miniSPIN_instructions = {
    type: jsPsychHtmlButtonResponseCA,
    stimulus: "<p style='margin-bottom:2em'>For the following statements, please choose the option that <em>best describes you.</em></p>",
    choices: ['continue'],
    button_html: '<button class="jspsych-btn">%choice%</button>',
    is_html: true
};

// scale items
var miniSPIN = {
    timeline: [
        {
            type: jsPsychSurveyLikert,
            questions: () => { return ([jsPsych.evaluateTimelineVariable('question')]) },
            data: function () {
                return {
                    questionnaire: 'miniSPIN',
                    question_name: jsPsych.evaluateTimelineVariable('question').name
                };
            },
            scale_width: scaleDisplayWidth,
            button_label: 'next'
            // No on_finish here
        }
    ],
    timeline_variables: [
        {
            question: {
                prompt: "<b>Fear of embarrassment causes me to avoid doing things or speaking to people.</b>",
                name: "miniSPIN_1", labels: respOptsMiniSPIN, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>I avoid activities in which I am the center of attention.</b>",
                name: "miniSPIN_2", labels: respOptsMiniSPIN, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>Being embarrassed or looking stupid are among my worst fears.</b>",
                name: "miniSPIN_3", labels: respOptsMiniSPIN, required: true, horizontal: true
            }
        }
    ],
    on_timeline_finish: function () { // Changed from on_finish
        const relevant_trials = jsPsych.data.get().filter({ questionnaire: 'miniSPIN' });
        let responses = {};
        let rts = {};
        let total_score = 0;

        relevant_trials.trials.forEach(trial => {
            if (trial.response) {
                for (const key in trial.response) {
                    responses[key] = trial.response[key];
                    rts[key] = trial.rt;
                    total_score += trial.response[key];
                }
            }
        });
        // responses.total_score = total_score; // Optionally add to saved data
        saveQuestData("miniSPIN", responses, rts);
    }
};

///////////////////////////////// DAQ //////////////////////////////////////////
// depression attributions questionnaire
var respOptsDAQ = ["Not at all", "Slightly agree", "Moderately agree", "Strongly agree", "Very strongly agree"];

var DAQ_instructions = {
    type: jsPsychHtmlButtonResponseCA,
    stimulus:
        "<p>You will next be presented with statements dealing with how you generally feel about yourself and things that happen to you.</p>" +
        "<p style='margin-bottom:2em'>Please choose the option that reflects how much you agree with each statement.</p>",
    choices: ['continue'],
    button_html: '<button class="jspsych-btn">%choice%</button>',
    is_html: true
};

// scale items
var DAQ = {
    timeline: [
        {
            type: jsPsychSurveyLikert,
            questions: () => { return ([jsPsych.evaluateTimelineVariable('question')]) },
            data: function () {
                return {
                    questionnaire: 'DAQ',
                    question_name: jsPsych.evaluateTimelineVariable('question').name
                };
            },
            scale_width: scaleDisplayWidth,
            button_label: 'next'
            // No on_finish here
        }
    ],
    timeline_variables: [
        {
            question: {
                prompt: "<b> When bad things happen, I think it is my fault.</b>", name: "DAQ_1", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> I feel helpless when bad things happen.</b>", name: "DAQ_2", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When things do not go well, I get easily discouraged.</b>", name: "DAQ_3", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When things go well, I think it is just due to good luck.</b>", name: "DAQ_4", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When something I do goes wrong, I think it is because I am incapable.</b>", name: "DAQ_5", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When something good happens, I think it will not last long.</b>", name: "DAQ_6", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When something bad happens, I think there is little I can do to make things better.</b>", name: "DAQ_7", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When something good happens to me, I think this was because of other people or the circumstances rather than me.</b>", name: "DAQ_8", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> Bad things always happen to me.</b>", name: "DAQ_9", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When bad things happen, I rely on other people to sort things out.</b>", name: "DAQ_10", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When bad things happen to me, I am sure it will happen again.</b>", name: "DAQ_11", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When bad things happen to me, I think my life will never get better.</b>", name: "DAQ_12", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When something bad happens, I think of the problems this will cause in all areas of my life.</b>", name: "DAQ_13", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> Bad things happen in all areas of my life.</b>", name: "DAQ_14", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When bad things happen to me, I can’t see anything positive in my life.</b>", name: "DAQ_15", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b> When bad things happen, nothing seems to be in place any more.</b>", name: "DAQ_16", labels: respOptsDAQ, required: true, horizontal: true
            }
        },
    ],
    on_timeline_finish: function () { // Changed from on_finish
        const relevant_trials = jsPsych.data.get().filter({ questionnaire: 'DAQ' });
        let responses = {};
        let rts = {};
        let total_score = 0;

        relevant_trials.trials.forEach(trial => {
            if (trial.response) {
                for (const key in trial.response) {
                    responses[key] = trial.response[key];
                    rts[key] = trial.rt;
                    total_score += trial.response[key];
                }
            }
        });
        // responses.total_score = total_score; // Optionally add to saved data
        saveQuestData("DAQ", responses, rts);
    }
};

////////////////////ERQ-CR//////////////////////////////
// define response labels
var respOptsERQCR = ["1 (strongly disagree)", "2", "3", "4 (neutral)", "5", "6", "7 (strongly agree)"];

var ERQCR_instructions = {
    type: jsPsychHtmlButtonResponseCA,
    stimulus:
        "<p>We would now like to ask you some questions about your emotional life, in particular, how you control (that is, regulate and manage) your emotions.</p>" +
        "<p>Although some of the following questions may seem similar to one another, they differ in important ways.</p>" +
        "<p style:'margin-bottom:2rem'>For each statement select how much you agree, thinking in particular about the <b>last week</b>.</p>",
    choices: ['continue'],
    button_html: '<button class="jspsych-btn">%choice%</button>',
    is_html: true
};

// scale items
var ERQCR = {
    timeline: [
        {
            type: jsPsychSurveyLikert,
            questions: () => { return ([jsPsych.evaluateTimelineVariable('question')]) },
            data: function () {
                return {
                    questionnaire: 'ERQCR',
                    question_name: jsPsych.evaluateTimelineVariable('question').name,
                    // catch_question flag for ERQCR (catch_2)
                    catch_question: jsPsych.evaluateTimelineVariable('question').name === 'catch_2'
                };
            },
            // preamble: "In the past week...", // Removed, handled by instruction screen
            scale_width: scaleDisplayWidth,
            button_label: 'next'
            // No on_finish here
        }
    ],
    timeline_variables: [
        {
            question: {
                prompt: "<b>When I want to feel more <i>positive</i> emotion (such as joy or amusement), I change what I’m thinking about.</b>",
                name: "ERQCR_1", labels: respOptsERQCR, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>When I want to feel less <i>negative</i> emotion (such as sadness or anger), I change what I’m thinking about.</b>",
                name: "ERQCR_2", labels: respOptsERQCR, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>When I’m faced with a stressful situation, I make myself think about it in a way that helps me stay calm.</b>",
                name: "ERQCR_3", labels: respOptsERQCR, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>When I want to feel more <i>positive</i> emotion, I change the way I’m thinking about the situation.</b>",
                name: "ERQCR_4", labels: respOptsERQCR, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>I can speak over thirty languages fluently. </b>",
                name: "catch_2", labels: respOptsERQCR, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>I control my emotions by changing the way I think about the situation I’m in.</b>",
                name: "ERQCR_5", labels: respOptsERQCR, required: true, horizontal: true
            }
        },
        {
            question: {
                prompt: "<b>When I want to feel less <i>negative</i> emotion, I change the way I’m thinking about the situation.</b>",
                name: "ERQCR_6", labels: respOptsERQCR, required: true, horizontal: true
            }
        }
    ],
    on_timeline_finish: function () {
        const non_catch_trials = jsPsych.data.get().filter({ questionnaire: 'ERQCR', catch_question: false });
        const catch_trial = jsPsych.data.get().filter({ questionnaire: 'ERQCR', question_name: 'catch_2' });

        let responses = {};
        let rts = {};
        let total_score = 0;

        non_catch_trials.trials.forEach(trial => {
            if (trial.response) {
                for (const key in trial.response) {
                    responses[key] = trial.response[key];
                    rts[key] = trial.rt;
                    total_score += trial.response[key];
                }
            }
        });

        let catch_trial_response = null;
        let catch_trial_rt = null;
        if (catch_trial.count() > 0 && catch_trial.trials[0].response) {
            catch_trial_response = catch_trial.trials[0].response['catch_2'];
            catch_trial_rt = catch_trial.trials[0].rt;
        }
        updateEligibility('erqcr_catch_2', catch_trial_response, 'Failed ERQCR catch question (catch_2).');

        // responses.total_score = total_score; // Optionally add to saved data
        saveQuestData("ERQCR", responses, rts);
        saveQuestData("catch_2", { catch_2_erqcr: catch_trial_response }, { catch_2_erqcr_rt: catch_trial_rt });
    }
};

var demogs = {
    type: jsPsychSurvey,
    survey_json: {
        title: "About You",
        showQuestionNumbers: "off",
        description: "Lastly, we would like you to answer the following questions about yourself and your personal circumstancess.",
        pages: [
            {
                name: "demographics_page_1",
                elements: [
                    {
                        type: "text",
                        name: "demogs_age",
                        title: "How old are you (in years)?",
                        inputType: "number",
                        widthMin: "auto",
                        widthMax: "auto",
                        min: 18,
                        max: 100,
                        isRequired: true
                    },
                    {
                        type: "dropdown",
                        name: "demogs_gender",
                        title: "What is your gender identity?",
                        isRequired: true,
                        choices: [
                            { value: "man", text: "Man" },
                            { value: "woman", text: "Woman" },
                            { value: "non-binary", text: "Non-binary" },
                            { value: "other", text: "Other" },
                            { value: "prefer_not_to_say", text: "Prefer not to say" }
                        ]
                    },
                    {
                        type: "dropdown",
                        name: "demogs_ethnicity",
                        title: "What is your ethnic group?",
                        isRequired: true,
                        choices: [
                            { value: "white_british", text: "White" },
                            { value: "mixed", text: "Mixed or multiple ethnic groups" },
                            { value: "asian", text: "Asian or Asian British" },
                            { value: "black", text: "Black, African, Caribbean or Black British" },
                            { value: "other_ethnicity", text: "Other ethnic group" }
                        ]
                    },
                    {
                        type: "dropdown",
                        name: "demogs_employment",
                        title: "Which of the options below best describes your current employment status?",
                        isRequired: true,
                        choices: [
                            { value: "employed", text: "Employed (including full-time and part-time employment)" },
                            { value: "unemployed", text: "Unemployed (job seekers and those unemployed owing to ill health)" },
                            { value: "not_seeking", text: "Not seeking employment (stay-at-home parents, students, and retirees)" }
                        ]
                    },
                    {
                        type: "dropdown",
                        name: "demogs_financial",
                        title: "Which of the options below best describes your current financial situation?",
                        isRequired: true,
                        choices: [
                            { value: "doing_okay", text: "Doing okay financially" },
                            { value: "getting_by", text: "Just about getting by" },
                            { value: "struggling", text: "Struggling financially" }
                        ]
                    },
                    {
                        type: "dropdown",
                        name: "demogs_housing",
                        title: "Which of the options below best describes your current housing situation?",
                        isRequired: true,
                        choices: [
                            { value: "homeowner", text: "Homeowner (including those with a mortgage)" },
                            { value: "tenant", text: "Tenant" },
                            { value: "other_housing", text: "Other (living with family or friends, homeless, or living in a hostel)" }
                        ]
                    },
                    {
                        type: "checkbox", // Changed from multi-select to checkbox for jsPsychSurvey
                        name: "demogs_tx_current", // Changed name to be unique
                        title: "Are you CURRENTLY receiving treatment for a mental health problem? Please select all that apply.",
                        isRequired: true,
                        choices: [
                            { value: "talk_therapy", text: "Yes - talking therapy (including cognitive-behavioural therapies)" },
                            { value: "medication", text: "Yes - medication" },
                            { value: "self_guided", text: "Yes - self-guided (e.g., workbooks or apps)" },
                            { value: "other_tx", text: "Yes - other" },
                            { value: "prefer_not_to_say_tx_current", text: "Prefer not to say" }
                        ],
                        showNoneItem: true, // Allows a "None of the above" option if appropriate, or handle exclusivity in logic
                        noneText: "Not currently receiving treatment for a mental health problem"
                    },
                    {
                        type: "checkbox", // Changed from multi-select to checkbox for jsPsychSurvey
                        name: "demogs_tx_previous", // Changed name to be unique
                        title: "Have you ever PREVIOUSLY received treatment for a mental health problem? Please select all that apply.",
                        isRequired: true,
                        choices: [
                            { value: "talk_therapy_prev", text: "Yes - talking therapy (including cognitive-behavioural therapies)" },
                            { value: "medication_prev", text: "Yes - medication" },
                            { value: "self_guided_prev", text: "Yes - self-guided (e.g., workbooks or apps)" },
                            { value: "other_tx_prev", text: "Yes - other" },
                            { value: "prefer_not_to_say_tx_prev", text: "Prefer not to say" }
                        ],
                        showNoneItem: true,
                        noneText: "Never received treatment for a mental health problem"
                    },
                    {
                        type: "dropdown",
                        name: "demogs_neurodiv",
                        title: "Do you consider yourself to be neurodivergent?\n(Neurodivergence is a term for when someone processes or learns information in a different way to that which is considered 'typical': common examples include autism and ADHD.)",
                        isRequired: true,
                        choices: [
                            { value: "yes", text: "Yes" },
                            { value: "no", text: "No" },
                            { value: "prefer_not_to_say_neurodiv", text: "Prefer not to say" }
                        ]
                    },
                    {
                        type: "checkbox", // Changed from multi-select to checkbox for jsPsychSurvey
                        name: "demogs_disability",
                        title: "Do you consider yourself to have a disability or form of neurodivergence that affects your ability to do any of the below? Please select all that apply.",
                        isRequired: true,
                        choices: [
                            { value: "concentration", text: "Concentrate for extended periods of time" },
                            { value: "physical_effort", text: "Perform physically effortful activites" },
                            { value: "reading_writing_maths", text: "Read, write, or do maths" },
                            { value: "social_interaction", text: "Deal with people you do not know" },
                            { value: "other_impact", text: "Other form of impact not listed above" },
                            { value: "prefer_not_to_say_disability", text: "Prefer not to say" }
                        ],
                        hasNone: true,
                        noneText: "None of the above"
                    }
                ]
            }
        ]
    },
    on_finish: function (data) {
        var respData = data.response;
        // RT for the whole survey page. If multiple pages, this might be just for the last page.
        // For a single page survey, getLastTrialData().trials[0].rt is fine.
        var respRT = jsPsych.data.getLastTrialData().trials[0].rt;

        // Eligibility checks from demographics
        if (respData.demogs_age !== undefined) {
            updateEligibility('age', parseInt(respData.demogs_age), 'Age outside 18-65 range.');
        } else {
            updateEligibility('age', null, 'Age not provided.'); // Should be caught by isRequired
        }
        saveQuestData("demogs", respData, respRT);

        const allChecked = Object.values(studyEligibility.all_criteria_checked).every(status => status);
        if (!allChecked) {
            studyEligibility.reasonIneligible.push("Not all eligibility criteria were checked during the study flow.");
            studyEligibility.isEligible = false; // Mark as ineligible if checks are incomplete
        }
    }
};

var study_qs = {
    type: jsPsychSurvey,
    survey_json: {
        title: "About this study",
        showQuestionNumbers: "off",
        description: "Lastly, we would like you to answer the following questions about this study and (optionally) your recent personal circumstances.",
        pages: [
            {
                name: "study_questions_page_1",
                elements: [
                    {
                        type: "dropdown",
                        name: "study_acceptability",
                        title: "In the future, would you be willing to play more games like the ones in this study, if you thought they could be used to give you information about your thought processes or decision-making?",
                        isRequired: true,
                        choices: [
                            { value: "yes", text: "Yes" },
                            { value: "no", text: "No" },
                            { value: "not_sure", text: "Not sure" }
                        ]
                    },
                    {
                        type: "comment", // Changed from text to comment for multi-line input
                        name: "life_events",
                        title: "[Optional] Over the last two weeks, did you experience any life events which you think have significantly affected your answers to the questions about your mood and feelings? For example, a change in your housing or employment status, illness, or any other event? If you want to, you can tell us about this below.",
                        rows: 2
                    },
                    {
                        type: "comment", // Changed from text to comment for multi-line input
                        name: "study_feedback",
                        title: "[Optional] Is there any feedback you would like to give us about any aspect of the study (including the tasks and questionnaires)?",
                        rows: 2
                    }
                ]
            }
        ]
    },
    on_finish: function (data) {
        var respData = data.response;
        // RT for the whole survey page. If multiple pages, this might be just for the last page.
        // For a single page survey, getLastTrialData().trials[0].rt is fine.
        var respRT = jsPsych.data.getLastTrialData().trials[0].rt;
        saveQuestData("study_qs", respData, respRT);
    }
};

// Prescreener extras  ---------------------------------------------------------------------------------------------
var eligibilityChoice = {
    type: jsPsychHtmlButtonResponseCA,
    stimulus: (
        "<p><h2>Thank you for completing the questionnaires!</h2></p>" +
        "<p>You are eligible to participate in the longer study, which will take place over the next two weeks.</p>" +
        "<p>" +
        "If you would like to participate, and would be willing to complete some further tasks over the coming weeks, we would like to invite you to play a short additional game. " +
        "This game will take around 10 minutes to complete, and will involve making some choices about everyday scenarios." +
        "</p>" +
        "<p> To compensate you for the extra time you will spend on this task, you will receive an additional bonus of <b>£1.50</b> on top of your base payment.</p>" +
        "<p> If you would like to continue, please choose <b>Continue to final task</b>.</p>" +
        "<p> Alternatively, if you do not wish to complete the final task and take part in the longer study, you can choose <b>Leave study</b>.</p><br><br>"
    ),
    choices: ['Leave study', 'Continue to final task'],
    button_html: '<button class="jspsych-btn">%choice%</button>',
    is_html: true,
    button_layout: 'grid',
    on_finish: (data) => {
        switch (data.response) {
            case 1: // If they chose to continue
                studyEligibility.isEligible = true; // Mark as eligible if they choose to continue
                saveEligData(studyEligibility.isEligible, studyEligibility.reasonIneligible);
                break;
            case 0: // If they chose to leave
                studyEligibility.isEligible = false; // Mark as ineligible if they choose to leave
                studyEligibility.reasonIneligible.push("Eligible but chose not to proceed.");
                saveEligData(studyEligibility.isEligible, studyEligibility.reasonIneligible);
                jsPsych.abortCurrentTimeline(); // End the current timeline and so skip to the end screen
                break;
        }
    }
};

// If meet eligibility criteria, give option to continue to the choice task --------------------------------------------
var taskRandomize = {
    type: jsPsychHtmlButtonResponseCA,
    stimulus: (
        "<p><h2> Thank you for completing the final task, and for agreeing to take part in the longer study!</h2></p><br>" +
        "<p> Over the next two weeks, starting tomorrow, we will invite you to complete a task every two days through Prolific.</p>" +
        "<p>Each study will be open for 48 hours for you to complete whenever is convenient. " +
        "Each task will take around 6 minutes to complete, and you will be paid <b>£1</b> to complete each task, for a total of <b>£6</b> for all 6 tasks.</p>" +
        "<p> To encourage your participation, you will receive a bonus of <b>£1</b> for completing 2+ tasks in total, " + 
        "another <b>£2</b> if you complete 4+ tasks, and another <b>£3</b> if you complete all 6 tasks.</p>" +
        "<p> After two weeks, you will be invited to complete a final set of questionnaires and tasks similar to the ones you have just completed.</p>" +
        "<p> We hope you enjoy the tasks, and thank you again for your participation!</p>" +
        "<p> On the next page, you will be able to return to Prolific. Please press <b>continue</b> to proceed.</p><br>"
    ),
    choices: ['continue'],
    button_html: '<button class="jspsych-btn">%choice%</button>',
    is_html: true,
    on_finish: function () {
        // randomise the participants to one of the two conditions, and save the data
        var condition = Math.random() < 0.5 ? "control" : "causal";
        jsPsych.data.addProperties({ condition: condition });
        // save the group assignment to Firebase
        saveCondData(condition);
    }
};
var extraTaskTimeline  = {
    timeline: [eligibilityChoice, timeline_choice_1, taskRandomize],
    conditional_function: function () {
        // Push eligibility data to Firebase
        // Check if the participant is eligible based on the studyEligibility object
        if (studyEligibility.isEligible === true) {
            return true; // Proceed to option to continue to the choice task
        } else {
            // If not eligible, skip the end screen
            saveEligData(studyEligibility.isEligible, studyEligibility.reasonIneligible);
            return false; // Do not proceed to the choice task
        }
    },
};

var questsEndScreen = {
    type: jsPsychHtmlButtonResponseCA,
    timing_post_trial: 0,
    choices: ['Return to Prolific'],
    is_html: true,
    stimulus: (
        "<p>" +
        "<h2>Thank you very much for your time!</h2>" +
        "</p>" +
        "<br>" +
        // "<p>" +
        // "If you would like to find out more about the ideas behind this study, " +
        // "please see <a href=\"https://www.psychologytools.com/self-help/thoughts-in-cbt/\" target=\"_blank\">this article</a> " +
        // "about why some psychologists believe the way we <i>interpret</i> events is key to understanding our feelings about them, " +
        // "and <a href=\"https://www.psychologytools.com/self-help/behavioral-activation/\" target=\"_blank\">this article</a> about why some psychologist " +
        // "think that changing the way we <i>act</i> can help improve our mood. " +
        // "</p>" +
        "<p>" +
        "If you became upset at any point during the study, or are concerned about your mental health for any other reason, "+
        "we recommend the below resources for further information. You may also wish to discuss any concerns with your family doctor." +
        "</p>  " +
        "<ul>  " +
        "<p><a href=\"http://mind.org.uk\" target=\"_blank\">Mind Charity</a></p>" +
        "<p><a href=\"https://www.samaritans.org\" target=\"_blank\">The Samaritans</a></p>" +
        "<p><a href=\"https://www.nhs.uk/mental-health\" target=\"_blank\">NHS Choices mental health page</a></p>" +
        "</ul>" +
        "<p>" +
        "<b>Please click the button below to submit your data back to Prolific!</b>" +
        // ! use target=blank to ensure links open in new window, and don't mess with prolific submission
        "</p><br>"
    )
};

var studyEndScreen = {
    type: jsPsychHtmlButtonResponseCA,
    timing_post_trial: 0,
    choices: ['Return to Prolific'],
    is_html: true,
    stimulus: (
        "<p>" +
        "<h2>Thank you very much for your time over the past two weeks. You have now completed the study.</h2>" +
        "</p><br>" +
        "If you would like to find out more about the ideas behind this study, " +
        "please see <a href=\"https://www.psychologytools.com/self-help/thoughts-in-cbt/\" target=\"_blank\">this article</a> " +
        "about why some psychologists believe the way we <i>interpret</i> events is key to understanding our feelings about them, " +
        "and <a href=\"https://www.psychologytools.com/self-help/behavioral-activation/\" target=\"_blank\">this article</a> about why some psychologist " +
        "think that changing the way we <i>act</i> can help improve our mood. " +
        "</p>" +
        "<p>" +
        "If you became upset at any point during the study, " +
        "or are concerned about your mental health for any other reason, we recommend the below resources for further " +
        "information. You may also wish to discuss any concerns with your family doctor." +
        "</p>  " +
        "<ul>  " +
        "<p><a href=\"http://mind.org.uk\" target=\"_blank\">Mind Charity</a></p>" +
        "<p><a href=\"https://www.samaritans.org\" target=\"_blank\">The Samaritans</a></p>" +
        "<p><a href=\"https://www.nhs.uk/mental-health\" target=\"_blank\">NHS Choices mental health page</a></p>" +
        "</ul>" +
        "</p>" +
        "<b>Please click the button below to submit your data back to Prolific!</b>" +
        // ! use target=blank to ensure links open in new window, and don't mess with prolific submission
        "<br><br></p>"
    )
};

// Construct timeline ----------------------------------------------------------------------------------------------------

var timeline_quests = [];
var timeline_quests_prescreen = [];
var timeline_quests_final = [];

timeline_quests.push(DAS_instructions, DAS);
timeline_quests.push(PHQ9_instructions, PHQ9);
timeline_quests.push(miniSPIN_instructions, miniSPIN);
timeline_quests.push(DAQ_instructions, DAQ);
timeline_quests.push(ERQCR_instructions, ERQCR);

timeline_quests_prescreen.push(questsIntroText, timeline_quests, demogs, extraTaskTimeline, questsEndScreen);
timeline_quests_final.push(questsFinalText, timeline_quests, proceedToChoiceText, timeline_choice_2, study_qs, studyEndScreen);

export { timeline_quests_prescreen, timeline_quests_final };